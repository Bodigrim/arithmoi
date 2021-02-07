-- |
-- Module:      Math.NumberTheory.Primes.Factorisation.Montgomery
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
--
-- Factorisation of 'Integer's by the elliptic curve algorithm after Montgomery.
-- The algorithm is explained at
-- <http://programmingpraxis.com/2010/04/23/modern-elliptic-curve-factorization-part-1/>
-- and
-- <http://programmingpraxis.com/2010/04/27/modern-elliptic-curve-factorization-part-2/>
--

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Primes.Factorisation.Montgomery
  ( -- *  Complete factorisation functions
    -- ** Functions with input checking
    factorise
  --   -- * Partial factorisation
  , smallFactors
  --   -- ** Single curve worker
  , montgomeryFactorisation
  , findParms
  ) where

import Control.Arrow
import Control.Monad.Trans.State.Lazy
import Data.Bits
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (foldl')
import Data.Maybe
import Data.Mod
import Data.Proxy
#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup
#endif
import Data.Traversable
import GHC.Exts
import GHC.Integer.GMP.Internals hiding (integerToInt, wordToInteger)
import GHC.Natural
import GHC.TypeNats (KnownNat, SomeNat(..), natVal, someNatVal)
import System.Random

import Math.NumberTheory.Curves.Montgomery
import Math.NumberTheory.Euclidean.Coprimes (splitIntoCoprimes, unCoprimes)
import Math.NumberTheory.Logarithms (integerLogBase')
import Math.NumberTheory.Primes.Sieve.Atkin
import Math.NumberTheory.Primes.Small
import Math.NumberTheory.Primes.Testing.Probabilistic
import Math.NumberTheory.Roots
import Math.NumberTheory.Utils (shiftToOddCount#, shiftToOddCountBigNat)
import Math.NumberTheory.Utils.FromIntegral

-- | @'factorise' n@ produces the prime factorisation of @n@. @'factorise' 0@ is
--   an error and the factorisation of @1@ is empty. Uses a 'StdGen' produced in
--   an arbitrary manner from the bit-pattern of @n@.
--
-- __Warning:__ there are no guarantees of any particular
-- order of prime factors, do not expect them to be ascending.
factorise :: Integral a => a -> [(a, Word)]
factorise 0 = error "0 has no prime factorisation"
factorise n' = map (first fromIntegral) sfs <> map (first fromInteger) rest
  where
    n = abs n'
    (sfs, mb) = smallFactors (fromIntegral n)
    sg = mkStdGen (fromIntegral n `xor` 0xdeadbeef)
    rest = case mb of
      Nothing -> []
      Just m  -> stdGenFactorisation (Just $ 65536 * 65536) sg Nothing (toInteger m)

----------------------------------------------------------------------------------------------------
--                                    Factorisation wrappers                                      --
----------------------------------------------------------------------------------------------------

-- | A wrapper around 'curveFactorisation' providing a few default arguments.
--   The primality test is 'bailliePSW', the @prng@ function - naturally -
--   'randomR'. This function also requires small prime factors to have been
--   stripped before.
stdGenFactorisation :: Maybe Integer     -- ^ Lower bound for composite divisors
                    -> StdGen            -- ^ Standard PRNG
                    -> Maybe Int         -- ^ Estimated number of digits of smallest prime factor
                    -> Integer           -- ^ The number to factorise
                    -> [(Integer, Word)] -- ^ List of prime factors and exponents
stdGenFactorisation primeBound =
  curveFactorisation primeBound bailliePSW (\m -> randomR (6, m - 2))

-- | 'curveFactorisation' is the driver for the factorisation. Its performance (and success)
--   can be influenced by passing appropriate arguments. If you know that @n@ has no prime divisors
--   below @b@, any divisor found less than @b*b@ must be prime, thus giving @Just (b*b)@ as the
--   first argument allows skipping the comparatively expensive primality test for those.
--   If @n@ is such that all prime divisors must have a specific easy to test for structure, a
--   custom primality test can improve the performance (normally, it will make very little
--   difference, since @n@ has not many divisors, and many curves have to be tried to find one).
--   More influence has the pseudo random generator (a function @prng@ with @6 <= fst (prng k s) <= k-2@
--   and an initial state for the PRNG) used to generate the curves to try. A lucky choice here can
--   make a huge difference. So, if the default takes too long, try another one; or you can improve your
--   chances for a quick result by running several instances in parallel.
--
--   'curveFactorisation' @n@ requires that small (< 65536) prime factors of @n@
--   have been stripped before. Otherwise it is likely to cycle forever.
--
--   'curveFactorisation' is unlikely to succeed if @n@ has more than one (really) large prime factor.
--
curveFactorisation
  :: forall g.
     Maybe Integer                  -- ^ Lower bound for composite divisors
  -> (Integer -> Bool)              -- ^ A primality test
  -> (Integer -> g -> (Integer, g)) -- ^ A PRNG
  -> g                              -- ^ Initial PRNG state
  -> Maybe Int                      -- ^ Estimated number of digits of the smallest prime factor
  -> Integer                        -- ^ The number to factorise
  -> [(Integer, Word)]              -- ^ List of prime factors and exponents
curveFactorisation primeBound primeTest prng seed mbdigs n
    | n == 1    = []
    | ptest n   = [(n, 1)]
    | otherwise = evalState (fact n digits) seed
      where
        digits :: Int
        digits = fromMaybe 8 mbdigs

        ptest :: Integer -> Bool
        ptest = maybe primeTest (\bd k -> k <= bd || primeTest k) primeBound

        rndR :: Integer -> State g Integer
        rndR k = state (prng k)

        perfPw :: Integer -> (Integer, Word)
        perfPw = maybe highestPower (largePFPower . integerSquareRoot) primeBound

        fact :: Integer -> Int -> State g [(Integer, Word)]
        fact 1 _ = return mempty
        fact m digs = do
          let (b1, b1Sieve, b2, ct) = findParms digs
          -- All factors (both @pfs@ and @cfs@), are pairwise coprime. This is
          -- because 'repFact' returns either a single factor, or output of 'workFact'.
          -- In its turn, 'workFact' returns either a single factor,
          -- or concats 'repFact's over coprime integers. Induction completes the proof.
          Factors pfs cfs <- repFact m b1 b1Sieve b2 ct
          case cfs of
            [] -> return pfs
            _  -> do
              nfs <- forM cfs $ \(k, j) ->
                  map (second (* j)) <$> fact k (if null pfs then digs + 5 else digs)
              return $ mconcat (pfs : nfs)

        repFact :: Integer -> Word -> PrimeSieve -> Word -> Word -> State g Factors
        repFact 1 _ _ _ _ = return mempty
        repFact m b1 b1Sieve b2 count =
          case perfPw m of
            (_, 1) -> workFact m b1 b1Sieve b2 count
            (b, e)
              | ptest b   -> return $ singlePrimeFactor b e
              | otherwise -> modifyPowers (* e) <$> workFact b b1 b1Sieve b2 count

        workFact :: Integer -> Word -> PrimeSieve -> Word -> Word -> State g Factors
        workFact 1 _ _ _ _ = return mempty
        workFact m _ _ _ 0 = return $ singleCompositeFactor m 1
        workFact m b1 b1Sieve b2 count = do
          s <- rndR m
          case someNatVal (fromInteger m) of
            SomeNat (_ :: Proxy t) -> case montgomeryFactorisation b1 b1Sieve b2 (fromInteger s :: Mod t) of
              Nothing -> workFact m b1 b1Sieve b2 (count - 1)
              Just d  -> do
                let cs = unCoprimes $ splitIntoCoprimes [(d, 1), (m `quot` d, 1)]
                -- Since all @cs@ are coprime, we can factor each of
                -- them and just concat results, without summing up
                -- powers of the same primes in different elements.
                fmap mconcat $ forM cs $
                  \(x, xm) -> if ptest x
                              then pure $ singlePrimeFactor x xm
                              else repFact x b1 b1Sieve b2 (count - 1)

data Factors = Factors
  { _primeFactors     :: [(Integer, Word)]
  , _compositeFactors :: [(Integer, Word)]
  }

singlePrimeFactor :: Integer -> Word -> Factors
singlePrimeFactor a b = Factors [(a, b)] []

singleCompositeFactor :: Integer -> Word -> Factors
singleCompositeFactor a b = Factors [] [(a, b)]

instance Semigroup Factors where
  Factors pfs1 cfs1 <> Factors pfs2 cfs2
    = Factors (pfs1 <> pfs2) (cfs1 <> cfs2)

instance Monoid Factors where
  mempty = Factors [] []
  mappend = (<>)

modifyPowers :: (Word -> Word) -> Factors -> Factors
modifyPowers f (Factors pfs cfs)
  = Factors (map (second f) pfs) (map (second f) cfs)

-------------------------------------------------------------------------------
-- largePFPower

-- | @'largePFPower' bd n@ produces the pair @(b,k)@ with the largest
--   exponent @k@ such that @n == b^k@, where @bd > 1@ (it is expected
--   that @bd@ is much larger, at least @1000@ or so), @n > bd^2@ and @n@
--   has no prime factors @p <= bd@, skipping the trial division phase
--   of @'highestPower'@ when that is a priori known to be superfluous.
--   It is only present to avoid duplication of work in factorisation
--   and primality testing, it is not expected to be generally useful.
--   The assumptions are not checked, if they are not satisfied, wrong
--   results and wasted work may be the consequence.
largePFPower :: Integer -> Integer -> (Integer, Word)
largePFPower bd n = rawPower ln n
  where
    ln = intToWord (integerLogBase' (bd+1) n)

rawPower :: Word -> Integer -> (Integer, Word)
rawPower mx n = case exactRoot 4 n of
                  Just r -> case rawPower (mx `quot` 4) r of
                              (m,e) -> (m, 4*e)
                  Nothing -> case exactSquareRoot n of
                               Just r -> case rawOddPower (mx `quot` 2) r of
                                           (m,e) -> (m, 2*e)
                               Nothing -> rawOddPower mx n

rawOddPower :: Word -> Integer -> (Integer, Word)
rawOddPower mx n
  | mx < 3       = (n,1)
rawOddPower mx n = case exactCubeRoot n of
                     Just r -> case rawOddPower (mx `quot` 3) r of
                                 (m,e) -> (m, 3*e)
                     Nothing -> badPower mx n

badPower :: Word -> Integer -> (Integer, Word)
badPower mx n
  | mx < 5      = (n,1)
  | otherwise   = go 1 mx n (takeWhile (<= mx) $ scanl (+) 5 $ cycle [2,4])
    where
      go !e b m (k:ks)
        | b < k     = (m,e)
        | otherwise = case exactRoot k m of
                        Just r -> go (e*k) (b `quot` k) r (k:ks)
                        Nothing -> go e b m ks
      go e _ m []   = (m,e)

----------------------------------------------------------------------------------------------------
--                                         The workhorse                                          --
----------------------------------------------------------------------------------------------------

-- | @'montgomeryFactorisation' n b1 b2 s@ tries to find a factor of @n@ using the
--   curve and point determined by the seed @s@ (@6 <= s < n-1@), multiplying the
--   point by the least common multiple of all numbers @<= b1@ and all primes
--   between @b1@ and @b2@. The idea is that there's a good chance that the order
--   of the point in the curve over one prime factor divides the multiplier, but the
--   order over another factor doesn't, if @b1@ and @b2@ are appropriately chosen.
--   If they are too small, none of the orders will probably divide the multiplier,
--   if they are too large, all probably will, so they should be chosen to fit
--   the expected size of the smallest factor.
--
--   It is assumed that @n@ has no small prime factors.
--
--   The result is maybe a nontrivial divisor of @n@.
montgomeryFactorisation :: KnownNat n => Word -> PrimeSieve -> Word -> Mod n -> Maybe Integer
montgomeryFactorisation b1 b1Sieve b2 s = case newPoint (toInteger (unMod s)) n of
  Nothing             -> Nothing
  Just (SomePoint p0) -> do
    -- Small step: for each prime p <= b1
    -- multiply point 'p0' by the highest power p^k <= b1.
    let q = foldl (flip multiply) p0 smallPowers
        z = pointZ q

    case gcd n z of
      -- If small step did not succeed, perform a big step.
      1 -> case gcd n (bigStep q b1 b2) of
        1 -> Nothing
        g -> Just g
      g -> Just g
  where
    n = toInteger (natVal s)
    smallPowers = map (findPower . fromIntegral) (atkinPrimeList b1Sieve)
    findPower p = go p
      where
        go acc
          | acc <= b1 `quot` p = go (acc * p)
          | otherwise          = acc

-- | The implementation follows the algorithm at p. 6-7
-- of <http://www.hyperelliptic.org/tanja/SHARCS/talks06/Gaj.pdf Implementing the Elliptic Curve Method of Factoring in Reconfigurable Hardware>
-- by K. Gaj, S. Kwon et al.
bigStep :: (KnownNat a24, KnownNat n) => Point a24 n -> Word -> Word -> Integer
bigStep q b1 b2 = rs
  where
    n = pointN q

    b0 = b1 - b1 `rem` wheel
    qks = zip [0..] $ map (`multiply` q) wheelCoprimes
    qs = enumAndMultiplyFromThenTo q b0 (b0 + wheel) b2

    rs = foldl' (\ts (_cHi, p) -> foldl' (\us (_cLo, pq) ->
        us * (pointZ p * pointX pq - pointX p * pointZ pq) `rem` n
        ) ts qks) 1 qs

wheel :: Num a => a
wheel = 210

wheelCoprimes :: [Word]
wheelCoprimes = [ k | k <- [1 .. wheel `div` 2], k `gcd` wheel == 1 ]

-- | Same as map (id *** flip multiply p) [from, thn .. to],
-- but calculated in more efficient way.
enumAndMultiplyFromThenTo
  :: (KnownNat a24, KnownNat n)
  => Point a24 n
  -> Word
  -> Word
  -> Word
  -> [(Word, Point a24 n)]
enumAndMultiplyFromThenTo p from thn to = zip [from, thn .. to] progression
  where
    step = thn - from

    pFrom = multiply from p
    pThen = multiply thn  p
    pStep = multiply step p

    progression = pFrom : pThen : zipWith (`add` pStep) progression (tail progression)

-- | @'smallFactors' n@ finds all prime divisors of @n > 1@ up to 2^16 by trial division and returns the
--   list of these together with their multiplicities, and a possible remaining factor which may be composite.
smallFactors :: Natural -> ([(Natural, Word)], Maybe Natural)
smallFactors = \case
  NatS# 0## -> error "0 has no prime factorisation"
  NatS# n#  -> case shiftToOddCount# n# of
    (# 0##, m# #) -> goWord m# 1
    (# k#,  m# #) -> (2, W# k#) <: goWord m# 1
  NatJ# n -> case shiftToOddCountBigNat n of
    (0, m) -> goBigNat m 1
    (k, m) -> (2, k) <: goBigNat m 1
  where
    x <: ~(l,b) = (x:l,b)

    !(Ptr smallPrimesAddr#) = smallPrimesPtr

    goBigNat :: BigNat -> Int -> ([(Natural, Word)], Maybe Natural)
    goBigNat !m i@(I# i#)
      | isTrue# (sizeofBigNat# m ==# 1#)
      = goWord (bigNatToWord m) i
      | i >= smallPrimesLength
      = ([], Just (NatJ# m))
      | otherwise
      = let p# = indexWord16OffAddr# smallPrimesAddr# i# in
      case m `quotRemBigNatWord` p# of
        (# mp, 0## #) ->
          let (# k, r #) = splitOff 1 mp in
            (NatS# p#, k) <: goBigNat r (i + 1)
          where
            splitOff !k x = case x `quotRemBigNatWord` p# of
              (# xp, 0## #) -> splitOff (k + 1) xp
              _             -> (# k, x #)
        _ -> goBigNat m (i + 1)

    goWord :: Word# -> Int -> ([(Natural, Word)], Maybe Natural)
    goWord 1## !_ = ([], Nothing)
    goWord m#  !i
      | i >= smallPrimesLength
      = if isTrue# (m# `leWord#` 4294967295##) -- 65536 * 65536 - 1
        then ([(NatS# m#, 1)], Nothing)
        else ([], Just (NatS# m#))
    goWord m# i@(I# i#) = let p# = indexWord16OffAddr# smallPrimesAddr# i# in
      if isTrue# (m# `ltWord#` (p# `timesWord#` p#))
        then ([(NatS# m#, 1)], Nothing)
        else case m# `quotRemWord#` p# of
          (# mp#, 0## #) ->
            let !(# k#, r# #) = splitOff 1## mp# in
              (NatS# p#, W# k#) <: goWord r# (i + 1)
            where
              splitOff k# x# = case x# `quotRemWord#` p# of
                (# xp#, 0## #) -> splitOff (k# `plusWord#` 1##) xp#
                _              -> (# k#, x# #)
          _ -> goWord m# (i + 1)

-- | For a given estimated decimal length of the smallest prime factor
-- ("tier") return parameters B1, B2 and the number of curves to try
-- before next "tier".
-- Roughly based on http://www.mersennewiki.org/index.php/Elliptic_Curve_Method#Choosing_the_best_parameters_for_ECM
testParms :: IntMap (Word, PrimeSieve, Word, Word)
testParms
  = IM.fromList
  $ map (\(k, (b1, b2, ct)) -> (k, (b1, atkinSieve 0 (fromIntegral b1), b2, ct)))
  [ (12, (       400,        40000,     10))
  , (15, (      2000,       200000,     25))
  , (20, (     11000,      1100000,     90))
  , (25, (     50000,      5000000,    300))
  , (30, (    250000,     25000000,    700))
  , (35, (   1000000,    100000000,   1800))
  , (40, (   3000000,    300000000,   5100))
  , (45, (  11000000,   1100000000,  10600))
  , (50, (  43000000,   4300000000,  19300))
  , (55, ( 110000000,  11000000000,  49000))
  , (60, ( 260000000,  26000000000, 124000))
  , (65, ( 850000000,  85000000000, 210000))
  , (70, (2900000000, 290000000000, 340000))
  ]

findParms :: Int -> (Word, PrimeSieve, Word, Word)
findParms digs
  = maybe (wheel, atkinSieve 0 wheel, 1000, 7) snd
  $ IM.lookupLT digs testParms
