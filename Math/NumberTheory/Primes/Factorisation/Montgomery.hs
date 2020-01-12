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
-- The implementation is not very optimised, so it is not suitable for factorising numbers
-- with only huge prime divisors. However, factors of 20-25 digits are normally found in
-- acceptable time. The time taken depends, however, strongly on how lucky the curve-picking
-- is. With luck, even large factors can be found in seconds; on the other hand, finding small
-- factors (about 10 digits) can take minutes when the curve-picking is bad.
--
-- Given enough time, the algorithm should be able to factor numbers of 100-120 digits, but it
-- is best suited for numbers of up to 50-60 digits.

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import System.Random
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
import qualified Data.Vector.Unboxed as V

import GHC.TypeNats.Compat

import Math.NumberTheory.Curves.Montgomery
import Math.NumberTheory.Euclidean.Coprimes (splitIntoCoprimes, unCoprimes)
import Math.NumberTheory.Roots.General     (highestPower, largePFPower)
import Math.NumberTheory.Roots.Squares     (integerSquareRoot')
import Math.NumberTheory.Primes.Sieve.Eratosthenes (PrimeSieve(..), psieveFrom)
import Math.NumberTheory.Primes.Sieve.Indexing (toPrim)
import Math.NumberTheory.Primes.Small
import Math.NumberTheory.Primes.Testing.Probabilistic
import Math.NumberTheory.Unsafe
import Math.NumberTheory.Utils hiding (splitOff)

-- | @'factorise' n@ produces the prime factorisation of @n@. @'factorise' 0@ is
--   an error and the factorisation of @1@ is empty. Uses a 'StdGen' produced in
--   an arbitrary manner from the bit-pattern of @n@.
--
-- __Warning:__ there are no guarantees of any particular
-- order of prime factors, do not expect them to be ascending. E. g.,
--
-- >>> factorise 10251562501
-- [(101701,1),(100801,1)]
factorise :: Integer -> [(Integer, Word)]
factorise n
    | abs n == 1 = []
    | n < 0      = factorise (-n)
    | n == 0     = error "0 has no prime factorisation"
    | otherwise  = factorise' n

-- | Like 'factorise', but without input checking, hence @n > 1@ is required.
factorise' :: Integer -> [(Integer, Word)]
factorise' n = defaultStdGenFactorisation' (mkStdGen $ fromInteger n `xor` 0xdeadbeef) n

-- | Like 'defaultStdGenFactorisation', but without input checking, so
--   @n@ must be larger than @1@.
defaultStdGenFactorisation' :: StdGen -> Integer -> [(Integer, Word)]
defaultStdGenFactorisation' sg n
    = let (sfs,mb) = smallFactors n
      in sfs ++ case mb of
                  Nothing -> []
                  Just m  -> stdGenFactorisation (Just $ 65536 * 65536) sg Nothing m

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
stdGenFactorisation primeBound sg digits n
    = curveFactorisation primeBound bailliePSW (\m -> randomR (6,m-2)) sg digits n

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
--   have been stripped before. Otherwise it is likely to cycle forever. When in doubt,
--   use 'defaultStdGenFactorisation'.
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
        perfPw = maybe highestPower (largePFPower . integerSquareRoot') primeBound

        fact :: Integer -> Int -> State g [(Integer, Word)]
        fact 1 _ = return mempty
        fact m digs = do
          let (b1, b2, ct) = findParms digs
          -- All factors (both @pfs@ and @cfs@), are pairwise coprime. This is
          -- because 'repFact' returns either a single factor, or output of 'workFact'.
          -- In its turn, 'workFact' returns either a single factor,
          -- or concats 'repFact's over coprime integers. Induction completes the proof.
          Factors pfs cfs <- repFact m b1 b2 ct
          case cfs of
            [] -> return pfs
            _  -> do
              nfs <- forM cfs $ \(k, j) ->
                  map (second (* j)) <$> fact k (if null pfs then digs + 5 else digs)
              return $ mconcat (pfs : nfs)

        repFact :: Integer -> Word -> Word -> Word -> State g Factors
        repFact 1 _ _ _ = return mempty
        repFact m b1 b2 count =
          case perfPw m of
            (_, 1) -> workFact m b1 b2 count
            (b, e)
              | ptest b   -> return $ singlePrimeFactor b e
              | otherwise -> modifyPowers (* e) <$> workFact b b1 b2 count

        workFact :: Integer -> Word -> Word -> Word -> State g Factors
        workFact 1 _ _ _ = return mempty
        workFact m _ _ 0 = return $ singleCompositeFactor m 1
        workFact m b1 b2 count = do
          s <- rndR m
          case someNatVal (fromInteger m) of
            SomeNat (_ :: Proxy t) -> case montgomeryFactorisation b1 b2 (fromInteger s :: Mod t) of
              Nothing -> workFact m b1 b2 (count - 1)
              Just d  -> do
                let cs = unCoprimes $ splitIntoCoprimes [(d, 1), (m `quot` d, 1)]
                -- Since all @cs@ are coprime, we can factor each of
                -- them and just concat results, without summing up
                -- powers of the same primes in different elements.
                fmap mconcat $ flip mapM cs $
                  \(x, xm) -> if ptest x
                              then pure $ singlePrimeFactor x xm
                              else repFact x b1 b2 (count - 1)

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
montgomeryFactorisation :: KnownNat n => Word -> Word -> Mod n -> Maybe Integer
montgomeryFactorisation b1 b2 s = case newPoint (toInteger (unMod s)) n of
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
    smallPowers
      = map findPower
      $ takeWhile (<= b1) (2 : 3 : 5 : list primeStore)
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
    qks = zip [0..] $ map (\k -> multiply k q) wheelCoprimes
    qs = enumAndMultiplyFromThenTo q b0 (b0 + wheel) b2

    rs = foldl' (\ts (_cHi, p) -> foldl' (\us (_cLo, pq) ->
        us * (pointZ p * pointX pq - pointX p * pointZ pq) `rem` n
        ) ts qks) 1 qs

wheel :: Word
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

    progression = pFrom : pThen : zipWith (\x0 x1 -> add x0 pStep x1) progression (tail progression)

-- primes, compactly stored as a bit sieve
primeStore :: [PrimeSieve]
primeStore = psieveFrom 7

-- generate list of primes from arrays
list :: [PrimeSieve] -> [Word]
list sieves = concat [[off + toPrim i | i <- [0 .. li], unsafeAt bs i]
                                | PS vO bs <- sieves, let { (_,li) = bounds bs; off = fromInteger vO; }]

-- | @'smallFactors' n@ finds all prime divisors of @n > 1@ up to 2^16 by trial division and returns the
--   list of these together with their multiplicities, and a possible remaining factor which may be composite.
smallFactors :: Integer -> ([(Integer, Word)], Maybe Integer)
smallFactors 0 = error "0 has no prime factorisation"
smallFactors n = case shiftToOddCount n of
                      (0,m) -> go m 1
                      (k,m) -> (2,k) <: go m 1
  where
    x <: ~(l,b) = (x:l,b)
    smallPrimesLen = V.length smallPrimes

    go :: Integer -> Int -> ([(Integer, Word)], Maybe Integer)
    go 1 _ = ([], Nothing)
    go !m !i
      | i >= smallPrimesLen
      = if m < 65536 * 65536
        then ([(m, 1)], Nothing)
        else ([], Just m)
    go !m !i = let p = toInteger (V.unsafeIndex smallPrimes i) in
      if m < p * p
        then ([(m, 1)], Nothing)
        else case m `quotRem` p of
          (mp, 0) -> let (k, r) = splitOff 1 mp in (p, k) <: go r (i + 1)
            where
              splitOff !k x = case x `quotRem` p of
                (xp, 0) -> splitOff (k + 1) xp
                _       -> (k, x)
          _ -> go m (i + 1)

-- | For a given estimated decimal length of the smallest prime factor
-- ("tier") return parameters B1, B2 and the number of curves to try
-- before next "tier".
-- Roughly based on http://www.mersennewiki.org/index.php/Elliptic_Curve_Method#Choosing_the_best_parameters_for_ECM
testParms :: IntMap (Word, Word, Word)
testParms = IM.fromList
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

findParms :: Int -> (Word, Word, Word)
findParms digs = maybe (wheel, 1000, 7) snd (IM.lookupLT digs testParms)
