-- |
-- Module:      Math.NumberTheory.Primes.Sieve.Misc
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fspec-constr-count=8 #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-# OPTIONS_HADDOCK hide #-}
module Math.NumberTheory.Primes.Sieve.Misc
    ( -- * Types
      FactorSieve
    , TotientSieve
    , CarmichaelSieve
      -- * Functions
      -- ** Smallest prime factors
    , factorSieve
    , sieveFactor
    , fsBound
    , fsPrimeTest
      -- ** Totients
    , totientSieve
    , sieveTotient
      -- ** Carmichael
    , carmichaelSieve
    , sieveCarmichael
    ) where

import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad (when)
import Data.Bits
import GHC.Word

import System.Random

import Math.NumberTheory.Powers.Squares (integerSquareRoot')
import Math.NumberTheory.Primes.Sieve.Indexing
import Math.NumberTheory.Primes.Factorisation.Montgomery
import Math.NumberTheory.Primes.Factorisation.Utils
import Math.NumberTheory.Unsafe
import Math.NumberTheory.Utils

{-
IMPORTANT NOTICE: Not all sieves use the same layout!

FactorSieve:

   To remain as efficient as possible, FactorSieve omits only even numbers.
   To relate an odd number x to its index i:

       i = (x `div` 2) - 1
       x = i * 2 + 3

TotientSieve, CarmichaelSieve:

   These sieves use a (2,3,5) wheel optimization, sacrificing performance to save
   more memory. The only indices stored are those coprime to 2, 3, and 5.
   To relate such an integer x to its index i:

       i = toIdx x
       x = toPrim i
-}

-- | A compact store of smallest prime factors.
data FactorSieve = FS {-# UNPACK #-} !Word {-# UNPACK #-} !(UArray Int Word16)

-- | A compact store of totients.
data TotientSieve = TS {-# UNPACK #-} !Word {-# UNPACK #-} !(UArray Int Word)

-- | A compact store of values of the Carmichael function.
data CarmichaelSieve = CS {-# UNPACK #-} !Word {-# UNPACK #-} !(UArray Int Word)

-- | @'factorSieve' n@ creates a store of smallest prime factors of the numbers not exceeding @n@.
--   If you need to factorise many smallish numbers, this can give a big speedup since it avoids
--   many superfluous divisions. However, a too large sieve leads to a slowdown due to cache misses.
--   The prime factors are stored as 'Word16' for compactness, so @n@ must be
--   smaller than @2^32@.
factorSieve :: Integer -> FactorSieve
factorSieve bound
  | 4294967295 < bound  = error "factorSieve: overflow"
  | bound < 8   = FS 7 (array (0,2) [(0,0),(1,0),(2,0)])
  | otherwise   = FS bnd fSieve
    where
      bnd = fromInteger bound
      ibnd = fromInteger ((bound - 3) `quot` 2)
      svbd = (fromInteger (integerSquareRoot' bound) - 1) `quot` 2
      fSieve = runSTUArray $ do
          sieve <- newArray (0,ibnd) 0 :: ST s (STUArray s Int Word16)
          let sift i
                  | i < svbd = do
                      sp <- unsafeRead sieve i
                      when (sp == 0) (mark (2*i+3) (2*i*(i+3)+3))
                      sift (i+1)
                  | otherwise = return sieve
              mark p j
                  | j > ibnd    = return ()
                  | otherwise   = do
                      sp <- unsafeRead sieve j
                      when (sp == 0) (unsafeWrite sieve j $ fromIntegral p)
                      mark p (j+p)
          sift 0

-- | @'fsBound' sieve@ tells the limit to which the sieve stores the smallest prime factors.
fsBound :: FactorSieve -> Word
fsBound (FS b _) = b

-- | @'fsPrimeTest' sieve n@ checks in the sieve whether @n@ is prime. If @n@ is larger
--   than the sieve can handle, an error is raised.
fsPrimeTest :: FactorSieve -> Integer -> Bool
fsPrimeTest fs@(FS bnd sve) n
    | n < 0     = fsPrimeTest fs (-n)
    | n < 2     = False
    | fromInteger n .&. (1 :: Int) == 0 = n == 2
    | n <= fromIntegral bnd = sve `unsafeAt` (fromInteger (n `shiftR` 1) - 1) == 0
    | otherwise = error "Out of bounds"

-- | @'sieveFactor' fs n@ finds the prime factorisation of @n@ using the 'FactorSieve' @fs@.
--   For negative @n@, a factor of @-1@ is included with multiplicity @1@.
--   After stripping any present factors @2@, the remaining cofactor @c@ (if larger
--   than @1@) is factorised with @fs@. This is most efficient of course if @c@ does not
--   exceed the bound with which @fs@ was constructed. If it does, trial division is performed
--   until either the cofactor falls below the bound or the sieve is exhausted. In the latter
--   case, the elliptic curve method is used to finish the factorisation.
sieveFactor :: FactorSieve -> Integer -> [(Integer,Int)]
sieveFactor (FS bnd sve) = check
  where
    bound = fromIntegral bnd
    check 0 = error "0 has no prime factorisation"
    check 1 = []
    check n
      | n < 0       = (-1,1) : check (-n)
      | n <= bound  = go2w (fromIntegral n)     -- avoid expensive Integer ops if possible
      | fromInteger n .&. (1 :: Int) == 1 = sieveLoop n
      | otherwise   = go2 n
    go2w n
        | n .&. 1 == 1 = intLoop ((n-3) `shiftR` 1)
        | otherwise = case shiftToOddCount n of
                        (k,m) -> (2,k) : if m == 1 then [] else intLoop ((m-3) `shiftR` 1)
    go2 n = case shiftToOddCount n of
              (k,m) -> (2,k) : if m == 1 then [] else sieveLoop m
    sieveLoop n
        | bound < n  = tdLoop n (integerSquareRoot' n) 0
        | otherwise = intLoop (fromIntegral (n `shiftR` 1)-1)
    intLoop :: Word -> [(Integer,Int)]
    intLoop !n = case unsafeAt sve (fromIntegral n) of
                  0 -> [(2*fromIntegral n+3,1)]
                  p -> let p' = fromIntegral p in countLoop p' (n `quot` p' - 1) 1
    countLoop !p !i !c
        = case unsafeAt sve (fromIntegral i) of
            0 | p-3 == 2*i -> [(fromIntegral p,c+1)]
              | otherwise  -> (fromIntegral p,c) : (2*fromIntegral i+3,1) : []
            q | fromIntegral q == p -> countLoop p (i `quot` p - 1) (c+1)
              | otherwise -> (fromIntegral p, c) : intLoop i
    lstIdx = snd (bounds sve)
    tdLoop n sr ix
        | lstIdx < ix   = curve n
        | sr < p        = [(n,1)]
        | pix /= 0      = tdLoop n sr (ix+1)    -- not a prime
        | otherwise     = case splitOff p n of
                            (0,_) -> tdLoop n sr (ix+1)
                            (k,m) -> (p,k) : case m of
                                               1 -> []
                                               j | j <= bound -> intLoop (fromIntegral (j `shiftR` 1) - 1)
                                                 | otherwise -> tdLoop j (integerSquareRoot' j) (ix+1)
          where
            p = fromIntegral $ 2 * ix + 3
            pix = unsafeAt sve ix
    curve n = stdGenFactorisation (Just (bound*(bound+2))) (mkStdGen $ fromIntegral n `xor` 0xdecaf00d) Nothing n

-- | @'totientSieve' n@ creates a store of the totients of the numbers not exceeding @n@.
--   A 'TotientSieve' only stores values for numbers coprime to @30@ to reduce space usage.
--   The maximal admissible value for @n@ is @'fromIntegral' ('maxBound' :: 'Word')@.
totientSieve :: Integer -> TotientSieve
totientSieve bound
  | fromIntegral (maxBound :: Word) < bound  = error "totientSieve: overflow"
  | bound < 8   = TS 7 (array (0,0) [(0,6)])
  | otherwise   = TS bnd (totSieve bnd)
    where
      bnd = fromInteger bound

-- | @'sieveTotient' ts n@ finds the totient @&#960;(n)@, i.e. the number of integers @k@ with
--   @1 <= k <= n@ and @'gcd' n k == 1@, in other words, the order of the group of units
--   in @&#8484;/(n)@, using the 'TotientSieve' @ts@.
--   First, factors of @2, 3@ and @5@ are handled individually, if the remaining
--   cofactor of @n@ is within the sieve range, its totient is looked up, otherwise
--   the calculation falls back on factorisation, first by trial division and
--   if necessary, elliptic curves.
sieveTotient :: TotientSieve -> Integer -> Integer
sieveTotient (TS bnd sve) = check
  where
    bound = fromIntegral bnd
    check n
        | n < 1     = error "Totient only defined for positive numbers"
        | n == 1    = 1
        | otherwise = go2 n
    go2 n = case shiftToOddCount n of
              (0,_) -> go3 1 n
              (k,m) -> let tt = (shiftL 1 (k-1)) in if m == 1 then tt else go3 tt m
    go3 !tt n = case splitOff 3 n of
                  (0,_) -> go5 tt n
                  (k,m) -> let nt = tt*(2*3^(k-1)) in if m == 1 then nt else go5 nt m
    go5 !tt n = case splitOff 5 n of
                  (0,_) -> sieveLoop tt n
                  (k,m) -> let nt = tt*(4*5^(k-1)) in if m == 1 then nt else sieveLoop nt m
    sieveLoop !tt n
        | bound < n = tdLoop tt n (integerSquareRoot' n) 0
        | otherwise = case unsafeAt sve (toIdx n) of
                        nt -> tt*fromIntegral nt
    lstIdx = snd (bounds sve)
    tdLoop !tt n sr ix
        | lstIdx < ix   = curve tt n
        | sr < p'       = tt*(n-1)      -- n is a prime
        | pix /= p-1    = tdLoop tt n sr (ix+1)    -- not a prime, next
        | otherwise     = case splitOff p' n of
                            (0,_) -> tdLoop tt n sr (ix+1)
                            (k,m) -> let nt = tt*ppTotient (p',k)
                                     in case m of
                                          1 -> nt
                                          j | j <= bound -> nt*fromIntegral (unsafeAt sve (toIdx j))
                                            | otherwise  -> tdLoop nt j (integerSquareRoot' j) (ix+1)
          where
            p = toPrim ix
            p' = fromIntegral p
            pix = unsafeAt sve ix
    curve tt n = tt * totientFromCanonical (stdGenFactorisation (Just (bound*(bound+2))) (mkStdGen $ fromIntegral n `xor` 0xdecaf00d) Nothing n)

-- | @'carmichaelSieve' n@ creates a store of values of the Carmichael function
--   for numbers not exceeding @n@.
--   Like a 'TotientSieve', a 'CarmichaelSieve' only stores values for numbers coprime to @30@
--   to reduce space usage. The maximal admissible value for @n@ is @'fromIntegral' ('maxBound' :: 'Word')@.
carmichaelSieve :: Integer -> CarmichaelSieve
carmichaelSieve bound
  | fromIntegral (maxBound :: Word) < bound  = error "carmichaelSieve: overflow"
  | bound < 8   = CS 7 (array (0,0) [(0,6)])
  | otherwise   = CS bnd (carSieve bnd)
    where
      bnd = fromInteger bound

-- | @'sieveCarmichael' cs n@ finds the value of @&#955;(n)@ (or @&#968;(n)@), the smallest positive
--   integer @e@ such that for all @a@ with @gcd a n == 1@ the congruence @a^e &#8801; 1 (mod n)@ holds,
--   in other words, the (smallest) exponent of the group of units in @&#8484;/(n)@.
--   The strategy is analogous to 'sieveTotient'.
sieveCarmichael :: CarmichaelSieve -> Integer -> Integer
sieveCarmichael (CS bnd sve) = check
  where
    bound = fromIntegral bnd
    check n
        | n < 1     = error "Carmichael function only defined for positive numbers"
        | n == 1    = 1
        | otherwise = go2 n
    go2 n = case shiftToOddCount n of
              (0,_) -> go3 1 n
              (k,m) -> let tt = case k of
                                  1 -> 1
                                  2 -> 2
                                  _ -> (shiftL 1 (k-2))
                       in if m == 1 then tt else go3 tt m
    go3 !tt n = case splitOff 3 n of
                  (0,_) -> go5 tt n
                  (k,1) | tt == 1   -> 2*3^(k-1)
                        | otherwise -> tt*3^(k-1)
                  (k,m) | tt == 1   -> go5 (2*3^(k-1)) m
                        | otherwise -> go5 (tt*3^(k-1)) m
    go5 !tt n = case splitOff 5 n of
                  (0,_) -> sieveLoop tt n
                  (k,m) -> let tt' = case fromInteger tt .&. (3 :: Int) of
                                       0 -> tt
                                       2 -> 2*tt
                                       _ -> 4*tt
                               nt = tt'*5^(k-1)
                           in if m == 1 then nt else sieveLoop nt m
    sieveLoop !tt n
        | bound < n = tdLoop tt n (integerSquareRoot' n) 0
        | otherwise = case unsafeAt sve (toIdx n) of
                        nt -> tt `lcm` fromIntegral nt
    lstIdx = snd (bounds sve)
    tdLoop !tt n sr ix
        | lstIdx < ix   = curve tt n
        | sr < p'       = tt `lcm` (n-1)      -- n is a prime
        | pix /= p-1    = tdLoop tt n sr (ix+1)    -- not a prime, next
        | otherwise     = case splitOff p' n of
                            (0,_) -> tdLoop tt n sr (ix+1)
                            (k,m) -> let nt = (lcm tt (p'-1))*p'^(k-1)
                                     in case m of
                                          1 -> nt
                                          j | j <= bound -> nt `lcm` fromIntegral (unsafeAt sve (toIdx j))
                                            | otherwise  -> tdLoop nt j (integerSquareRoot' j) (ix+1)
          where
            p = toPrim ix
            p' = fromIntegral p
            pix = unsafeAt sve ix
    curve tt n = tt `lcm` carmichaelFromCanonical (stdGenFactorisation (Just (bound*(bound+2))) (mkStdGen $ fromIntegral n `xor` 0xdecaf00d) Nothing n)


-- NOTE: This is a legacy implementation of FactorSieve which uses the
--       same (2,3,5) wheel optimization as the other sieves.
--       It is still used to generate the other sieves.
spfSieve :: Word -> ST s (STUArray s Int Word)
spfSieve bound = do
  let (octs,lidx) = idxPr bound
      !mxidx = 8*octs+lidx
      mxval :: Word
      mxval = 30*fromIntegral octs + fromIntegral (rho lidx)
      !mxsve = integerSquareRoot' mxval
      (kr,r) = idxPr mxsve
      !svbd = 8*kr+r
  ar <- newArray (0,mxidx) 0
  let start k i = 8*(k*(30*k+2*rho i) + byte i) + idx i
      tick p stp off j ix
        | mxidx < ix    = return ()
        | otherwise = do
          s <- unsafeRead ar ix
          when (s == 0) (unsafeWrite ar ix p)
          tick p stp off ((j+1) .&. 7) (ix + stp*delta j + tau (off+j))
      sift ix
        | svbd < ix = return ar
        | otherwise = do
          e <- unsafeRead ar ix
          when (e == 0)  (do let i = ix .&. 7
                                 k = ix `shiftR` 3
                                 !off = i `shiftL` 3
                                 !stp = ix - i
                                 !p = toPrim ix
                             tick p stp off i (start k i))
          sift (ix+1)
  sift 0

totSieve :: Word -> UArray Int Word
totSieve bound = runSTUArray $ do
    ar <- spfSieve bound
    (_,lst) <- getBounds ar
    let tot ix
          | lst < ix    = return ar
          | otherwise   = do
            p <- unsafeRead ar ix
            if p == 0
                then unsafeWrite ar ix (toPrim ix - 1)
                else do let !n = toPrim ix
                            (tp,m) = unFact p (n `quot` p)
                        case m of
                          1 -> unsafeWrite ar ix tp
                          _ -> do
                            tm <- unsafeRead ar (toIdx m)
                            unsafeWrite ar ix (tp*tm)
            tot (ix+1)
    tot 0

carSieve :: Word -> UArray Int Word
carSieve bound = runSTUArray $ do
    ar <- spfSieve bound
    (_,lst) <- getBounds ar
    let car ix
          | lst < ix    = return ar
          | otherwise   = do
            p <- unsafeRead ar ix
            if p == 0
                then unsafeWrite ar ix (toPrim ix - 1)
                else do let !n = toPrim ix
                            (tp,m) = unFact p (n `quot` p)
                        case m of
                          1 -> unsafeWrite ar ix tp
                          _ -> do
                            tm <- unsafeRead ar (toIdx m)
                            unsafeWrite ar ix (lcm tp tm)
            car (ix+1)
    car 0

-- Find the p-part of the totient of (p*m) and the cofactor
-- of the p-power in m.
{-# INLINE unFact #-}
unFact :: Word -> Word -> (Word,Word)
unFact p m = go (p-1) m
  where
    go !tt k = case k `quotRem` p of
                 (q,0) -> go (p*tt) q
                 _ -> (tt,k)
