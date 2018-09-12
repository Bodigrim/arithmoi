-- |
-- Module:      Math.NumberTheory.ArithmeticFunctions.Inverse
-- Copyright:   (c) 2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Computing inverses of multiplicative functions.
-- The implementation is based on
-- <https://www.emis.de/journals/JIS/VOL19/Alekseyev/alek5.pdf Computing the Inverses, their Power Sums, and Extrema for Euler’s Totient and Other Multiplicative Functions>
-- by M. A. Alekseyev.

{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.ArithmeticFunctions.Inverse
  ( inverseTotient
  , inverseSigma
  , -- * Wrappers
    MinWord(..)
  , MaxWord(..)
  , MinNatural(..)
  , MaxNatural(..)
  , -- * Utils
    asSetOfPreimages
  ) where

import Prelude hiding (rem, quot)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord (Down(..))
import Data.Semigroup
import Data.Semiring (Semiring(..))
import Data.Set (Set)
import qualified Data.Set as S
import Numeric.Natural

import Math.NumberTheory.ArithmeticFunctions
import Math.NumberTheory.Euclidean
import Math.NumberTheory.Logarithms
import Math.NumberTheory.Powers
import Math.NumberTheory.Primes.Sieve (primes)
import Math.NumberTheory.UniqueFactorisation
import Math.NumberTheory.Utils.DirichletSeries (DirichletSeries)
import qualified Math.NumberTheory.Utils.DirichletSeries as DS
import Math.NumberTheory.Utils.FromIntegral

data PrimePowers a = PrimePowers
  { _ppPrime  :: Prime a
  , _ppPowers :: [Word] -- sorted list
  }

instance Show a => Show (PrimePowers a) where
  show (PrimePowers p xs) = "PP " ++ show (unPrime p) ++ " " ++ show xs

-- | Convert a list of powers of a prime into an atomic Dirichlet series
-- (Section 4, Step 2).
atomicSeries
  :: (Semiring b, Num a, Ord a)
  => (a -> b)               -- ^ How to inject a number into a semiring
  -> ArithmeticFunction a c -- ^ Arithmetic function, which we aim to inverse
  -> PrimePowers a          -- ^ List of powers of a prime
  -> DirichletSeries c b    -- ^ Atomic Dirichlet series
atomicSeries point (ArithmeticFunction f g) (PrimePowers p ks) =
  DS.fromDistinctAscList (map (\k -> (g (f p k), point (unPrime p ^ k))) ks)

-- | See section 5.1 of the paper.
invTotient
  :: forall a. (Num a, UniqueFactorisation a, Eq a)
  => [(Prime a, Word)]
  -- ^ Factorisation of a value of the totient function
  -> [PrimePowers a]
  -- ^ Possible prime factors of an argument of the totient function
invTotient fs = map (\p -> PrimePowers p (doPrime p)) ps
  where
    divs :: [a]
    divs = runFunctionOnFactors divisorsListA fs

    ps :: [Prime a]
    ps = mapMaybe (isPrime . (+ 1)) divs

    doPrime :: Prime a -> [Word]
    doPrime p = case lookup p fs of
      Nothing -> [1]
      Just k  -> [1 .. k+1]

-- | See section 5.2 of the paper.
invSigma
  :: forall a. (Euclidean a, Integral a, UniqueFactorisation a)
  => [(Prime a, Word)]
  -- ^ Factorisation of a value of the sum-of-divisors function
  -> [PrimePowers a]
  -- ^ Possible prime factors of an argument of the sum-of-divisors function
invSigma fs
  = map (\(x, ys) -> PrimePowers x (S.toList ys))
  $ M.assocs
  $ M.unionWith (<>) pksSmall pksLarge
  where
    numDivs :: a
    numDivs = runFunctionOnFactors tauA fs

    divs :: [a]
    divs = runFunctionOnFactors divisorsListA fs

    n :: a
    n = product $ map (\(p, k) -> unPrime p ^ k) fs

    -- There are two possible strategies to find possible prime factors
    -- of an argument of the sum-of-divisors function.
    -- 1. Take each prime p and each power e such that p^e <= n,
    -- compute sigma(p^e) and check whether it is a divisor of n.
    -- (corresponds to 'pksSmall' below)
    -- 2. Take each divisor d of n and each power e such that e <= log_2 d,
    -- compute p = floor(e-th root of (d - 1)) and check whether sigma(p^e) = d
    -- and p is actually prime (correposnds to 'pksLarge' below).
    --
    -- Asymptotically the second strategy is beneficial, but computing
    -- exact e-th roots of huge integers (especially when they exceed MAX_DOUBLE)
    -- is very costly. That is why we employ the first strategy for primes
    -- below limit 'lim' and the second one for larger ones. This allows us
    -- to loop over e <= log_lim d which is much smaller than log_2 d.
    --
    -- The value of 'lim' below was chosen heuristically;
    -- it may be tuned in future in accordance with new experimental data.
    lim :: a
    lim = numDivs `max` 2

    pksSmall :: Map (Prime a) (Set Word)
    pksSmall = M.fromDistinctAscList
      [ (p, pows)
      | p <- takeWhile ((< lim) . unPrime) primes
      , let pows = doPrime p
      , not (null pows)
      ]

    doPrime :: Prime a -> Set Word
    doPrime p' = let p = unPrime p' in S.fromDistinctAscList
      [ e
      | e <- [1 .. intToWord (integerLogBase (toInteger p) (toInteger n))]
      , n `rem` ((p ^ (e + 1) - 1) `quot` (p - 1)) == 0
      ]

    pksLarge :: Map (Prime a) (Set Word)
    pksLarge = M.unionsWith (<>)
      [ maybe mempty (flip M.singleton (S.singleton e)) (isPrime p)
      | d <- divs
      , e <- [1 .. intToWord (integerLogBase (toInteger lim) (toInteger d))]
      , let p = integerRoot e (d - 1)
      , p ^ (e + 1) - 1 == d * (p - 1)
      ]

-- | Instead of multiplying all atomic series and filtering out everything,
-- which is not divisible by @n@, we'd rather split all atomic series into
-- a couple of batches, each of which corresponds to a prime factor of @n@.
-- This allows us to crop resulting Dirichlet series (see 'filter' calls
-- in 'invertFunction' below) at the end of each batch, saving time and memory.
strategy
  :: forall a c. (Euclidean c, Ord c)
  => ArithmeticFunction a c
  -- ^ Arithmetic function, which we aim to inverse
  -> [(Prime c, Word)]
  -- ^ Factorisation of a value of the arithmetic function
  -> [PrimePowers a]
  -- ^ Possible prime factors of an argument of the arithmetic function
  -> [(Maybe (Prime c, Word), [PrimePowers a])]
  -- ^ Batches, corresponding to each element of the factorisation of a value
strategy (ArithmeticFunction f g) factors args = (Nothing, ret) : rets
  where
    (ret, rets)
      = mapAccumL go args
      $ sortOn (Down . fst) factors

    go
      :: [PrimePowers a]
      -> (Prime c, Word)
      -> ([PrimePowers a], (Maybe (Prime c, Word), [PrimePowers a]))
    go ts (p, k) = (rs, (Just (p, k), qs))
      where
        predicate (PrimePowers q ls) = any (\l -> g (f q l) `rem` unPrime p == 0) ls
        (qs, rs) = partition predicate ts

-- | Main workhorse.
invertFunction
  :: forall a b c.
     (Num a, Ord a, Semiring b, Euclidean c, UniqueFactorisation c, Ord c)
  => (a -> b)
  -- ^ How to inject a number into a semiring
  -> ArithmeticFunction a c
  -- ^ Arithmetic function, which we aim to inverse
  -> ([(Prime c, Word)] -> [PrimePowers a])
  -- ^ How to find possible prime factors of the argument
  -> c
  -- ^ Value of the arithmetic function, which we aim to inverse
  -> b
  -- ^ Semiring element, representing preimages
invertFunction point f invF n
  = DS.lookup n
  $ foldl' (\ds b -> uncurry processBatch b ds) (DS.fromDistinctAscList []) batches
  where
    factors = factorise n
    batches = strategy f factors $ invF factors

    processBatch
      :: Maybe (Prime c, Word)
      -> [PrimePowers a]
      -> DirichletSeries c b
      -> DirichletSeries c b
    processBatch Nothing xs acc
      = foldl' (DS.timesAndCrop n) acc
      $ map (atomicSeries point f) xs

    -- This is equivalent to the next, general case, but is faster,
    -- since it avoids construction of many intermediate series.
    processBatch (Just (p, 1)) xs acc
      = DS.filter (\a -> a `rem` unPrime p == 0)
      $ foldl' (DS.timesAndCrop n) acc'
      $ map (atomicSeries point f) xs2
      where
        (xs1, xs2) = partition (\(PrimePowers _ ts) -> length ts == 1) xs
        vs = DS.unions $ map (atomicSeries point f) xs1
        (ys, zs) = DS.partition (\a -> a `rem` unPrime p == 0) acc
        acc' = ys `DS.union` DS.timesAndCrop n zs vs

    processBatch (Just pk) xs acc
      = (\(p, k) -> DS.filter (\a -> a `rem` (unPrime p ^ k) == 0)) pk
      $ foldl' (DS.timesAndCrop n) acc
      $ map (atomicSeries point f) xs

{-# SPECIALISE invertFunction :: Semiring b => (Int -> b) -> ArithmeticFunction Int Int -> ([(Prime Int, Word)] -> [PrimePowers Int]) -> Int -> b #-}
{-# SPECIALISE invertFunction :: Semiring b => (Word -> b) -> ArithmeticFunction Word Word -> ([(Prime Word, Word)] -> [PrimePowers Word]) -> Word -> b #-}
{-# SPECIALISE invertFunction :: Semiring b => (Integer -> b) -> ArithmeticFunction Integer Integer -> ([(Prime Integer, Word)] -> [PrimePowers Integer]) -> Integer -> b #-}
{-# SPECIALISE invertFunction :: Semiring b => (Natural -> b) -> ArithmeticFunction Natural Natural -> ([(Prime Natural, Word)] -> [PrimePowers Natural]) -> Natural -> b #-}

-- | The inverse for 'totient' function.
--
-- The return value is parameterized by a 'Semiring', which allows
-- various applications by providing different (multiplicative) embeddings.
-- E. g., list all preimages (see a helper 'asSetOfPreimages'):
--
-- >>> import qualified Data.Set as S
-- >>> import Data.Semigroup
-- >>> S.mapMonotonic getProduct (inverseTotient (S.singleton . Product) 120)
-- fromList [143,155,175,183,225,231,244,248,286,308,310,350,366,372,396,450,462]
--
-- Count preimages:
--
-- >>> inverseTotient (const 1) 120
-- 17
--
-- Sum preimages:
--
-- >>> inverseTotient id 120
-- 4904
--
-- Find minimal and maximal preimages:
--
-- >>> unMinWord (inverseTotient MinWord 120)
-- 143
-- >>> unMaxWord (inverseTotient MaxWord 120)
-- 462
inverseTotient
  :: (Semiring b, Euclidean a, UniqueFactorisation a, Ord a)
  => (a -> b)
  -> a
  -> b
inverseTotient point = invertFunction point totientA invTotient
{-# SPECIALISE inverseTotient :: Semiring b => (Int -> b) -> Int -> b #-}
{-# SPECIALISE inverseTotient :: Semiring b => (Word -> b) -> Word -> b #-}
{-# SPECIALISE inverseTotient :: Semiring b => (Integer -> b) -> Integer -> b #-}
{-# SPECIALISE inverseTotient :: Semiring b => (Natural -> b) -> Natural -> b #-}

-- | The inverse for 'sigma' 1 function.
--
-- The return value is parameterized by a 'Semiring', which allows
-- various applications by providing different (multiplicative) embeddings.
-- E. g., list all preimages (see a helper 'asSetOfPreimages'):
--
-- >>> import qualified Data.Set as S
-- >>> import Data.Semigroup
-- >>> S.mapMonotonic getProduct (inverseSigma (S.singleton . Product) 120)
-- fromList [54,56,87,95]
--
-- Count preimages:
--
-- >>> inverseSigma (const 1) 120
-- 4
--
-- Sum preimages:
--
-- >>> inverseSigma id 120
-- 292
--
-- Find minimal and maximal preimages:
--
-- >>> unMinWord (inverseSigma MinWord 120)
-- 54
-- >>> unMaxWord (inverseSigma MaxWord 120)
-- 95
inverseSigma
  :: (Semiring b, Euclidean a, UniqueFactorisation a, Integral a)
  => (a -> b)
  -> a
  -> b
inverseSigma point = invertFunction point (sigmaA 1) invSigma
{-# SPECIALISE inverseSigma :: Semiring b => (Int -> b) -> Int -> b #-}
{-# SPECIALISE inverseSigma :: Semiring b => (Word -> b) -> Word -> b #-}
{-# SPECIALISE inverseSigma :: Semiring b => (Integer -> b) -> Integer -> b #-}
{-# SPECIALISE inverseSigma :: Semiring b => (Natural -> b) -> Natural -> b #-}

--------------------------------------------------------------------------------
-- Wrappers

-- | Wrapper to use in conjunction with 'inverseTotient' and 'inverseSigma'.
-- Extracts the maximal preimage of function.
newtype MaxWord = MaxWord { unMaxWord :: Word }
  deriving (Eq, Ord, Show)

instance Semiring MaxWord where
  zero = MaxWord minBound
  one  = MaxWord 1
  plus  (MaxWord a) (MaxWord b) = MaxWord (a `max` b)
  times (MaxWord a) (MaxWord b) = MaxWord (a * b)

-- | Wrapper to use in conjunction with 'inverseTotient' and 'inverseSigma'.
-- Extracts the minimal preimage of function.
newtype MinWord = MinWord { unMinWord :: Word }
  deriving (Eq, Ord, Show)

instance Semiring MinWord where
  zero = MinWord maxBound
  one  = MinWord 1
  plus  (MinWord a) (MinWord b) = MinWord (a `min` b)
  times (MinWord a) (MinWord b) = MinWord (a * b)

-- | Wrapper to use in conjunction with 'inverseTotient' and 'inverseSigma'.
-- Extracts the maximal preimage of function.
newtype MaxNatural = MaxNatural { unMaxNatural :: Natural }
  deriving (Eq, Ord, Show)

instance Semiring MaxNatural where
  zero = MaxNatural 0
  one  = MaxNatural 1
  plus  (MaxNatural a) (MaxNatural b) = MaxNatural (a `max` b)
  times (MaxNatural a) (MaxNatural b) = MaxNatural (a * b)

-- | Wrapper to use in conjunction with 'inverseTotient' and 'inverseSigma'.
-- Extracts the minimal preimage of function.
data MinNatural
  = MinNatural { unMinNatural :: !Natural }
  | Infinity
  deriving (Eq, Ord, Show)

instance Semiring MinNatural where
  zero = Infinity
  one  = MinNatural 1

  plus a b = a `min` b

  times Infinity _ = Infinity
  times _ Infinity = Infinity
  times (MinNatural a) (MinNatural b) = MinNatural (a * b)

-- | Helper to extract a set of preimages for 'inverseTotient' or 'inverseSigma'.
asSetOfPreimages
  :: (Euclidean a, Integral a)
  => (forall b. Semiring b => (a -> b) -> a -> b)
  -> a
  -> S.Set a
asSetOfPreimages f = S.mapMonotonic getProduct . f (S.singleton . Product)
