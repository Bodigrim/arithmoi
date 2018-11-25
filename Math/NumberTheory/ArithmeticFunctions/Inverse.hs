-- |
-- Module:      Math.NumberTheory.ArithmeticFunctions.Inverse
-- Copyright:   (c) 2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- The inverse totient function.
-- https://www.emis.de/journals/JIS/VOL19/Alekseyev/alek5.pdf
--

{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}

module Math.NumberTheory.ArithmeticFunctions.Inverse
  ( inverseTotient
  , inverseSigma
  , -- * Wrappers
    MinWord(..)
  , MaxWord(..)
  , MinNatural(..)
  , MaxNatural(..)
  ) where

import Prelude hiding (rem, quot)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Semiring (Semiring(..))
import qualified Data.Set as S
import Numeric.Natural

import Math.NumberTheory.ArithmeticFunctions
import Math.NumberTheory.Euclidean
import Math.NumberTheory.Logarithms
import Math.NumberTheory.Powers
import Math.NumberTheory.Primes (primes)
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

atomicSeries
  :: (Semiring b, Num a, Ord a)
  => (a -> b)
  -> ArithmeticFunction a a
  -> PrimePowers a
  -> DirichletSeries a b
atomicSeries point ar (PrimePowers p ks) = case ar of
  ArithmeticFunction f g -> DS.fromDistinctAscList (map (\k -> (g (f p k), point (unPrime p ^ k))) ks)

-- from factorisation of n to possible (p, e) s. t. f(p^e) | n
type InversePrimorials a = [(Prime a, Word)] -> [PrimePowers a]

invTotient
  :: forall a. (Num a, UniqueFactorisation a, Eq a)
  => InversePrimorials a
invTotient fs = map (\p -> PrimePowers p (doPrime p)) ps
  where
    divs :: [a]
    divs = case divisorsListA of
      ArithmeticFunction f g -> g $ mconcat $ map (uncurry f) fs

    ps :: [Prime a]
    ps = mapMaybe (isPrime . (+ 1)) divs

    -- TODO: optimize linear lookup
    doPrime :: Prime a -> [Word]
    doPrime p = case lookup p fs of
      Nothing -> [1]
      Just k  -> [1 .. k+1]

invSigma
  :: forall a. (Euclidean a, Integral a, UniqueFactorisation a)
  => InversePrimorials a
invSigma fs
  = map (\(x, ys) -> PrimePowers x (S.toList ys))
  $ M.assocs
  $ M.unionWith (<>) pksSmall pksLarge
  where
    numDivs :: a
    numDivs = case tauA of
      ArithmeticFunction f g -> g $ mconcat $ map (uncurry f) fs

    lim :: a
    lim = numDivs `max` 2

    divs :: [a]
    divs = case divisorsListA of
      ArithmeticFunction f g -> g $ mconcat $ map (uncurry f) fs

    n :: a
    n = product $ map (\(p, k) -> unPrime p ^ k) fs

    pksSmall :: Map (Prime a) (S.Set Word)
    pksSmall
      = M.fromDistinctAscList
      $ filter (not . null . snd)
      $ map (\p -> (p, doPrime p))
      $ takeWhile ((< lim) . unPrime)
      $ primes

    doPrime :: Prime a -> S.Set Word
    doPrime (unPrime -> p) = S.fromDistinctAscList
      [ e
      | e <- [1 .. intToWord (integerLogBase (toInteger p) (toInteger n))]
      , n `rem` ((p ^ (e + 1) - 1) `quot` (p - 1)) == 0
      ]

    pksLarge :: M.Map (Prime a) (S.Set Word)
    pksLarge = M.unionsWith (<>)
      [ maybe mempty (flip M.singleton (S.singleton e)) (isPrime p)
      | d <- divs
      , e <- [1 .. intToWord (integerLogBase (toInteger lim) (toInteger d))]
      , let p = integerRoot e (d - 1)
      , p ^ (e + 1) - 1 == d * (p - 1)
      ]

strategy
  :: forall a. (Euclidean a, Ord a)
  => ArithmeticFunction a a -- totient function
  -> [(Prime a, Word)]      -- factors of totient value
  -> [PrimePowers a]        -- output of invTotient
  -> [(Maybe (Prime a, Word), [PrimePowers a])] -- batches with postconditions
strategy tot fs tots = (Nothing, ret) : rets
  where
    fs' = sortBy (\(p1, _) (p2, _) -> p2 `compare` p1) fs

    (ret, rets) = mapAccumL go tots fs'

    go
      :: [PrimePowers a]
      -> (Prime a, Word)
      -> ([PrimePowers a], (Maybe (Prime a, Word), [PrimePowers a]))
    go ts (p, k) = (rs, (Just (p, k), qs))
      where
        predicate (PrimePowers q ls) = any (\l -> runTot q l `rem` unPrime p == 0) ls
        (qs, rs) = partition predicate ts

    runTot :: Prime a -> Word -> a
    runTot p k = case tot of
      ArithmeticFunction f g -> g $ f p k

invertFunction
  :: forall a b.
     (Semiring b, Euclidean a, UniqueFactorisation a, Ord a)
  => (a -> b)
  -> ArithmeticFunction a a
  -> InversePrimorials a
  -> a
  -> b
invertFunction point f invF n
  = DS.last n
  $ foldl (\ds b -> uncurry processBatch b ds) (DS.fromDistinctAscList []) batches
  where
    factors = factorise n
    batches = strategy f factors $ invF factors

    processBatch
      :: Maybe (Prime a, Word)
      -> [PrimePowers a]
      -> DirichletSeries a b
      -> DirichletSeries a b
    processBatch Nothing xs acc
      = foldl (DS.timesAndCrop n) acc
      $ map (atomicSeries point f) xs

    processBatch (Just (p, 1)) xs acc
      = DS.filter (\a -> a `rem` unPrime p == 0)
      $ foldl (DS.timesAndCrop n) acc'
      $ map (atomicSeries point f) xs2
      where
        (xs1, xs2) = partition (\(PrimePowers _ ts) -> length ts == 1) xs
        vs = DS.unions $ map (atomicSeries point f) xs1
        (ys, zs) = DS.partition (\a -> a `rem` unPrime p == 0) acc
        acc' = ys `DS.union` DS.timesAndCrop n zs vs

    processBatch (Just pk) xs acc
      = (\(p, k) -> DS.filter (\a -> a `rem` (unPrime p ^ k) == 0)) pk
      $ foldl (DS.timesAndCrop n) acc
      $ map (atomicSeries point f) xs

{-# SPECIALISE invertFunction :: Semiring b => (Integer -> b) -> ArithmeticFunction Integer Integer -> InversePrimorials Integer -> Integer -> b #-}

-- | The inverse 'totient' function such that
--
-- > all ((== x) . totient) (inverseTotient x)
-- > x `elem` inverseTotient (totient x)
--
-- The return value is parametrized by a semiring, which allows
-- various applications. E. g., list all preimages:
--
-- >>> import qualified Data.Set as S
-- >>> S.map getProduct (inverseTotient (S.singleton . Product) 120) :: Set Integer
-- fromList [143,155,175,183,225,231,244,248,286,308,310,350,366,372,396,450,462]
--
-- Count preimages:
--
-- >>> import Control.Applicative
-- >>> import Data.Semigroup
-- >>> inverseTotient (const $ Const 1) 120 :: Const (Sum Word) Integer
-- Const (Sum {getSum = 17})
--
-- Find minimal and maximal preimages:
--
-- >>> inverseTotient MinWord 120 :: MinWord
-- MinWord {unMinWord = 143}
-- >>> inverseTotient MaxWord 120 :: MaxWord
-- MaxWord {unMaxWord = 462}
inverseTotient
  :: (Semiring b, Euclidean a, UniqueFactorisation a, Ord a)
  => (a -> b)
  -> a
  -> b
inverseTotient point n = invertFunction point totientA invTotient n
{-# SPECIALISE inverseTotient :: Semiring b => (Integer -> b) -> Integer -> b #-}

inverseSigma
  :: (Semiring b, Euclidean a, UniqueFactorisation a, Integral a)
  => (a -> b)
  -> a
  -> b
inverseSigma point n = invertFunction point (sigmaA 1) invSigma n
{-# SPECIALISE inverseSigma :: Semiring b => (Integer -> b) -> Integer -> b #-}

--------------------------------------------------------------------------------
-- Wrappers

-- | Wrapper to use in conjunction with 'inverseTotient'.
-- Extracts the maximal preimage of function.
newtype MaxWord = MaxWord { unMaxWord :: Word }
  deriving (Show)

instance Semiring MaxWord where
  zero = MaxWord minBound
  one  = MaxWord 1
  plus  (MaxWord a) (MaxWord b) = MaxWord (a `max` b)
  times (MaxWord a) (MaxWord b) = MaxWord (a * b)

-- | Wrapper to use in conjunction with 'inverseTotient'.
-- Extracts the minimal preimage of function.
newtype MinWord = MinWord { unMinWord :: Word }
  deriving (Show)

instance Semiring MinWord where
  zero = MinWord maxBound
  one  = MinWord 1
  plus  (MinWord a) (MinWord b) = MinWord (a `min` b)
  times (MinWord a) (MinWord b) = MinWord (a * b)

-- | Wrapper to use in conjunction with 'inverseTotient'.
-- Extracts the maximal preimage of function.
newtype MaxNatural = MaxNatural { unMaxNatural :: Natural }
  deriving (Show)

instance Semiring MaxNatural where
  zero = MaxNatural 0
  one  = MaxNatural 1
  plus  (MaxNatural a) (MaxNatural b) = MaxNatural (a `max` b)
  times (MaxNatural a) (MaxNatural b) = MaxNatural (a * b)

-- | Wrapper to use in conjunction with 'inverseTotient'.
-- Extracts the minimal preimage of function.
-- 'Nothing' stands for a positive infinity.
newtype MinNatural = MinNatural { unMinNatural :: Maybe Natural }
  deriving (Show)

instance Semiring MinNatural where
  zero = MinNatural Nothing
  one  = MinNatural (Just 1)

  plus (MinNatural Nothing) b = b
  plus a (MinNatural Nothing) = a
  plus (MinNatural (Just a)) (MinNatural (Just b)) = MinNatural (Just (a `min` b))

  times (MinNatural a) (MinNatural b) = MinNatural ((*) <$> a <*> b)
