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
{-# LANGUAGE ViewPatterns          #-}

module Math.NumberTheory.ArithmeticFunctions.Inverse
  ( inverseTotient
  ) where

import Prelude hiding (rem)
import Data.List
import Data.Maybe
import Data.Semigroup
import Data.Semiring (Semiring(..))

import Math.NumberTheory.ArithmeticFunctions
import Math.NumberTheory.Euclidean
import Math.NumberTheory.UniqueFactorisation
import Math.NumberTheory.Utils.Series (DirichletSeries)
import qualified Math.NumberTheory.Utils.Series as DS

atomicSeries
  :: (Semiring b, Num a, Ord a)
  => (a -> b)
  -> ArithmeticFunction a a
  -> Prime a
  -> [Word]
  -> DirichletSeries a b
atomicSeries point ar p ks = case ar of
  ArithmeticFunction f g -> one `plus`
    DS.fromDistinctAscList (map (\k -> (Product (g (f p k)), point (unPrime p ^ k))) ks)

-- from factorisation of n to possible (p, e) s. t. f(p^e) | n
type InversePrimorials a = [(Prime a, Word)] -> [(Prime a, [Word])]

invTotient
  :: forall a. (Num a, UniqueFactorisation a, Eq a)
  => InversePrimorials a
invTotient fs = map (\p -> (p, doPrime p)) ps
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

strategy
  :: forall a. (Euclidean a, Ord a)
  => ArithmeticFunction a a     -- totient function
  -> [(Prime a, Word)]          -- factors of totient value
  -> [(Prime a, [Word])]        -- output of invTotient
  -> [(a, [(Prime a, [Word])])] -- batches with postconditions
strategy tot fs tots = (1, ret) : rets
  where
    fs' = sortBy (\(p1, _) (p2, _) -> p2 `compare` p1) fs

    (ret, rets) = mapAccumL go tots fs'

    go :: [(Prime a, [Word])] -> (Prime a, Word) -> ([(Prime a, [Word])], (a, [(Prime a, [Word])]))
    go ts (unPrime -> p, k) = (rs, (p ^ k, qs))
      where
        predicate (q, ls) = any (\l -> runTot q l `rem` p == 0) ls
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
  = DS.last
  $ foldl (\ds b -> uncurry processBatch b ds) one batches
  where
    factors = factorise n
    batches = strategy f factors $ invF factors

    processBatch :: a -> [(Prime a, [Word])] -> DirichletSeries a b -> DirichletSeries a b
    processBatch pk xs acc
      = DS.filter (\(Product a) -> a `rem` pk == 0)
      $ foldl (DS.timesAndCrop (\(Product a) -> n `rem` a == 0)) acc
      $ map (uncurry $ atomicSeries point f) xs

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
inverseTotient
  :: (Semiring b, Euclidean a, UniqueFactorisation a, Ord a)
  => (a -> b)
  -> a
  -> b
inverseTotient point n = invertFunction point totientA invTotient n
