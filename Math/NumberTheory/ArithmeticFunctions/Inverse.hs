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

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Math.NumberTheory.ArithmeticFunctions.Inverse
  ( inverseTotient
  , Counter
  , Max2
  ) where

import Prelude hiding (rem)

import Math.NumberTheory.ArithmeticFunctions
import Math.NumberTheory.Euclidean
import Math.NumberTheory.UniqueFactorisation
import Control.Applicative
import Data.Functor.Classes
import Data.Functor.Compose
import Data.List
import Data.Maybe
import Data.Semigroup

instance Semigroup (f a) => Semigroup (Compose Option f a) where
  Compose ofa <> Compose ofb = Compose (ofa <> ofb)

instance Semigroup (f a) => Monoid (Compose Option f a) where
  mempty = Compose mempty
  mappend = (<>)

instance Show1 Max where
  liftShowsPrec sp _ d (Max x) = showsUnaryWith sp "Max" d x

instance Show1 Min where
  liftShowsPrec sp _ d (Min x) = showsUnaryWith sp "Min" d x

instance Show1 Option where
  liftShowsPrec sp sl d (Option x) = showsUnaryWith (liftShowsPrec sp sl) "Option" d x

-- Variation of @Const Word a@
newtype Counter a = Counter Word
  deriving (Show)

instance Functor Counter where
  fmap _ (Counter w) = Counter w

instance Applicative Counter where
  pure _ = Counter 1
  Counter x <*> Counter y = Counter (x * y)

instance Semigroup (Counter a) where
  Counter x <> Counter y = Counter (x + y)

instance Monoid (Counter a) where
  mempty = Counter 0
  mappend = (<>)

data Max2 a = Max2 (Maybe a) (Maybe a)
  deriving (Show, Functor)

instance Applicative Max2 where
  pure a = Max2 Nothing (Just a)
  Max2 f1 f2 <*> Max2 x1 x2 = Max2 (f1 <*> x2 <|> f2 <*> x1) (f2 <*> x2)

instance Ord a => Semigroup (Max2 a) where
  Max2 a1 b1 <> Max2 a2 b2 = Max2 a3 b3
    where
      [_, _, a3, b3] = sort [a1, b1, a2, b2]

instance Ord a => Monoid (Max2 a) where
  mempty = Max2 Nothing Nothing
  mappend = (<>)

newtype DirichletSeries f a = DirichletSeries { unDirichletSeries :: [(a, f a)] }
  deriving (Show)

lastDS :: Monoid (f a) => DirichletSeries f a -> f a
lastDS (DirichletSeries xs) = case xs of
  [] -> mempty
  _  -> snd $ last xs

filterDS :: (a -> Bool) -> DirichletSeries f a -> DirichletSeries f a
filterDS predicate (DirichletSeries xs) = DirichletSeries $ filter (predicate . fst) xs

zeroDS :: DirichletSeries f a
zeroDS = DirichletSeries mempty

unitDS :: (Applicative f, Num a) => DirichletSeries f a
unitDS = DirichletSeries [(1, pure 1)]

addDS
  :: forall f a.
     (Semigroup (f a), Ord a)
  => DirichletSeries f a
  -> DirichletSeries f a
  -> DirichletSeries f a
addDS (DirichletSeries as) (DirichletSeries bs) = DirichletSeries $ merge as bs
  where
    merge :: [(a, f a)] -> [(a, f a)] -> [(a, f a)]
    merge xs [] = xs
    merge [] ys = ys
    merge xs'@(x'@(x, fx) : xs) ys'@(y'@(y, fy) : ys) = case x `compare` y of
      LT -> x' : merge xs ys'
      EQ -> (x, fx <> fy) : merge xs ys
      GT -> y' : merge xs' ys

mulAndCropDS
  :: forall f a.
     (Applicative f, Semigroup (f a), Ord a, Num a)
  => (a -> Bool)
  -> DirichletSeries f a -- ^ longer series
  -> DirichletSeries f a -- ^ shorter series
  -> DirichletSeries f a
mulAndCropDS predicate (DirichletSeries as) (DirichletSeries bs)
  = foldl addDS zeroDS
  $ map
    (\(b, fb) -> DirichletSeries $ filter (predicate . fst) $ map
      (\(a, fa) -> (a * b, (*) <$> fa <*> fb))
      as)
    bs

atomicSeries
  :: (Applicative f, Semigroup (f a), Num a, Ord a)
  => ArithmeticFunction a a
  -> Prime a
  -> [Word]
  -> DirichletSeries f a
atomicSeries ar p ks = case ar of
  ArithmeticFunction f g -> unitDS `addDS`
    DirichletSeries (map (\k -> (g (f p k), pure (unPrime p ^ k))) ks)

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
  :: forall f a.
     (Applicative f, Monoid (f a), Semigroup (f a), Euclidean a, UniqueFactorisation a, Ord a)
  => ArithmeticFunction a a
  -> InversePrimorials a
  -> a
  -> f a
invertFunction f invF n
  = lastDS
  $ foldl (\ds b -> uncurry processBatch b ds) unitDS batches
  where
    factors = factorise n
    batches = strategy f factors $ invF factors

    processBatch :: a -> [(Prime a, [Word])] -> DirichletSeries f a -> DirichletSeries f a
    processBatch pk xs acc
      = filterDS (\a -> a `rem` pk == 0)
      $ foldl (mulAndCropDS (\a -> n `rem` a == 0)) acc
      $ map (uncurry $ atomicSeries f) xs

-- >>> inverseTotient 120 :: [Integer]
-- [155,310,183,366,225,450,175,350,231,462,143,286,244,372,396,308,248]
-- >>> inverseTotient 120 :: Counter Integer
-- Counter 17
-- >>> inverseTotient 120 :: Compose Option Min Integer
-- Compose (Option (Just (Min 143)))
-- >>> inverseTotient 120 :: Compose Option Max Integer
-- Compose (Option (Just (Max 462)))
-- >>> inverseTotient 120 :: Max2 Integer
-- Max2 (Just 450) (Just 462)
inverseTotient
  :: (Applicative f, Monoid (f a), Semigroup (f a), Euclidean a, UniqueFactorisation a, Ord a)
  => a
  -> f a
inverseTotient n = invertFunction totientA invTotient n
