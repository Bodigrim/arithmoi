-- |
-- Module:      Math.NumberTheory.TestUtils.Wrappers
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Utils to test Math.NumberTheory
--

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.TestUtils.Wrappers where

import Control.Applicative
import Data.Functor.Classes
#if MIN_VERSION_base(4,8,0)
#else
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
#endif

import Test.Tasty.QuickCheck as QC hiding (Positive, NonNegative, generate, getNonNegative, getPositive)
import Test.SmallCheck.Series (Positive(..), NonNegative(..), Serial(..), Series)

import Math.NumberTheory.Primes (isPrime)

-------------------------------------------------------------------------------
-- AnySign

newtype AnySign a = AnySign { getAnySign :: a }
  deriving (Eq, Ord, Read, Show, Num, Enum, Bounded, Integral, Real, Functor, Foldable, Traversable, Arbitrary)

instance (Monad m, Serial m a) => Serial m (AnySign a) where
  series = AnySign <$> series

instance Eq1 AnySign where
#if MIN_VERSION_transformers(0,5,0)
  liftEq eq (AnySign a) (AnySign b) = a `eq` b
#else
  (AnySign a) `eq1` (AnySign b) = a == b
#endif

instance Ord1 AnySign where
#if MIN_VERSION_transformers(0,5,0)
  liftCompare cmp (AnySign a) (AnySign b) = a `cmp` b
#else
  (AnySign a) `compare1` (AnySign b) = a `compare` b
#endif

instance Show1 AnySign where
#if MIN_VERSION_transformers(0,5,0)
  liftShowsPrec shw _ p (AnySign a) = shw p a
#else
  showsPrec1 p (AnySign a) = showsPrec p a
#endif

-------------------------------------------------------------------------------
-- Positive from smallcheck

deriving instance Functor Positive

instance (Num a, Ord a, Arbitrary a) => Arbitrary (Positive a) where
  arbitrary = Positive <$> (arbitrary `suchThat` (> 0))
  shrink (Positive x) = Positive <$> filter (> 0) (shrink x)

instance (Num a, Bounded a) => Bounded (Positive a) where
  minBound = Positive 1
  maxBound = Positive (maxBound :: a)

instance Eq1 Positive where
#if MIN_VERSION_transformers(0,5,0)
  liftEq eq (Positive a) (Positive b) = a `eq` b
#else
  (Positive a) `eq1` (Positive b) = a == b
#endif

instance Ord1 Positive where
#if MIN_VERSION_transformers(0,5,0)
  liftCompare cmp (Positive a) (Positive b) = a `cmp` b
#else
  (Positive a) `compare1` (Positive b) = a `compare` b
#endif

instance Show1 Positive where
#if MIN_VERSION_transformers(0,5,0)
  liftShowsPrec shw _ p (Positive a) = shw p a
#else
  showsPrec1 p (Positive a) = showsPrec p a
#endif

-------------------------------------------------------------------------------
-- NonNegative from smallcheck

deriving instance Functor NonNegative

instance (Num a, Ord a, Arbitrary a) => Arbitrary (NonNegative a) where
  arbitrary = NonNegative <$> (arbitrary `suchThat` (>= 0))
  shrink (NonNegative x) = NonNegative <$> filter (>= 0) (shrink x)

instance (Num a, Bounded a) => Bounded (NonNegative a) where
  minBound = NonNegative 0
  maxBound = NonNegative (maxBound :: a)

instance Eq1 NonNegative where
#if MIN_VERSION_transformers(0,5,0)
  liftEq eq (NonNegative a) (NonNegative b) = a `eq` b
#else
  (NonNegative a) `eq1` (NonNegative b) = a == b
#endif

instance Ord1 NonNegative where
#if MIN_VERSION_transformers(0,5,0)
  liftCompare cmp (NonNegative a) (NonNegative b) = a `cmp` b
#else
  (NonNegative a) `compare1` (NonNegative b) = a `compare` b
#endif

instance Show1 NonNegative where
#if MIN_VERSION_transformers(0,5,0)
  liftShowsPrec shw _ p (NonNegative a) = shw p a
#else
  showsPrec1 p (NonNegative a) = showsPrec p a
#endif

-------------------------------------------------------------------------------
-- Huge

newtype Huge a = Huge { getHuge :: a }
  deriving (Eq, Ord, Read, Show, Num, Enum, Bounded, Integral, Real, Functor, Foldable, Traversable)

instance (Num a, Arbitrary a) => Arbitrary (Huge a) where
  arbitrary = do
    Positive l <- arbitrary
    ds <- vector l
    return $ Huge $ foldl1 (\acc n -> acc * 2^63 + n) ds

instance Eq1 Huge where
#if MIN_VERSION_transformers(0,5,0)
  liftEq eq (Huge a) (Huge b) = a `eq` b
#else
  (Huge a) `eq1` (Huge b) = a == b
#endif

instance Ord1 Huge where
#if MIN_VERSION_transformers(0,5,0)
  liftCompare cmp (Huge a) (Huge b) = a `cmp` b
#else
  (Huge a) `compare1` (Huge b) = a `compare` b
#endif

instance Show1 Huge where
#if MIN_VERSION_transformers(0,5,0)
  liftShowsPrec shw _ p (Huge a) = shw p a
#else
  showsPrec1 p (Huge a) = showsPrec p a
#endif

-------------------------------------------------------------------------------
-- Power

newtype Power a = Power { getPower :: a }
  deriving (Eq, Ord, Read, Show, Num, Enum, Bounded, Integral, Real, Functor, Foldable, Traversable)

instance (Monad m, Num a, Ord a, Serial m a) => Serial m (Power a) where
  series = Power <$> series `suchThatSerial` (> 0)

instance (Num a, Ord a, Integral a, Arbitrary a) => Arbitrary (Power a) where
  arbitrary = Power <$> (getSmall <$> arbitrary) `suchThat` (> 0)
  shrink (Power x) = Power <$> filter (> 0) (shrink x)

instance Eq1 Power where
#if MIN_VERSION_transformers(0,5,0)
  liftEq eq (Power a) (Power b) = a `eq` b
#else
  (Power a) `eq1` (Power b) = a == b
#endif

instance Ord1 Power where
#if MIN_VERSION_transformers(0,5,0)
  liftCompare cmp (Power a) (Power b) = a `cmp` b
#else
  (Power a) `compare1` (Power b) = a `compare` b
#endif

instance Show1 Power where
#if MIN_VERSION_transformers(0,5,0)
  liftShowsPrec shw _ p (Power a) = shw p a
#else
  showsPrec1 p (Power a) = showsPrec p a
#endif

-------------------------------------------------------------------------------
-- Odd

newtype Odd a = Odd { getOdd :: a }
  deriving (Eq, Ord, Read, Show, Num, Enum, Bounded, Integral, Real, Functor, Foldable, Traversable)

instance (Monad m, Serial m a, Integral a) => Serial m (Odd a) where
  series = Odd <$> series `suchThatSerial` odd

instance (Integral a, Arbitrary a) => Arbitrary (Odd a) where
  arbitrary = Odd <$> (arbitrary `suchThat` odd)
  shrink (Odd x) = Odd <$> filter odd (shrink x)

instance Eq1 Odd where
#if MIN_VERSION_transformers(0,5,0)
  liftEq eq (Odd a) (Odd b) = a `eq` b
#else
  (Odd a) `eq1` (Odd b) = a == b
#endif

instance Ord1 Odd where
#if MIN_VERSION_transformers(0,5,0)
  liftCompare cmp (Odd a) (Odd b) = a `cmp` b
#else
  (Odd a) `compare1` (Odd b) = a `compare` b
#endif

instance Show1 Odd where
#if MIN_VERSION_transformers(0,5,0)
  liftShowsPrec shw _ p (Odd a) = shw p a
#else
  showsPrec1 p (Odd a) = showsPrec p a
#endif

-------------------------------------------------------------------------------
-- Prime

newtype Prime = Prime { getPrime :: Integer }
  deriving (Eq, Ord, Show)

instance Arbitrary Prime where
  arbitrary = Prime <$> arbitrary `suchThat` (\p -> p > 0 && isPrime p)

instance Monad m => Serial m Prime where
  series = Prime <$> series `suchThatSerial` (\p -> p > 0 && isPrime p)

-------------------------------------------------------------------------------
-- Utils

suchThatSerial :: Series m a -> (a -> Bool) -> Series m a
suchThatSerial s p = s >>= \x -> if p x then pure x else empty
