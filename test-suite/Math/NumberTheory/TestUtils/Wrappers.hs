-- |
-- Module:      Math.NumberTheory.TestUtils.Wrappers
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Utils to test Math.NumberTheory
--

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.TestUtils.Wrappers where

import Control.Applicative
import Data.Coerce
import Data.Euclidean
import Data.Functor.Classes
import Data.Semiring (Semiring)

import Test.Tasty.QuickCheck as QC hiding (Positive(..), NonNegative(..), NonZero(..))
import Test.SmallCheck.Series (Positive(..), NonNegative(..), NonZero(..), Serial(..), Series)

import Math.NumberTheory.Primes (Prime, UniqueFactorisation(..))

-------------------------------------------------------------------------------
-- AnySign

newtype AnySign a = AnySign { getAnySign :: a }
  deriving (Eq, Ord, Read, Show, Num, Enum, Bounded, Integral, Real, Functor, Foldable, Traversable, Arbitrary, Semiring, GcdDomain, Euclidean)

instance (Monad m, Serial m a) => Serial m (AnySign a) where
  series = AnySign <$> series

instance Eq1 AnySign where
  liftEq eq (AnySign a) (AnySign b) = a `eq` b

instance Ord1 AnySign where
  liftCompare cmp (AnySign a) (AnySign b) = a `cmp` b

instance Show1 AnySign where
  liftShowsPrec shw _ p (AnySign a) = shw p a

-------------------------------------------------------------------------------
-- Positive from smallcheck

deriving instance Semiring a => Semiring (Positive a)
deriving instance GcdDomain a => GcdDomain (Positive a)
deriving instance Euclidean a => Euclidean (Positive a)

instance (Num a, Ord a, Arbitrary a) => Arbitrary (Positive a) where
  arbitrary = Positive <$> (arbitrary `suchThat` (> 0))
  shrink (Positive x) = Positive <$> filter (> 0) (shrink x)

instance Eq1 Positive where
  liftEq eq (Positive a) (Positive b) = a `eq` b

instance Ord1 Positive where
  liftCompare cmp (Positive a) (Positive b) = a `cmp` b

instance Show1 Positive where
  liftShowsPrec shw _ p (Positive a) = shw p a

-------------------------------------------------------------------------------
-- NonNegative from smallcheck

deriving instance Semiring a => Semiring (NonNegative a)
deriving instance GcdDomain a => GcdDomain (NonNegative a)
deriving instance Euclidean a => Euclidean (NonNegative a)

instance (Num a, Ord a, Arbitrary a) => Arbitrary (NonNegative a) where
  arbitrary = NonNegative <$> (arbitrary `suchThat` (>= 0))
  shrink (NonNegative x) = NonNegative <$> filter (>= 0) (shrink x)

instance Eq1 NonNegative where
  liftEq eq (NonNegative a) (NonNegative b) = a `eq` b

instance Ord1 NonNegative where
  liftCompare cmp (NonNegative a) (NonNegative b) = a `cmp` b

instance Show1 NonNegative where
  liftShowsPrec shw _ p (NonNegative a) = shw p a

-------------------------------------------------------------------------------
-- NonZero from smallcheck

deriving instance Semiring a => Semiring (NonZero a)
deriving instance GcdDomain a => GcdDomain (NonZero a)
deriving instance Euclidean a => Euclidean (NonZero a)

instance (Num a, Ord a, Arbitrary a) => Arbitrary (NonZero a) where
  arbitrary = NonZero <$> (arbitrary `suchThat` (/= 0))
  shrink (NonZero x) = NonZero <$> filter (/= 0) (shrink x)

instance Eq1 NonZero where
  liftEq eq (NonZero a) (NonZero b) = a `eq` b

instance Ord1 NonZero where
  liftCompare cmp (NonZero a) (NonZero b) = a `cmp` b

instance Show1 NonZero where
  liftShowsPrec shw _ p (NonZero a) = shw p a

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
  liftEq eq (Huge a) (Huge b) = a `eq` b

instance Ord1 Huge where
  liftCompare cmp (Huge a) (Huge b) = a `cmp` b

instance Show1 Huge where
  liftShowsPrec shw _ p (Huge a) = shw p a

-------------------------------------------------------------------------------
-- Power

newtype Power a = Power { getPower :: a }
  deriving (Eq, Ord, Read, Show, Num, Enum, Bounded, Integral, Real, Functor, Foldable, Traversable, Semiring, GcdDomain, Euclidean)

instance (Monad m, Num a, Ord a, Serial m a) => Serial m (Power a) where
  series = Power <$> series `suchThatSerial` (> 0)

instance (Num a, Ord a, Integral a, Arbitrary a) => Arbitrary (Power a) where
  arbitrary = Power <$> arbitrarySizedNatural `suchThat` (> 0)
  shrink (Power x) = Power <$> filter (> 0) (shrink x)

instance Eq1 Power where
  liftEq eq (Power a) (Power b) = a `eq` b

instance Ord1 Power where
  liftCompare cmp (Power a) (Power b) = a `cmp` b

instance Show1 Power where
  liftShowsPrec shw _ p (Power a) = shw p a

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
  liftEq eq (Odd a) (Odd b) = a `eq` b

instance Ord1 Odd where
  liftCompare cmp (Odd a) (Odd b) = a `cmp` b

instance Show1 Odd where
  liftShowsPrec shw _ p (Odd a) = shw p a

-------------------------------------------------------------------------------
-- Prime

instance (Arbitrary a, UniqueFactorisation a) => Arbitrary (Prime a) where
  arbitrary = (arbitrary :: Gen a) `suchThatMap` isPrime

instance (Monad m, Serial m a, UniqueFactorisation a) => Serial m (Prime a) where
  series = (series :: Series m a) `suchThatMapSerial` isPrime

-------------------------------------------------------------------------------
-- UniqueFactorisation

instance UniqueFactorisation a => UniqueFactorisation (Large a) where
  factorise (Large x) = coerce $ factorise x
  isPrime (Large x) = coerce $ isPrime x

instance UniqueFactorisation a => UniqueFactorisation (Huge a) where
  factorise (Huge x) = coerce $ factorise x
  isPrime (Huge x) = coerce $ isPrime x

-------------------------------------------------------------------------------
-- Utils

suchThatSerial :: Series m a -> (a -> Bool) -> Series m a
suchThatSerial s p = s >>= \x -> if p x then pure x else empty

suchThatMapSerial :: Series m a -> (a -> Maybe b) -> Series m b
suchThatMapSerial s p = s >>= maybe empty pure . p
