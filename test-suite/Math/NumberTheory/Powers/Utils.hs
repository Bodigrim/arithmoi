-- |
-- Module:      Math.NumberTheory.Powers.CubesTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Utils to test Math.NumberTheory.Powers
--

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Powers.Utils where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC hiding (Positive, NonNegative, generate, getNonNegative)

import Test.SmallCheck.Series

import Control.Applicative
import Data.Functor.Identity
#if MIN_VERSION_base(4,8,0)
#else
import Data.Word
#endif

instance (Num a, Ord a, Arbitrary a) => Arbitrary (Positive a) where
  arbitrary = Positive <$> (arbitrary `suchThat` (> 0))
  shrink (Positive x) = Positive <$> filter (> 0) (shrink x)

instance (Num a, Ord a, Arbitrary a) => Arbitrary (NonNegative a) where
  arbitrary = NonNegative <$> (arbitrary `suchThat` (>= 0))
  shrink (NonNegative x) = NonNegative <$> filter (>= 0) (shrink x)

instance (Num a, Bounded a) => Bounded (Positive a) where
  minBound = Positive 1
  maxBound = Positive (maxBound :: a)

instance (Num a, Bounded a) => Bounded (NonNegative a) where
  minBound = NonNegative 0
  maxBound = NonNegative (maxBound :: a)

instance Enum a => Enum (Identity a) where
  toEnum a = Identity $ toEnum a
  fromEnum (Identity a) = fromEnum a

instance Bounded a => Bounded (Identity a) where
  minBound = Identity (minBound :: a)
  maxBound = Identity (maxBound :: a)

instance Integral a => Integral (Identity a) where
  toInteger (Identity a) = toInteger a
  quotRem (Identity a) (Identity b) = (Identity q, Identity r)
    where
      (q, r) = quotRem a b

instance Real a => Real (Identity a) where
  toRational (Identity a) = toRational a

instance Num a => Num (Identity a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  fromInteger a = Identity $ fromInteger a
  abs = liftA abs
  signum = liftA signum
  negate = liftA negate

newtype Huge a = Huge { getHuge :: a }
  deriving (Eq, Ord, Enum, Bounded, Show, Num, Real, Integral)

instance (Num a, Arbitrary a) => Arbitrary (Huge a) where
  arbitrary = do
    Positive l <- arbitrary
    ds <- vector l
    return $ Huge $ foldl1 (\acc n -> acc * 2^63 + n) ds

newtype Power a = Power { getPower :: a }
  deriving (Eq, Ord, Enum, Bounded, Show, Num, Real, Integral)

instance (Monad m, Num a, Ord a, Serial m a) => Serial m (Power a) where
  series = Power <$> series `suchThatSerial` (> 0)

instance (Num a, Ord a, Arbitrary (Small a)) => Arbitrary (Power a) where
  arbitrary = Power <$> (getSmall <$> arbitrary) `suchThat` (> 0)

instance Monad m => Serial m Word where
  series =
    generate (\d -> if d >= 0 then pure 0 else empty) <|> nats
    where
      nats = generate $ \d -> if d >0 then [1 .. fromInteger (toInteger d)] else empty

instance (Monad m, Serial m a) => Serial m (Identity a) where
  series = Identity <$> series

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary
  shrink (Identity x) = Identity <$> shrink x

suchThatSerial :: Series m a -> (a -> Bool) -> Series m a
suchThatSerial s p = s >>= \x -> if p x then pure x else empty

testIntegralProperty
  :: forall wrapper.
     (Arbitrary (wrapper Int), Arbitrary (wrapper Word), Arbitrary (wrapper Integer))
  => (Arbitrary (Large (wrapper Int)), Arbitrary (Large (wrapper Word)), Arbitrary (Huge (wrapper Integer)))
  => (Show (wrapper Int), Show (wrapper Word), Show (wrapper Integer))
  => (Serial IO (wrapper Int), Serial IO (wrapper Word), Serial IO (wrapper Integer))
  => String -> (forall a. (Integral a) => wrapper a -> Bool) -> TestTree
testIntegralProperty name f = testGroup name
  [ SC.testProperty "smallcheck Int"     (f :: wrapper Int     -> Bool)
  , SC.testProperty "smallcheck Word"    (f :: wrapper Word    -> Bool)
  , SC.testProperty "smallcheck Integer" (f :: wrapper Integer -> Bool)
  , QC.testProperty "quickcheck Int"     (f :: wrapper Int     -> Bool)
  , QC.testProperty "quickcheck Word"    (f :: wrapper Word    -> Bool)
  , QC.testProperty "quickcheck Integer" (f :: wrapper Integer -> Bool)
  , QC.testProperty "quickcheck Large Int"     ((f :: wrapper Int     -> Bool) . getLarge)
  , QC.testProperty "quickcheck Large Word"    ((f :: wrapper Word    -> Bool) . getLarge)
  , QC.testProperty "quickcheck Huge  Integer" ((f :: wrapper Integer -> Bool) . getHuge)
  ]

testIntegral2Property
  :: forall wrapper1 wrapper2.
     (Arbitrary (wrapper1 Int), Arbitrary (wrapper1 Word), Arbitrary (wrapper1 Integer))
  => (Arbitrary (Large (wrapper1 Int)), Arbitrary (Large (wrapper1 Word)), Arbitrary (Huge (wrapper1 Integer)))
  => (Show (wrapper1 Int), Show (wrapper1 Word), Show (wrapper1 Integer))
  => (Serial IO (wrapper1 Int), Serial IO (wrapper1 Word), Serial IO (wrapper1 Integer))
  => (Arbitrary (wrapper2 Int), Arbitrary (wrapper2 Word), Arbitrary (wrapper2 Integer))
  => (Arbitrary (Large (wrapper2 Int)), Arbitrary (Large (wrapper2 Word)), Arbitrary (Huge (wrapper2 Integer)))
  => (Show (wrapper2 Int), Show (wrapper2 Word), Show (wrapper2 Integer))
  => (Serial IO (wrapper2 Int), Serial IO (wrapper2 Word), Serial IO (wrapper2 Integer))
  => String -> (forall a1 a2. (Integral a1, Integral a2) => wrapper1 a1 -> wrapper2 a2 -> Bool) -> TestTree
testIntegral2Property name f = testGroup name
  [ SC.testProperty "smallcheck Int Int"         (f :: wrapper1 Int     -> wrapper2 Int     -> Bool)
  , SC.testProperty "smallcheck Int Word"        (f :: wrapper1 Int     -> wrapper2 Word    -> Bool)
  , SC.testProperty "smallcheck Int Integer"     (f :: wrapper1 Int     -> wrapper2 Integer -> Bool)
  , SC.testProperty "smallcheck Word Int"        (f :: wrapper1 Word    -> wrapper2 Int     -> Bool)
  , SC.testProperty "smallcheck Word Word"       (f :: wrapper1 Word    -> wrapper2 Word    -> Bool)
  , SC.testProperty "smallcheck Word Integer"    (f :: wrapper1 Word    -> wrapper2 Integer -> Bool)
  , SC.testProperty "smallcheck Integer Int"     (f :: wrapper1 Integer -> wrapper2 Int     -> Bool)
  , SC.testProperty "smallcheck Integer Word"    (f :: wrapper1 Integer -> wrapper2 Word    -> Bool)
  , SC.testProperty "smallcheck Integer Integer" (f :: wrapper1 Integer -> wrapper2 Integer -> Bool)

  , QC.testProperty "quickcheck Int Int"         (f :: wrapper1 Int     -> wrapper2 Int     -> Bool)
  , QC.testProperty "quickcheck Int Word"        (f :: wrapper1 Int     -> wrapper2 Word    -> Bool)
  , QC.testProperty "quickcheck Int Integer"     (f :: wrapper1 Int     -> wrapper2 Integer -> Bool)
  , QC.testProperty "quickcheck Word Int"        (f :: wrapper1 Word    -> wrapper2 Int     -> Bool)
  , QC.testProperty "quickcheck Word Word"       (f :: wrapper1 Word    -> wrapper2 Word    -> Bool)
  , QC.testProperty "quickcheck Word Integer"    (f :: wrapper1 Word    -> wrapper2 Integer -> Bool)
  , QC.testProperty "quickcheck Integer Int"     (f :: wrapper1 Integer -> wrapper2 Int     -> Bool)
  , QC.testProperty "quickcheck Integer Word"    (f :: wrapper1 Integer -> wrapper2 Word    -> Bool)
  , QC.testProperty "quickcheck Integer Integer" (f :: wrapper1 Integer -> wrapper2 Integer -> Bool)

  , QC.testProperty "quickcheck Large Int Int"         ((f :: wrapper1 Int     -> wrapper2 Int     -> Bool) . getLarge)
  , QC.testProperty "quickcheck Large Int Word"        ((f :: wrapper1 Int     -> wrapper2 Word    -> Bool) . getLarge)
  , QC.testProperty "quickcheck Large Int Integer"     ((f :: wrapper1 Int     -> wrapper2 Integer -> Bool) . getLarge)
  , QC.testProperty "quickcheck Large Word Int"        ((f :: wrapper1 Word    -> wrapper2 Int     -> Bool) . getLarge)
  , QC.testProperty "quickcheck Large Word Word"       ((f :: wrapper1 Word    -> wrapper2 Word    -> Bool) . getLarge)
  , QC.testProperty "quickcheck Large Word Integer"    ((f :: wrapper1 Word    -> wrapper2 Integer -> Bool) . getLarge)
  , QC.testProperty "quickcheck Huge  Integer Int"     ((f :: wrapper1 Integer -> wrapper2 Int     -> Bool) . getHuge)
  , QC.testProperty "quickcheck Huge  Integer Word"    ((f :: wrapper1 Integer -> wrapper2 Word    -> Bool) . getHuge)
  , QC.testProperty "quickcheck Huge  Integer Integer" ((f :: wrapper1 Integer -> wrapper2 Integer -> Bool) . getHuge)
  ]

testSmallAndQuick
  :: SC.Testable IO a
  => QC.Testable a
  => String -> a -> TestTree
testSmallAndQuick name f = testGroup name
  [ SC.testProperty "smallcheck" f
  , QC.testProperty "quickcheck" f
  ]
