-- |
-- Module:      Math.NumberTheory.TestUtils
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Utils to test Math.NumberTheory
--

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.TestUtils
  ( module Math.NumberTheory.TestUtils.Wrappers
  , module Math.NumberTheory.TestUtils.MyCompose
  , module Test.SmallCheck.Series
  , Large(..)
  , NonZero(..)
  , testIntegralProperty
  , testIntegralPropertyNoLarge
  , testSameIntegralProperty
  , testSameIntegralProperty3
  , testIntegral2Property
  , testSmallAndQuick
  , testEqualSmallAndQuick

  -- * Export for @Zeta@ tests
  , assertEqualUpToEps

  , lawsToTest
  ) where

import Test.QuickCheck.Classes
import Test.SmallCheck.Series (Positive(..), NonNegative(..), Serial(..), Series, generate, (\/), cons2)
import Test.Tasty
import Test.Tasty.HUnit       (Assertion, assertBool)
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC hiding (Positive, getPositive, NonNegative, generate, getNonNegative)

import Data.Bits
import Data.Euclidean
import Data.Mod
import Data.Semiring (Semiring)
import GHC.Exts
import GHC.TypeNats.Compat
import Numeric.Natural

import qualified Math.NumberTheory.Quadratic.EisensteinIntegers as E (EisensteinInteger(..))
import Math.NumberTheory.Quadratic.GaussianIntegers (GaussianInteger(..))
import Math.NumberTheory.Primes (Prime, UniqueFactorisation)
import qualified Math.NumberTheory.SmoothNumbers as SN

import Math.NumberTheory.TestUtils.MyCompose
import Math.NumberTheory.TestUtils.Wrappers

instance Arbitrary Natural where
  arbitrary = fromInteger <$> (arbitrary `suchThat` (>= 0))
  shrink = map fromInteger . filter (>= 0) . shrink . toInteger

instance Arbitrary E.EisensteinInteger where
  arbitrary = (E.:+) <$> arbitrary <*> arbitrary
  shrink (x E.:+ y) = map (x E.:+) (shrink y) ++ map (E.:+ y) (shrink x)

instance Monad m => Serial m E.EisensteinInteger where
  series = cons2 (E.:+)

instance Arbitrary GaussianInteger where
  arbitrary = (:+) <$> arbitrary <*> arbitrary
  shrink (x :+ y) = map (x :+) (shrink y) ++ map (:+ y) (shrink x)

instance Monad m => Serial m GaussianInteger where
  series = cons2 (:+)

-------------------------------------------------------------------------------
-- SmoothNumbers

instance (Ord a, Num a, Euclidean a, Arbitrary a) => Arbitrary (SN.SmoothBasis a) where
  arbitrary = SN.fromList <$> arbitrary
  shrink xs = SN.fromList <$> shrink (SN.unSmoothBasis xs)

instance (Ord a, Num a, Euclidean a, Serial m a) => Serial m (SN.SmoothBasis a) where
  series = SN.fromList <$> series

-------------------------------------------------------------------------------
-- Mod

instance KnownNat m => Arbitrary (Mod m) where
  arbitrary = oneof [arbitraryBoundedEnum, fromInteger <$> arbitrary]

-------------------------------------------------------------------------------

-- https://www.cs.ox.ac.uk/projects/utgp/school/andres.pdf, p. 21
-- :k Compose = (k1 -> Constraint) -> (k2 -> k1) -> (k2 -> Constraint)
class    (f (g x)) => (f `Compose` g) x
instance (f (g x)) => (f `Compose` g) x

type family ConcatMap (w :: * -> Constraint) (cs :: [*]) :: Constraint
  where
    ConcatMap w '[] = ()
    ConcatMap w (c ': cs) = (w c, ConcatMap w cs)

type family Matrix (as :: [* -> Constraint]) (w :: * -> *) (bs :: [*]) :: Constraint
  where
    Matrix '[] w bs = ()
    Matrix (a ': as) w bs = (ConcatMap (a `Compose` w) bs, Matrix as w bs)

type TestableIntegral wrapper =
  ( Matrix '[Arbitrary, Show, Serial IO] wrapper '[Int, Word, Integer, Natural]
  , Matrix '[Arbitrary, Show] wrapper '[Large Int, Large Word, Huge Integer, Huge Natural]
  , Matrix '[Bounded, Integral] wrapper '[Int, Word]
  , Num (wrapper Integer)
  , Num (wrapper Natural)
  , Functor wrapper
  )

testIntegralProperty
  :: forall wrapper bool. (TestableIntegral wrapper, SC.Testable IO bool, QC.Testable bool)
  => String -> (forall a. (GcdDomain a, Euclidean a, Semiring a, Integral a, Bits a, UniqueFactorisation a, Show a) => wrapper a -> bool) -> TestTree
testIntegralProperty name f = testGroup name
  [ SC.testProperty "smallcheck Int"     (f :: wrapper Int     -> bool)
  , SC.testProperty "smallcheck Word"    (f :: wrapper Word    -> bool)
  , SC.testProperty "smallcheck Integer" (f :: wrapper Integer -> bool)
  , SC.testProperty "smallcheck Natural" (f :: wrapper Natural -> bool)
  , QC.testProperty "quickcheck Int"     (f :: wrapper Int     -> bool)
  , QC.testProperty "quickcheck Word"    (f :: wrapper Word    -> bool)
  , QC.testProperty "quickcheck Integer" (f :: wrapper Integer -> bool)
  , QC.testProperty "quickcheck Natural" (f :: wrapper Natural -> bool)
  , QC.testProperty "quickcheck Large Int"     ((f :: wrapper Int     -> bool) . getLarge)
  , QC.testProperty "quickcheck Large Word"    ((f :: wrapper Word    -> bool) . getLarge)
  , QC.testProperty "quickcheck Huge  Integer" ((f :: wrapper Integer -> bool) . getHuge)
  , QC.testProperty "quickcheck Huge  Natural" ((f :: wrapper Natural -> bool) . getHuge)
  ]

testIntegralPropertyNoLarge
  :: forall wrapper bool. (TestableIntegral wrapper, SC.Testable IO bool, QC.Testable bool)
  => String -> (forall a. (Euclidean a, Semiring a, Integral a, Bits a, UniqueFactorisation a, Show a, Enum (Prime a)) => wrapper a -> bool) -> TestTree
testIntegralPropertyNoLarge name f = testGroup name
  [ SC.testProperty "smallcheck Int"     (f :: wrapper Int     -> bool)
  , SC.testProperty "smallcheck Word"    (f :: wrapper Word    -> bool)
  , SC.testProperty "smallcheck Integer" (f :: wrapper Integer -> bool)
  , SC.testProperty "smallcheck Natural" (f :: wrapper Natural -> bool)
  , QC.testProperty "quickcheck Int"     (f :: wrapper Int     -> bool)
  , QC.testProperty "quickcheck Word"    (f :: wrapper Word    -> bool)
  , QC.testProperty "quickcheck Integer" (f :: wrapper Integer -> bool)
  , QC.testProperty "quickcheck Natural" (f :: wrapper Natural -> bool)
  ]

testSameIntegralProperty
  :: forall wrapper1 wrapper2 bool. (TestableIntegral wrapper1, TestableIntegral wrapper2, SC.Testable IO bool, QC.Testable bool)
  => String -> (forall a. (GcdDomain a, Euclidean a, Integral a, Bits a, UniqueFactorisation a, Show a) => wrapper1 a -> wrapper2 a -> bool) -> TestTree
testSameIntegralProperty name f = testGroup name
  [ SC.testProperty "smallcheck Int"     (f :: wrapper1 Int     -> wrapper2 Int     -> bool)
  , SC.testProperty "smallcheck Word"    (f :: wrapper1 Word    -> wrapper2 Word    -> bool)
  , SC.testProperty "smallcheck Integer" (f :: wrapper1 Integer -> wrapper2 Integer -> bool)
  , SC.testProperty "smallcheck Natural" (f :: wrapper1 Natural -> wrapper2 Natural -> bool)
  , QC.testProperty "quickcheck Int"     (f :: wrapper1 Int     -> wrapper2 Int     -> bool)
  , QC.testProperty "quickcheck Word"    (f :: wrapper1 Word    -> wrapper2 Word    -> bool)
  , QC.testProperty "quickcheck Integer" (f :: wrapper1 Integer -> wrapper2 Integer -> bool)
  , QC.testProperty "quickcheck Natural" (f :: wrapper1 Natural -> wrapper2 Natural -> bool)
  , QC.testProperty "quickcheck Large Int"     (\a b -> (f :: wrapper1 Int     -> wrapper2 Int     -> bool) (getLarge <$> a) (getLarge <$> b))
  , QC.testProperty "quickcheck Large Word"    (\a b -> (f :: wrapper1 Word    -> wrapper2 Word    -> bool) (getLarge <$> a) (getLarge <$> b))
  , QC.testProperty "quickcheck Huge  Integer" (\a b -> (f :: wrapper1 Integer -> wrapper2 Integer -> bool) (getHuge  <$> a) (getHuge  <$> b))
  , QC.testProperty "quickcheck Huge  Natural" (\a b -> (f :: wrapper1 Natural -> wrapper2 Natural -> bool) (getHuge  <$> a) (getHuge  <$> b))
  ]

testSameIntegralProperty3
  :: forall wrapper1 wrapper2 wrapper3 bool. (TestableIntegral wrapper1, TestableIntegral wrapper2, TestableIntegral wrapper3, SC.Testable IO bool, QC.Testable bool)
  => String -> (forall a. (Euclidean a, Integral a, Bits a, UniqueFactorisation a, Show a) => wrapper1 a -> wrapper2 a -> wrapper3 a -> bool) -> TestTree
testSameIntegralProperty3 name f = testGroup name
  [ SC.testProperty "smallcheck Int"     (f :: wrapper1 Int     -> wrapper2 Int     -> wrapper3 Int     -> bool)
  , SC.testProperty "smallcheck Word"    (f :: wrapper1 Word    -> wrapper2 Word    -> wrapper3 Word    -> bool)
  , SC.testProperty "smallcheck Integer" (f :: wrapper1 Integer -> wrapper2 Integer -> wrapper3 Integer -> bool)
  , SC.testProperty "smallcheck Natural" (f :: wrapper1 Natural -> wrapper2 Natural -> wrapper3 Natural -> bool)
  , QC.testProperty "quickcheck Int"     (f :: wrapper1 Int     -> wrapper2 Int     -> wrapper3 Int     -> bool)
  , QC.testProperty "quickcheck Word"    (f :: wrapper1 Word    -> wrapper2 Word    -> wrapper3 Word    -> bool)
  , QC.testProperty "quickcheck Integer" (f :: wrapper1 Integer -> wrapper2 Integer -> wrapper3 Integer -> bool)
  , QC.testProperty "quickcheck Natural" (f :: wrapper1 Natural -> wrapper2 Natural -> wrapper3 Natural -> bool)
  , QC.testProperty "quickcheck Large Int"     (\a b c -> (f :: wrapper1 Int     -> wrapper2 Int     -> wrapper3 Int     -> bool) (getLarge <$> a) (getLarge <$> b) (getLarge <$> c))
  , QC.testProperty "quickcheck Large Word"    (\a b c -> (f :: wrapper1 Word    -> wrapper2 Word    -> wrapper3 Word    -> bool) (getLarge <$> a) (getLarge <$> b) (getLarge <$> c))
  , QC.testProperty "quickcheck Huge  Integer" (\a b c -> (f :: wrapper1 Integer -> wrapper2 Integer -> wrapper3 Integer -> bool) (getHuge  <$> a) (getHuge  <$> b) (getHuge  <$> c))
  , QC.testProperty "quickcheck Huge  Natural" (\a b c -> (f :: wrapper1 Natural -> wrapper2 Natural -> wrapper3 Natural -> bool) (getHuge  <$> a) (getHuge  <$> b) (getHuge  <$> c))
  ]

testIntegral2Property
  :: forall wrapper1 wrapper2 bool. (TestableIntegral wrapper1, TestableIntegral wrapper2, SC.Testable IO bool, QC.Testable bool)
  => String -> (forall a1 a2. (Integral a1, Integral a2, Bits a1, Bits a2, UniqueFactorisation a1, UniqueFactorisation a2, Show a1, Show a2) => wrapper1 a1 -> wrapper2 a2 -> bool) -> TestTree
testIntegral2Property name f = testGroup name
  [ SC.testProperty "smallcheck Int Int"         (f :: wrapper1 Int     -> wrapper2 Int     -> bool)
  , SC.testProperty "smallcheck Int Word"        (f :: wrapper1 Int     -> wrapper2 Word    -> bool)
  , SC.testProperty "smallcheck Int Integer"     (f :: wrapper1 Int     -> wrapper2 Integer -> bool)
  , SC.testProperty "smallcheck Int Natural"     (f :: wrapper1 Int     -> wrapper2 Natural -> bool)
  , SC.testProperty "smallcheck Word Int"        (f :: wrapper1 Word    -> wrapper2 Int     -> bool)
  , SC.testProperty "smallcheck Word Word"       (f :: wrapper1 Word    -> wrapper2 Word    -> bool)
  , SC.testProperty "smallcheck Word Integer"    (f :: wrapper1 Word    -> wrapper2 Integer -> bool)
  , SC.testProperty "smallcheck Word Natural"    (f :: wrapper1 Word    -> wrapper2 Natural -> bool)
  , SC.testProperty "smallcheck Integer Int"     (f :: wrapper1 Integer -> wrapper2 Int     -> bool)
  , SC.testProperty "smallcheck Integer Word"    (f :: wrapper1 Integer -> wrapper2 Word    -> bool)
  , SC.testProperty "smallcheck Integer Integer" (f :: wrapper1 Integer -> wrapper2 Integer -> bool)
  , SC.testProperty "smallcheck Integer Natural" (f :: wrapper1 Integer -> wrapper2 Natural -> bool)
  , SC.testProperty "smallcheck Natural Int"     (f :: wrapper1 Natural -> wrapper2 Int     -> bool)
  , SC.testProperty "smallcheck Natural Word"    (f :: wrapper1 Natural -> wrapper2 Word    -> bool)
  , SC.testProperty "smallcheck Natural Integer" (f :: wrapper1 Natural -> wrapper2 Integer -> bool)
  , SC.testProperty "smallcheck Natural Natural" (f :: wrapper1 Natural -> wrapper2 Natural -> bool)

  , QC.testProperty "quickcheck Int Int"         (f :: wrapper1 Int     -> wrapper2 Int     -> bool)
  , QC.testProperty "quickcheck Int Word"        (f :: wrapper1 Int     -> wrapper2 Word    -> bool)
  , QC.testProperty "quickcheck Int Integer"     (f :: wrapper1 Int     -> wrapper2 Integer -> bool)
  , QC.testProperty "quickcheck Int Natural"     (f :: wrapper1 Int     -> wrapper2 Natural -> bool)
  , QC.testProperty "quickcheck Word Int"        (f :: wrapper1 Word    -> wrapper2 Int     -> bool)
  , QC.testProperty "quickcheck Word Word"       (f :: wrapper1 Word    -> wrapper2 Word    -> bool)
  , QC.testProperty "quickcheck Word Integer"    (f :: wrapper1 Word    -> wrapper2 Integer -> bool)
  , QC.testProperty "quickcheck Word Natural"    (f :: wrapper1 Word    -> wrapper2 Natural -> bool)
  , QC.testProperty "quickcheck Integer Int"     (f :: wrapper1 Integer -> wrapper2 Int     -> bool)
  , QC.testProperty "quickcheck Integer Word"    (f :: wrapper1 Integer -> wrapper2 Word    -> bool)
  , QC.testProperty "quickcheck Integer Integer" (f :: wrapper1 Integer -> wrapper2 Integer -> bool)
  , QC.testProperty "quickcheck Integer Natural" (f :: wrapper1 Integer -> wrapper2 Natural -> bool)
  , QC.testProperty "quickcheck Natural Int"     (f :: wrapper1 Natural -> wrapper2 Int     -> bool)
  , QC.testProperty "quickcheck Natural Word"    (f :: wrapper1 Natural -> wrapper2 Word    -> bool)
  , QC.testProperty "quickcheck Natural Integer" (f :: wrapper1 Natural -> wrapper2 Integer -> bool)
  , QC.testProperty "quickcheck Natural Natural" (f :: wrapper1 Natural -> wrapper2 Natural -> bool)

  , QC.testProperty "quickcheck Large Int Int"         ((f :: wrapper1 Int     -> wrapper2 Int     -> bool) . fmap getLarge)
  , QC.testProperty "quickcheck Large Int Word"        ((f :: wrapper1 Int     -> wrapper2 Word    -> bool) . fmap getLarge)
  , QC.testProperty "quickcheck Large Int Integer"     ((f :: wrapper1 Int     -> wrapper2 Integer -> bool) . fmap getLarge)
  , QC.testProperty "quickcheck Large Int Natural"     ((f :: wrapper1 Int     -> wrapper2 Natural -> bool) . fmap getLarge)
  , QC.testProperty "quickcheck Large Word Int"        ((f :: wrapper1 Word    -> wrapper2 Int     -> bool) . fmap getLarge)
  , QC.testProperty "quickcheck Large Word Word"       ((f :: wrapper1 Word    -> wrapper2 Word    -> bool) . fmap getLarge)
  , QC.testProperty "quickcheck Large Word Integer"    ((f :: wrapper1 Word    -> wrapper2 Integer -> bool) . fmap getLarge)
  , QC.testProperty "quickcheck Large Word Natural"    ((f :: wrapper1 Word    -> wrapper2 Natural -> bool) . fmap getLarge)
  , QC.testProperty "quickcheck Huge  Integer Int"     ((f :: wrapper1 Integer -> wrapper2 Int     -> bool) . fmap getHuge)
  , QC.testProperty "quickcheck Huge  Integer Word"    ((f :: wrapper1 Integer -> wrapper2 Word    -> bool) . fmap getHuge)
  , QC.testProperty "quickcheck Huge  Integer Integer" ((f :: wrapper1 Integer -> wrapper2 Integer -> bool) . fmap getHuge)
  , QC.testProperty "quickcheck Huge  Integer Natural" ((f :: wrapper1 Integer -> wrapper2 Natural -> bool) . fmap getHuge)
  , QC.testProperty "quickcheck Huge  Natural Int"     ((f :: wrapper1 Natural -> wrapper2 Int     -> bool) . fmap getHuge)
  , QC.testProperty "quickcheck Huge  Natural Word"    ((f :: wrapper1 Natural -> wrapper2 Word    -> bool) . fmap getHuge)
  , QC.testProperty "quickcheck Huge  Natural Integer" ((f :: wrapper1 Natural -> wrapper2 Integer -> bool) . fmap getHuge)
  , QC.testProperty "quickcheck Huge  Natural Natural" ((f :: wrapper1 Natural -> wrapper2 Natural -> bool) . fmap getHuge)
  ]

testSmallAndQuick
  :: (SC.Testable IO a, QC.Testable a)
  => String
  -> a
  -> TestTree
testSmallAndQuick name f = testGroup name
  [ SC.testProperty "smallcheck" f
  , QC.testProperty "quickcheck" f
  ]

testEqualSmallAndQuick
  :: (Serial IO a, Arbitrary a, Show a, Eq b, Show b)
  => String
  -> (a -> (b, b))
  -> TestTree
testEqualSmallAndQuick name f = testGroup name
  [ SC.testProperty "smallcheck" (uncurry (==)  . f)
  , QC.testProperty "quickcheck" (uncurry (===) . f)
  ]

-- | Used in @Math.NumberTheory.Zeta.DirichletTests@ and
-- @Math.NumberTheory.Zeta.RiemannTests@.
assertEqualUpToEps :: String -> Double -> Double -> Double -> Assertion
assertEqualUpToEps msg eps expected actual
  = assertBool msg (abs (expected - actual) < eps)

lawsToTest :: Laws -> TestTree
lawsToTest (Laws name props) =
  testGroup name $ map (uncurry QC.testProperty) props
