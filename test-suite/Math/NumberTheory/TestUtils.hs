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
  , testSameIntegralProperty
  , testIntegral2Property
  , testSmallAndQuick

  -- * Export for @Zeta@ tests
  , assertEqualUpToEps
  ) where

import Test.SmallCheck.Series (cons2)
import Test.Tasty
import Test.Tasty.HUnit       (Assertion, assertBool)
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC hiding (Positive, getPositive, NonNegative, generate, getNonNegative)

import Test.SmallCheck.Series (Positive(..), NonNegative(..), Serial(..), Series, generate, (\/))

import Data.Bits
import GHC.Exts
import Numeric.Natural

import Math.NumberTheory.Euclidean
import qualified Math.NumberTheory.Quadratic.EisensteinIntegers as E (EisensteinInteger(..))
import Math.NumberTheory.Quadratic.GaussianIntegers (GaussianInteger(..))
import Math.NumberTheory.Moduli.PrimitiveRoot (CyclicGroup(..))
import Math.NumberTheory.Primes (UniqueFactorisation, Prime, unPrime)
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
-- Cyclic group

instance (Eq a, Num a, UniqueFactorisation a, Arbitrary a) => Arbitrary (CyclicGroup a) where
  arbitrary = frequency
    [ (1, pure CG2)
    , (1, pure CG4)
    , (9, CGOddPrimePower
      <$> (arbitrary :: Gen (Prime a)) `suchThatMap` isOddPrime
      <*> (getPower <$> arbitrary))
    , (9, CGDoubleOddPrimePower
      <$> (arbitrary :: Gen (Prime a)) `suchThatMap` isOddPrime
      <*> (getPower <$> arbitrary))
    ]

instance (Monad m, Eq a, Num a, UniqueFactorisation a, Serial m a) => Serial m (CyclicGroup a) where
  series = pure CG2
        \/ pure CG4
        \/ (CGOddPrimePower
           <$> (series :: Series m (Prime a)) `suchThatMapSerial` isOddPrime
           <*> (getPower <$> series))
        \/ (CGDoubleOddPrimePower
           <$> (series :: Series m (Prime a)) `suchThatMapSerial` isOddPrime
           <*> (getPower <$> series))

isOddPrime
  :: forall a. (Eq a, Num a, UniqueFactorisation a)
  => Prime a
  -> Maybe (Prime a)
isOddPrime p = if (unPrime p :: a) == 2 then Nothing else Just p

-------------------------------------------------------------------------------
-- SmoothNumbers

instance (Ord a, Euclidean a, Arbitrary a) => Arbitrary (SN.SmoothBasis a) where
  arbitrary = (fmap getPositive <$> arbitrary) `suchThatMap` SN.fromList

instance (Ord a, Euclidean a, Serial m a) => Serial m (SN.SmoothBasis a) where
  series = (fmap getPositive <$> series) `suchThatMapSerial` SN.fromList

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
  => String -> (forall a. (Euclidean a, Integral a, Bits a, UniqueFactorisation a, Show a) => wrapper a -> bool) -> TestTree
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

testSameIntegralProperty
  :: forall wrapper1 wrapper2 bool. (TestableIntegral wrapper1, TestableIntegral wrapper2, SC.Testable IO bool, QC.Testable bool)
  => String -> (forall a. (Euclidean a, Integral a, Bits a, UniqueFactorisation a, Show a) => wrapper1 a -> wrapper2 a -> bool) -> TestTree
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
  , QC.testProperty "quickcheck Huge  integer" (\a b -> (f :: wrapper1 Integer -> wrapper2 Integer -> bool) (getHuge  <$> a) (getHuge  <$> b))
  , QC.testProperty "quickcheck Huge  Natural" (\a b -> (f :: wrapper1 Natural -> wrapper2 Natural -> bool) (getHuge  <$> a) (getHuge  <$> b))
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
  :: SC.Testable IO a
  => QC.Testable a
  => String -> a -> TestTree
testSmallAndQuick name f = testGroup name
  [ SC.testProperty "smallcheck" f
  , QC.testProperty "quickcheck" f
  ]


-- | Used in @Math.NumberTheory.Zeta.DirichletTests@ and
-- @Math.NumberTheory.Zeta.RiemannTests@.
assertEqualUpToEps :: String -> Double -> Double -> Double -> Assertion
assertEqualUpToEps msg eps expected actual
  = assertBool msg (abs (expected - actual) < eps)
