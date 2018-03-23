-- |
-- Module:      Math.NumberTheory.TestUtils
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
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

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses    #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
#endif

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
  ) where

import Test.SmallCheck.Series (cons2)
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC hiding (Positive, NonNegative, generate, getNonNegative)

import Test.SmallCheck.Series (Positive(..), NonNegative(..), Serial(..), Series, generate, (\/))

import Data.Bits
import GHC.Exts
import Numeric.Natural

import Math.NumberTheory.GaussianIntegers (GaussianInteger(..))
import Math.NumberTheory.Moduli.PrimitiveRoot (CyclicGroup(..))
import Math.NumberTheory.UniqueFactorisation (UniqueFactorisation, Prime, unPrime)

import Math.NumberTheory.TestUtils.MyCompose
import Math.NumberTheory.TestUtils.Wrappers

instance Arbitrary Natural where
  arbitrary = fromInteger <$> (arbitrary `suchThat` (>= 0))
  shrink = map fromInteger . filter (>= 0) . shrink . toInteger

instance Arbitrary GaussianInteger where
  arbitrary = (:+) <$> arbitrary <*> arbitrary
  shrink (x :+ y) = (:+) <$> shrink x <*> shrink y

instance Monad m => Serial m GaussianInteger where
  series = cons2 (:+)

-------------------------------------------------------------------------------
-- Cyclic group

instance (Eq a, Num a, UniqueFactorisation a, Arbitrary a) => Arbitrary (CyclicGroup a) where
  arbitrary = frequency
    [ (1, pure CG2)
    , (1, pure CG4)
    , (9, CGOddPrimePower
      <$> (arbitrary :: Gen (PrimeWrapper a)) `suchThatMap` isOddPrime
      <*> (getPower <$> arbitrary))
    , (9, CGDoubleOddPrimePower
      <$> (arbitrary :: Gen (PrimeWrapper a)) `suchThatMap` isOddPrime
      <*> (getPower <$> arbitrary))
    ]

instance (Monad m, Eq a, Num a, UniqueFactorisation a, Serial m a) => Serial m (CyclicGroup a) where
  series = pure CG2
        \/ pure CG4
        \/ (CGOddPrimePower
           <$> (series :: Series m (PrimeWrapper a)) `suchThatMapSerial` isOddPrime
           <*> (getPower <$> series))
        \/ (CGDoubleOddPrimePower
           <$> (series :: Series m (PrimeWrapper a)) `suchThatMapSerial` isOddPrime
           <*> (getPower <$> series))

isOddPrime
  :: forall a. (Eq a, Num a, UniqueFactorisation a)
  => PrimeWrapper a
  -> Maybe (Prime a)
isOddPrime (PrimeWrapper p) = if (unPrime p :: a) == 2 then Nothing else Just p

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
  ( Matrix '[Arbitrary, Show, Serial IO] wrapper '[Int, Word, Integer]
  , Matrix '[Arbitrary, Show] wrapper '[Large Int, Large Word, Huge Integer]
  , Matrix '[Bounded, Integral] wrapper '[Int, Word]
  , Num (wrapper Integer)
  , Functor wrapper
  )


testIntegralProperty
  :: forall wrapper bool. (TestableIntegral wrapper, SC.Testable IO bool, QC.Testable bool)
  => String -> (forall a. (Integral a, Bits a, UniqueFactorisation a, Show a) => wrapper a -> bool) -> TestTree
testIntegralProperty name f = testGroup name
  [ SC.testProperty "smallcheck Int"     (f :: wrapper Int     -> bool)
  , SC.testProperty "smallcheck Word"    (f :: wrapper Word    -> bool)
  , SC.testProperty "smallcheck Integer" (f :: wrapper Integer -> bool)
  , QC.testProperty "quickcheck Int"     (f :: wrapper Int     -> bool)
  , QC.testProperty "quickcheck Word"    (f :: wrapper Word    -> bool)
  , QC.testProperty "quickcheck Integer" (f :: wrapper Integer -> bool)
  , QC.testProperty "quickcheck Large Int"     ((f :: wrapper Int     -> bool) . getLarge)
  , QC.testProperty "quickcheck Large Word"    ((f :: wrapper Word    -> bool) . getLarge)
  , QC.testProperty "quickcheck Huge  Integer" ((f :: wrapper Integer -> bool) . getHuge)
  ]

testSameIntegralProperty
  :: forall wrapper1 wrapper2 bool. (TestableIntegral wrapper1, TestableIntegral wrapper2, SC.Testable IO bool, QC.Testable bool)
  => String -> (forall a. (Integral a, Bits a, UniqueFactorisation a, Show a) => wrapper1 a -> wrapper2 a -> bool) -> TestTree
testSameIntegralProperty name f = testGroup name
  [ SC.testProperty "smallcheck Int"     (f :: wrapper1 Int     -> wrapper2 Int     -> bool)
  , SC.testProperty "smallcheck Word"    (f :: wrapper1 Word    -> wrapper2 Word    -> bool)
  , SC.testProperty "smallcheck Integer" (f :: wrapper1 Integer -> wrapper2 Integer -> bool)
  , QC.testProperty "quickcheck Int"     (f :: wrapper1 Int     -> wrapper2 Int     -> bool)
  , QC.testProperty "quickcheck Word"    (f :: wrapper1 Word    -> wrapper2 Word    -> bool)
  , QC.testProperty "quickcheck Integer" (f :: wrapper1 Integer -> wrapper2 Integer -> bool)
  , QC.testProperty "quickcheck Large Int"     (\a b -> (f :: wrapper1 Int     -> wrapper2 Int     -> bool) (getLarge <$> a) (getLarge <$> b))
  , QC.testProperty "quickcheck Large Word"    (\a b -> (f :: wrapper1 Word    -> wrapper2 Word    -> bool) (getLarge <$> a) (getLarge <$> b))
  , QC.testProperty "quickcheck Huge  Integer" (\a b -> (f :: wrapper1 Integer -> wrapper2 Integer -> bool) (getHuge  <$> a) (getHuge  <$> b))
  ]

testIntegral2Property
  :: forall wrapper1 wrapper2 bool. (TestableIntegral wrapper1, TestableIntegral wrapper2, SC.Testable IO bool, QC.Testable bool)
  => String -> (forall a1 a2. (Integral a1, Integral a2, Bits a1, Bits a2, UniqueFactorisation a1, UniqueFactorisation a2, Show a1, Show a2) => wrapper1 a1 -> wrapper2 a2 -> bool) -> TestTree
testIntegral2Property name f = testGroup name
  [ SC.testProperty "smallcheck Int Int"         (f :: wrapper1 Int     -> wrapper2 Int     -> bool)
  , SC.testProperty "smallcheck Int Word"        (f :: wrapper1 Int     -> wrapper2 Word    -> bool)
  , SC.testProperty "smallcheck Int Integer"     (f :: wrapper1 Int     -> wrapper2 Integer -> bool)
  , SC.testProperty "smallcheck Word Int"        (f :: wrapper1 Word    -> wrapper2 Int     -> bool)
  , SC.testProperty "smallcheck Word Word"       (f :: wrapper1 Word    -> wrapper2 Word    -> bool)
  , SC.testProperty "smallcheck Word Integer"    (f :: wrapper1 Word    -> wrapper2 Integer -> bool)
  , SC.testProperty "smallcheck Integer Int"     (f :: wrapper1 Integer -> wrapper2 Int     -> bool)
  , SC.testProperty "smallcheck Integer Word"    (f :: wrapper1 Integer -> wrapper2 Word    -> bool)
  , SC.testProperty "smallcheck Integer Integer" (f :: wrapper1 Integer -> wrapper2 Integer -> bool)

  , QC.testProperty "quickcheck Int Int"         (f :: wrapper1 Int     -> wrapper2 Int     -> bool)
  , QC.testProperty "quickcheck Int Word"        (f :: wrapper1 Int     -> wrapper2 Word    -> bool)
  , QC.testProperty "quickcheck Int Integer"     (f :: wrapper1 Int     -> wrapper2 Integer -> bool)
  , QC.testProperty "quickcheck Word Int"        (f :: wrapper1 Word    -> wrapper2 Int     -> bool)
  , QC.testProperty "quickcheck Word Word"       (f :: wrapper1 Word    -> wrapper2 Word    -> bool)
  , QC.testProperty "quickcheck Word Integer"    (f :: wrapper1 Word    -> wrapper2 Integer -> bool)
  , QC.testProperty "quickcheck Integer Int"     (f :: wrapper1 Integer -> wrapper2 Int     -> bool)
  , QC.testProperty "quickcheck Integer Word"    (f :: wrapper1 Integer -> wrapper2 Word    -> bool)
  , QC.testProperty "quickcheck Integer Integer" (f :: wrapper1 Integer -> wrapper2 Integer -> bool)

  , QC.testProperty "quickcheck Large Int Int"         ((f :: wrapper1 Int     -> wrapper2 Int     -> bool) . fmap getLarge)
  , QC.testProperty "quickcheck Large Int Word"        ((f :: wrapper1 Int     -> wrapper2 Word    -> bool) . fmap getLarge)
  , QC.testProperty "quickcheck Large Int Integer"     ((f :: wrapper1 Int     -> wrapper2 Integer -> bool) . fmap getLarge)
  , QC.testProperty "quickcheck Large Word Int"        ((f :: wrapper1 Word    -> wrapper2 Int     -> bool) . fmap getLarge)
  , QC.testProperty "quickcheck Large Word Word"       ((f :: wrapper1 Word    -> wrapper2 Word    -> bool) . fmap getLarge)
  , QC.testProperty "quickcheck Large Word Integer"    ((f :: wrapper1 Word    -> wrapper2 Integer -> bool) . fmap getLarge)
  , QC.testProperty "quickcheck Huge  Integer Int"     ((f :: wrapper1 Integer -> wrapper2 Int     -> bool) . fmap getHuge)
  , QC.testProperty "quickcheck Huge  Integer Word"    ((f :: wrapper1 Integer -> wrapper2 Word    -> bool) . fmap getHuge)
  , QC.testProperty "quickcheck Huge  Integer Integer" ((f :: wrapper1 Integer -> wrapper2 Integer -> bool) . fmap getHuge)
  ]

testSmallAndQuick
  :: SC.Testable IO a
  => QC.Testable a
  => String -> a -> TestTree
testSmallAndQuick name f = testGroup name
  [ SC.testProperty "smallcheck" f
  , QC.testProperty "quickcheck" f
  ]
