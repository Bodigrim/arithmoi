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
  , module Test.SmallCheck.Series
  , Large(..)
  , testIntegralProperty
  , testSameIntegralProperty
  , testIntegral2Property
  , testSmallAndQuick
  ) where

import Test.SmallCheck.Series (cons2)
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC hiding (Positive, NonNegative, generate, getNonNegative)

import Test.SmallCheck.Series (Positive(..), NonNegative(..), Serial(..), Series, generate)

import Control.Applicative
import Data.Bits
#if !(MIN_VERSION_base(4,8,0))
import Data.Word
#endif
import GHC.Exts
import Numeric.Natural

import Math.NumberTheory.GaussianIntegers (GaussianInteger(..))

import Math.NumberTheory.TestUtils.Compose ()
import Math.NumberTheory.TestUtils.Wrappers

instance Monad m => Serial m Word where
  series =
    generate (\d -> if d >= 0 then pure 0 else empty) <|> nats
    where
      nats = generate $ \d -> if d > 0 then [1 .. fromInteger (toInteger d)] else empty

#if !(MIN_VERSION_base(4,8,0)) && !(MIN_VERSION_QuickCheck(2,9,0))
instance Arbitrary Natural where
  arbitrary = fromInteger <$> (arbitrary `suchThat` (>= 0))
  shrink = map fromInteger . filter (>= 0) . shrink . toInteger
#endif

instance Monad m => Serial m Natural where
  series =
    generate (\d -> if d >= 0 then pure 0 else empty) <|> nats
    where
      nats = generate $ \d -> if d > 0 then [1 .. fromInteger (toInteger d)] else empty

instance Arbitrary GaussianInteger where
  arbitrary = (:+) <$> arbitrary <*> arbitrary
  shrink (x :+ y) = (:+) <$> shrink x <*> shrink y

instance Monad m => Serial m GaussianInteger where
  series = cons2 (:+)

-- https://www.cs.ox.ac.uk/projects/utgp/school/andres.pdf, p. 21
-- :k Compose = (k1 -> Constraint) -> (k2 -> k1) -> (k2 -> Constraint)
class    (f (g x)) => (f `Compose` g) x
instance (f (g x)) => (f `Compose` g) x

type family ConcatMap (w :: * -> Constraint) (cs :: [*]) :: Constraint
#if __GLASGOW_HASKELL__ >= 708
  where
    ConcatMap w '[] = ()
    ConcatMap w (c ': cs) = (w c, ConcatMap w cs)
#else
type instance ConcatMap w '[] = ()
type instance ConcatMap w (c ': cs) = (w c, ConcatMap w cs)
#endif

type family Matrix (as :: [* -> Constraint]) (w :: * -> *) (bs :: [*]) :: Constraint
#if __GLASGOW_HASKELL__ >= 708
  where
    Matrix '[] w bs = ()
    Matrix (a ': as) w bs = (ConcatMap (a `Compose` w) bs, Matrix as w bs)
#else
type instance Matrix '[] w bs = ()
type instance Matrix (a ': as) w bs = (ConcatMap (a `Compose` w) bs, Matrix as w bs)
#endif

type TestableIntegral wrapper =
  ( Matrix '[Arbitrary, Show, Serial IO] wrapper '[Int, Word, Integer]
  , Matrix '[Arbitrary, Show] wrapper '[Large Int, Large Word, Huge Integer]
  , Matrix '[Bounded, Integral] wrapper '[Int, Word]
  , Num (wrapper Integer)
  , Functor wrapper
  )


testIntegralProperty
  :: forall wrapper bool. (TestableIntegral wrapper, SC.Testable IO bool, QC.Testable bool)
  => String -> (forall a. (Integral a, Bits a) => wrapper a -> bool) -> TestTree
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
  => String -> (forall a. (Integral a, Bits a) => wrapper1 a -> wrapper2 a -> bool) -> TestTree
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
  => String -> (forall a1 a2. (Integral a1, Integral a2, Bits a1, Bits a2) => wrapper1 a1 -> wrapper2 a2 -> bool) -> TestTree
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
