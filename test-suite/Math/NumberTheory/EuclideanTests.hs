-- |
-- Module:      Math.NumberTheory.EuclideanTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Tests for Math.NumberTheory.Euclidean.Coprimes
--

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-type-defaults  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Math.NumberTheory.EuclideanTests
  ( testSuite
  ) where

import Prelude hiding (gcd)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC hiding (Positive(..), NonZero(..))

import Control.Arrow
import Data.Bits
import Data.Euclidean
import Data.Maybe
import Data.Semigroup
import Data.List (tails, sort)
import Numeric.Natural

import Math.NumberTheory.Euclidean.Coprimes
import Math.NumberTheory.Quadratic.GaussianIntegers
import Math.NumberTheory.TestUtils

-- | Check that numbers are coprime iff their gcd equals to 1.
coprimeProperty :: (Eq a, Num a, GcdDomain a, Euclidean a) => AnySign a -> AnySign a -> Bool
coprimeProperty (AnySign a) (AnySign b) = coprime a b == (gcd a b == 1)

splitIntoCoprimesProperty1
  :: (Eq a, Num a, GcdDomain a)
  => [(a, Power Word)]
  -> Bool
splitIntoCoprimesProperty1 fs' = factorback fs == factorback (unCoprimes $ splitIntoCoprimes fs)
  where
    fs = map (second getPower) fs'
    factorback = abs . product . map (uncurry (^))

splitIntoCoprimesProperty2
  :: (Eq a, Num a, GcdDomain a)
  => [(NonZero a, Power Word)]
  -> Bool
splitIntoCoprimesProperty2 fs' = multiplicities fs <= multiplicities (unCoprimes $ splitIntoCoprimes fs)
  where
    fs = map (getNonZero *** getPower) fs'
    multiplicities = sum . map snd . filter ((/= 1) . abs . fst)

splitIntoCoprimesProperty3
  :: (Eq a, Num a, GcdDomain a)
  => [(a, Power Word)]
  -> Bool
splitIntoCoprimesProperty3 fs' = and [ coprime x y | (x : xs) <- tails fs, y <- xs ]
  where
    fs = map fst $ unCoprimes $ splitIntoCoprimes $ map (second getPower) fs'

-- | Check that evaluation never freezes.
splitIntoCoprimesProperty4
  :: (Eq a, Num a, GcdDomain a)
  => [(a, Word)]
  -> Bool
splitIntoCoprimesProperty4 fs' = fs == fs
  where
    fs = splitIntoCoprimes fs'

splitIntoCoprimesProperty5
  :: (Eq a, Num a, GcdDomain a)
  => [(a, Word)]
  -> Bool
splitIntoCoprimesProperty5 =
  all ((/= 1) . abs . fst) . unCoprimes . splitIntoCoprimes

-- | This is an undefined behaviour, but at least it should not
-- throw exceptions or loop forever.
splitIntoCoprimesSpecialCase1 :: Assertion
splitIntoCoprimesSpecialCase1 =
  assertBool "should not fail" $ splitIntoCoprimesProperty4 @Integer [(0, 0), (0, 0)]

-- | This is an undefined behaviour, but at least it should not
-- throw exceptions or loop forever.
splitIntoCoprimesSpecialCase2 :: Assertion
splitIntoCoprimesSpecialCase2 =
  assertBool "should not fail" $ splitIntoCoprimesProperty4 @Integer [(0, 1), (-2, 0)]

toListReturnsCorrectValues :: Assertion
toListReturnsCorrectValues = assertEqual
  "should be equal"
  (sort $ unCoprimes $ splitIntoCoprimes [(140, 1), (165, 1)])
  ([(5,2),(28,1),(33,1)] :: [(Integer, Word)])

unionReturnsCorrectValues :: Assertion
unionReturnsCorrectValues = assertEqual "should be equal" expected actual
  where
    a :: Coprimes Integer Word
    a = splitIntoCoprimes [(700, 1), (165, 1)] -- [(5,3),(28,1),(33,1)]
    b = splitIntoCoprimes [(360, 1), (210, 1)] -- [(2,4),(3,3),(5,2),(7,1)]
    expected = [(2,6),(3,4),(5,5),(7,2),(11,1)]
    actual = sort $ unCoprimes (a <> b)

insertReturnsCorrectValuesWhenCoprimeBase :: Assertion
insertReturnsCorrectValuesWhenCoprimeBase =
  let a = insert 5 2 (singleton 4 3)
      expected = [(4,3), (5,2)]
      actual = sort $ unCoprimes a :: [(Int, Int)]
  in assertEqual "should be equal" expected actual

insertReturnsCorrectValuesWhenNotCoprimeBase :: Assertion
insertReturnsCorrectValuesWhenNotCoprimeBase =
  let a = insert 2 4 (insert 7 1 (insert 5 2 (singleton 4 3)))
      actual = sort $ unCoprimes a :: [(Int, Int)]
      expected = [(2,10), (5,2), (7,1)]
  in assertEqual "should be equal" expected actual

unionProperty1
  :: (Ord a, GcdDomain a)
  => [(a, Power Word)]
  -> [(a, Power Word)]
  -> Bool
unionProperty1 xs ys
  =  sort (unCoprimes (splitIntoCoprimes (xs' <> ys')))
  == sort (unCoprimes (splitIntoCoprimes xs' <> splitIntoCoprimes ys'))
  where
    xs' = map (second getPower) xs
    ys' = map (second getPower) ys

testSuite :: TestTree
testSuite = testGroup "Euclidean"
  [ testSameIntegralProperty "coprime"     coprimeProperty
  , testGroup "splitIntoCoprimes"
    [ testGroup "preserves product of factors"
      [ testSmallAndQuick "Natural" (splitIntoCoprimesProperty1 @Natural)
      , testSmallAndQuick "Integer" (splitIntoCoprimesProperty1 @Integer)
      , testSmallAndQuick "Gaussian" (splitIntoCoprimesProperty1 @GaussianInteger)
      ]
    , testGroup "number of factors is non-decreasing"
      [ testSmallAndQuick "Natural" (splitIntoCoprimesProperty2 @Natural)
      , testSmallAndQuick "Integer" (splitIntoCoprimesProperty2 @Integer)
      , testSmallAndQuick "Gaussian" (splitIntoCoprimesProperty2 @GaussianInteger)
      ]
    , testGroup "output factors are coprime"
      [ testSmallAndQuick "Natural" (splitIntoCoprimesProperty3 @Natural)
      , testSmallAndQuick "Integer" (splitIntoCoprimesProperty3 @Integer)
      , testSmallAndQuick "Gaussian" (splitIntoCoprimesProperty3 @GaussianInteger)
      ]
    , testGroup "does not freeze"
      [ testCase          "case 1"                   splitIntoCoprimesSpecialCase1
      , testCase          "case 2"                   splitIntoCoprimesSpecialCase2
      , testSmallAndQuick "Natural" (splitIntoCoprimesProperty4 @Natural)
      -- smallcheck for Integer and GaussianInteger takes too long
      , QC.testProperty "Integer" (splitIntoCoprimesProperty4 @Integer)
      , QC.testProperty "Gaussian" (splitIntoCoprimesProperty4 @GaussianInteger)
      ]
    , testGroup "output factors are non-unit"
      [ testSmallAndQuick "Natural" (splitIntoCoprimesProperty5 @Natural)
      -- smallcheck for Integer and GaussianInteger takes too long
      , QC.testProperty "Integer" (splitIntoCoprimesProperty5 @Integer)
      , QC.testProperty "Gaussian" (splitIntoCoprimesProperty5 @GaussianInteger)
      ]
    ]
  , testGroup "Coprimes"
    [  testCase         "test equality"                       toListReturnsCorrectValues
    ,  testCase         "test union"                          unionReturnsCorrectValues
    ,  testCase         "test insert with coprime base"       insertReturnsCorrectValuesWhenCoprimeBase
    ,  testCase         "test insert with non-coprime base"   insertReturnsCorrectValuesWhenNotCoprimeBase
    ,  testGroup "property union"
      [ testSmallAndQuick "Natural" (unionProperty1 @Natural)
      -- smallcheck for Integer takes too long
      , QC.testProperty "Integer" (unionProperty1 @Integer)
      ]
    ]
  ]
