-- |
-- Module:      Math.NumberTheory.SmoothNumbersTests
-- Copyright:   (c) 2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Tests for Math.NumberTheory.SmoothNumbersTests
--

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.SmoothNumbersTests
  ( testSuite
  ) where

import Prelude hiding (mod, rem)
import Test.Tasty
import Test.Tasty.HUnit

import Data.Coerce
import Data.List (genericDrop, nub, sort)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Numeric.Natural

import Math.NumberTheory.Euclidean
import Math.NumberTheory.Primes (Prime (..))
import qualified Math.NumberTheory.Quadratic.GaussianIntegers as G
import qualified Math.NumberTheory.Quadratic.EisensteinIntegers as E
import Math.NumberTheory.SmoothNumbers
import Math.NumberTheory.TestUtils

fromSetListProperty :: (Num a, Euclidean a, Ord a) => [a] -> Bool
fromSetListProperty xs = fromSet (S.fromList xs) == fromList (sort xs)

isSmoothPropertyHelper :: (Eq a, Num a, Euclidean a) => (a -> Integer) -> [a] -> Int -> Int -> Bool
isSmoothPropertyHelper norm primes' i1 i2 =
    let primes = take i1 primes'
        basis  = fromJust (fromList primes)
    in all (isSmooth basis) $ take i2 $ smoothOver' norm basis

isSmoothProperty1 :: Positive Int -> Positive Int -> Bool
isSmoothProperty1 (Positive i1) (Positive i2) =
    isSmoothPropertyHelper G.norm (map unPrime G.primes) i1 i2

isSmoothProperty2 :: Positive Int -> Positive Int -> Bool
isSmoothProperty2 (Positive i1) (Positive i2) =
    isSmoothPropertyHelper E.norm (map unPrime E.primes) i1 i2

fromSmoothUpperBoundProperty :: Integral a => Positive a -> Bool
fromSmoothUpperBoundProperty (Positive n') = case fromSmoothUpperBound n of
    Nothing -> n < 2
    Just sb -> head (genericDrop (n - 1) (smoothOver (coerce sb))) == n
  where
    n = WrapIntegral n' `rem` 5000

smoothOverInRangeProperty :: Integral a => SmoothBasis a -> Positive a -> Positive a -> Bool
smoothOverInRangeProperty s (Positive lo') (Positive diff')
  = xs == ys
  where
    lo   = WrapIntegral lo'   `rem` 2^18
    diff = WrapIntegral diff' `rem` 2^18
    hi   = lo + diff
    xs   = smoothOverInRange   (coerce s) lo hi
    ys   = smoothOverInRangeBF (coerce s) lo hi

smoothNumbersAreUniqueProperty :: Integral a => SmoothBasis a -> Positive Int -> Bool
smoothNumbersAreUniqueProperty s (Positive len)
  = nub l == l
  where
    l = take len $ smoothOver s

isSmoothSpecialCase1 :: Assertion
isSmoothSpecialCase1 = assertBool "should be distinct" $ nub l == l
  where
    b = fromJust $ fromList [1+3*G.ι,6+8*G.ι]
    l = take 10 $ map abs $ smoothOver' G.norm b

isSmoothSpecialCase2 :: Assertion
isSmoothSpecialCase2 = assertBool "should be smooth" $ isSmooth b 6
  where
    b = fromJust $ fromList [4, 3, 6, 10, 7::Int]

testSuite :: TestTree
testSuite = testGroup "SmoothNumbers"
  [ testGroup "fromSet == fromList"
    [ testSmallAndQuick "Int"     (fromSetListProperty :: [Int] -> Bool)
    , testSmallAndQuick "Word"    (fromSetListProperty :: [Word] -> Bool)
    , testSmallAndQuick "Integer" (fromSetListProperty :: [Integer] -> Bool)
    , testSmallAndQuick "Natural" (fromSetListProperty :: [Natural] -> Bool)
    ]
  , testIntegralPropertyNoLarge "fromSmoothUpperBound" fromSmoothUpperBoundProperty
  , testGroup "smoothOverInRange == smoothOverInRangeBF"
    [ testSmallAndQuick "Int"
      (smoothOverInRangeProperty :: SmoothBasis Int -> Positive Int -> Positive Int -> Bool)
    , testSmallAndQuick "Word"
      (smoothOverInRangeProperty :: SmoothBasis Word -> Positive Word -> Positive Word -> Bool)
    , testSmallAndQuick "Integer"
      (smoothOverInRangeProperty :: SmoothBasis Integer -> Positive Integer -> Positive Integer -> Bool)
    , testSmallAndQuick "Natural"
      (smoothOverInRangeProperty :: SmoothBasis Natural -> Positive Natural -> Positive Natural -> Bool)
    ]
  , testGroup "smoothOver generates a list without duplicates"
    [ testSmallAndQuick "Integer"
      (smoothNumbersAreUniqueProperty :: SmoothBasis Integer -> Positive Int -> Bool)
    , testSmallAndQuick "Natural"
      (smoothNumbersAreUniqueProperty :: SmoothBasis Natural -> Positive Int -> Bool)
    ]
  , testGroup "Quadratic rings (Gaussian/Eisenstein)"
    [ testGroup "smoothOver generates valid smooth numbers"
      [ testSmallAndQuick "Gaussian" isSmoothProperty1
      , testSmallAndQuick "Eisenstein" isSmoothProperty2
      ]
    , testCase "all distinct for base [1+3*i,6+8*i]" isSmoothSpecialCase1
    , testCase "6 is smooth for base [4,3,6,10,7]" isSmoothSpecialCase2
    ]
  ]
