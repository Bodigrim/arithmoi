-- |
-- Module:      Math.NumberTheory.Primes.FactorisationTests
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Primes.Factorisation
--

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Primes.FactorisationTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.List (nub, sort, find)

import Math.NumberTheory.Primes.Factorisation
import Math.NumberTheory.Primes.Testing
import Math.NumberTheory.TestUtils

specialCases :: [(Integer, [(Integer, Int)])]
specialCases =
  [ (4181339589500970917,[(15034813,1),(278110515209,1)])
  , (4181339589500970918,[(2,1),(3,2),(7,1),(2595773,1),(12784336241,1)])
  , (2227144715990344929,[(3,1),(317,1),(17381911,1),(134731889,1)])
  , (10489674846272137811130167281,[(1312601,1),(9555017,1),(836368815445393,1)])
  , (10489674846272137811130167282,[(2,1),(17,1),(577,1),(3863,1),(179347163,1),(771770327021,1)])
  , (10489674846272137811130167283,[(3,1),(7,1),(4634410717,1),(107782489838601619,1)])
  , (10489674846272137811130167287,[(4122913189601,1),(2544238591472087,1)])
  , (6293073306208101456461600748,[(2,2),(3,1),(1613,1),(69973339,1),(4646378436563447,1)])
  , (6293073306208101456461600749,[(7,1),(103,1),(4726591,1),(1846628365511484259,1)])
  , (6293073306208101456461600750,[(2,1),(5,3),(239,1),(34422804769,1),(3059698456333,1)])
  , (6293073306208101456461600751,[(3,1),(13523,1),(1032679,1),(150211485989006401,1)])
  , (6293073306208101456461600753,[(19391,1),(372473053129,1),(871300023127,1)])
  , (6293073306208101456461600754,[(2,1),(3,2),(11,1),(13,1),(71,1),(2311,1),(22859,1),(7798621,1),(83583569,1)])
  , (11999991291828813663324577057,[(14381453,1),(10088205181,1),(82711187849,1)])
  , (11999991291828813663324577062,[(2,1),(3,1),(7,1),(3769,1),(634819511,1),(119413997449529,1)])
  , (16757651897802863152387219654541878160,[(2,4),(5,1),(12323,1),(1424513,1),(6205871923,1),(1922815011093901,1)])
  , (16757651897802863152387219654541878162,[(2,1),(29,1),(78173,1),(401529283,1),(1995634649,1),(4612433663779,1)])
  , (16757651897802863152387219654541878163,[(11,1),(31,1),(112160981904206269,1),(438144115295608147,1)])
  , (16757651897802863152387219654541878166,[(2,1),(23,1),(277,1),(505353699591289,1),(2602436338718275457,1)])
  ]

factoriseProperty1 :: Assertion
factoriseProperty1 = assertEqual "0" [] (factorise 1)

factoriseProperty2 :: Positive Integer -> Bool
factoriseProperty2 (Positive n) = (-1, 1) : factorise n == factorise (negate n)

factoriseProperty3 :: Positive Integer -> Bool
factoriseProperty3 (Positive n) = all (isPrime . fst) (factorise n)

factoriseProperty4 :: Positive Integer -> Bool
factoriseProperty4 (Positive n) = bases == nub (sort bases)
  where
    bases = map fst $ factorise n

factoriseProperty5 :: Positive Integer -> Bool
factoriseProperty5 (Positive n) = product (map (uncurry (^)) (factorise n)) == n

factoriseProperty6 :: (Integer, [(Integer, Int)]) -> Assertion
factoriseProperty6 (n, fs) = assertEqual (show n) fs (factorise n)

sieveSmallestFactorProperty1 :: Assertion
sieveSmallestFactorProperty1 = assertEqual "" (f <$> [-2, -1, 0, 1, 2])
                                              [Just (-1), Just (-1), Just 2, Nothing, Just 2]
  where
    f = sieveSmallestFactor (factorSieve 2)

sieveSmallestFactorProperty2 :: Positive Integer -> AnySign Integer -> Bool
sieveSmallestFactorProperty2 (Positive highBound) (AnySign x)
  = case (abs x, signum x) of
     -- factorise error case
     (0, _) -> sieveSmallestFactor (factorSieve highBound) 0 == Just 2
     -- sieveSmallestFactor error case
     (a, s) | a > highBound -> sieveSmallestFactorProperty2 (Positive a) (AnySign $ s * highBound)
     -- general case
     _ -> computed == expected
       where
         computed = sieveSmallestFactor (factorSieve highBound) x
         expected = headMay . fmap fst $ factorise x
         headMay = find (const True)

testSuite :: TestTree
testSuite = testGroup "Factorisation"
  [ testGroup "factorise" $
    [ testCase          "0"                              factoriseProperty1
    , testSmallAndQuick "negate"                         factoriseProperty2
    , testSmallAndQuick "bases are prime"                factoriseProperty3
    , testSmallAndQuick "bases are ordered and distinct" factoriseProperty4
    , testSmallAndQuick "factorback"                     factoriseProperty5
    ] ++
    map (\x -> testCase ("special case " ++ show (fst x)) (factoriseProperty6 x)) specialCases
  , testGroup "sieveSmallestFactor" $
    [ testCase          "edge cases"            sieveSmallestFactorProperty1
    , testSmallAndQuick "relation to factorise" sieveSmallestFactorProperty2
    ]
  ]
