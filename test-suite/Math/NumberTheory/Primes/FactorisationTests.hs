-- |
-- Module:      Math.NumberTheory.Primes.FactorisationTests
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Tests for Math.NumberTheory.Primes.Factorisation
--

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Primes.FactorisationTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Arrow
import Control.Monad (zipWithM_)
import Data.List (nub, sort)
import Data.Maybe

import Math.NumberTheory.Primes
import Math.NumberTheory.TestUtils

specialCases :: [(Integer, [(Integer, Word)])]
specialCases =
  [ (35,[(5,1),(7,1)])
  , (75,[(3,1),(5,2)])
  , (65521^2,[(65521,2)])
  , (65537^2,[(65537,2)])
  , (2147483647, [(2147483647, 1)])
  , (4294967291, [(4294967291, 1)])
  , (19000000000000000001, [(19000000000000000001, 1)])
  , (3 * 5^2 * 7^21, [(3,1), (5,2), (7, 21)])
  , (9223372036854775783, [(9223372036854775783, 1)])
  , (18446744073709551557, [(18446744073709551557, 1)])
  , (4181339589500970917,[(15034813,1),(278110515209,1)])
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
  -- , (16757651897802863152387219654541878166,[(2,1),(23,1),(277,1),(505353699591289,1),(2602436338718275457,1)])
  , ((10 ^ 80 - 1) `div` 9, [(11,1),(17,1),(41,1),(73,1),(101,1),(137,1),(271,1),(3541,1),(9091,1),(27961,1),
                             (1676321,1),(5070721,1),(5882353,1),(5964848081,1),(19721061166646717498359681,1)])
  , (623506907396924300595652906937, [(300137,1),(825131,2),(1746779,2)])
  , (626472835738582668418814215862, [(2,1),(150211,1),(11746151,2),(122939,2)])
  , (638396704483535474833679624037, [(3,1),(11,2),(100519,1),(104281,2),(1268419,2)])
  ]

lazyCases :: [(Integer, [(Integer, Word)])]
lazyCases =
  [ ( 14145130711
    * 10000000000000000000000000000000000000121
    * 100000000000000000000000000000000000000000000000447
    , [(14145130711, 1)]
    )
  ]

shortenNumber :: Integer -> String
shortenNumber n
  | l <= 10 = xs
  | otherwise = take 5 xs ++ "..." ++ drop (l - 5) xs
  where
    xs = show n
    l = length xs

factoriseProperty1 :: Assertion
factoriseProperty1 = assertEqual "0" [] (factorise (1 :: Int))

factoriseProperty2 :: Positive Integer -> Bool
factoriseProperty2 (Positive n) = factorise n == factorise (negate n)

factoriseProperty3 :: Positive Integer -> Bool
factoriseProperty3 (Positive n) = all (isJust . isPrime . unPrime . fst) (factorise n)

factoriseProperty4 :: Positive Integer -> Bool
factoriseProperty4 (Positive n) = sort bases == nub (sort bases)
  where
    bases = map fst $ factorise n

factoriseProperty5 :: Positive Integer -> Bool
factoriseProperty5 (Positive n) = product (map (\(p, k) -> unPrime p ^ k) (factorise n)) == n

factoriseProperty6 :: (Integer, [(Integer, Word)]) -> Assertion
factoriseProperty6 (n, fs) = assertEqual (show n) (sort fs) (sort $ map (first unPrime) $ factorise n)

factoriseProperty7 :: (Integer, [(Integer, Word)]) -> Assertion
factoriseProperty7 (n, fs) = zipWithM_ (assertEqual (show n)) fs (map (first unPrime) $ factorise n)

testSuite :: TestTree
testSuite = testGroup "Factorisation"
  [ testGroup "factorise" $
    [ testCase          "0"                              factoriseProperty1
    , testSmallAndQuick "negate"                         factoriseProperty2
    , testSmallAndQuick "bases are prime"                factoriseProperty3
    , testSmallAndQuick "bases are distinct"             factoriseProperty4
    , testSmallAndQuick "factorback"                     factoriseProperty5
    ] ++
    map (\x -> testCase ("special case " ++ shortenNumber (fst x)) (factoriseProperty6 x)) specialCases
    ++
    map (\x -> testCase ("laziness " ++ shortenNumber (fst x)) (factoriseProperty7 x)) lazyCases
  ]
