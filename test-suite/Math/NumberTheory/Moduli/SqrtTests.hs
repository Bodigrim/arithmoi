-- |
-- Module:      Math.NumberTheory.Moduli.SqrtTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Tests for Math.NumberTheory.Moduli.Sqrt
--

{-# LANGUAGE CPP             #-}
{-# LANGUAGE ViewPatterns    #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Moduli.SqrtTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Arrow
import Data.List (group, sort)
import Data.Maybe (fromJust)
import Numeric.Natural

import Math.NumberTheory.Moduli hiding (invertMod)
import Math.NumberTheory.Primes (unPrime, isPrime, Prime)
import Math.NumberTheory.TestUtils

unwrapPP :: (Prime Integer, Power Word) -> (Prime Integer, Word)
unwrapPP (p, Power e) = (p, e `mod` 5)

nubOrd :: Ord a => [a] -> [a]
nubOrd = map head . group . sort

-- | Check that 'sqrtMod' is defined iff a quadratic residue exists.
--   Also check that the result is a solution of input modular equation.
sqrtsModPrimeProperty1 :: AnySign Integer -> Prime Integer -> Bool
sqrtsModPrimeProperty1 (AnySign n) p'@(unPrime -> p) = case sqrtsModPrime n p' of
  []     -> jacobi n p == MinusOne
  rt : _ -> (p == 2 || jacobi n p /= MinusOne) && (rt ^ 2 - n) `rem` p == 0

sqrtsModPrimeProperty2 :: AnySign Integer -> Prime Integer -> Bool
sqrtsModPrimeProperty2 (AnySign n) p'@(unPrime -> p) = all (\rt -> (rt ^ 2 - n) `rem` p == 0) (sqrtsModPrime n p')

sqrtsModPrimeProperty3 :: AnySign Integer -> Prime Integer -> Bool
sqrtsModPrimeProperty3 (AnySign n) p'@(unPrime -> p) = nubOrd rts == sort rts
  where
    rts = map (`mod` p) $ sqrtsModPrime n p'

sqrtsModPrimeProperty4 :: AnySign Integer -> Prime Integer -> Bool
sqrtsModPrimeProperty4 (AnySign n) p'@(unPrime -> p) = all (\rt -> rt >= 0 && rt < p) (sqrtsModPrime n p')

tonelliShanksProperty1 :: Positive Integer -> Prime Integer -> Bool
tonelliShanksProperty1 (Positive n) p'@(unPrime -> p) = p `mod` 4 /= 1 || jacobi n p /= One || rt ^ 2 `mod` p == n `mod` p
  where
    rt : _ = sqrtsModPrime n p'

tonelliShanksProperty2 :: Prime Integer -> Bool
tonelliShanksProperty2 p'@(unPrime -> p) = p `mod` 4 /= 1 || (rt ^ 2 - n) `rem` p == 0
  where
    n  = head $ filter (\s -> jacobi s p == One) [2..p-1]
    rt : _ = sqrtsModPrime n p'

tonelliShanksProperty3 :: Prime Integer -> Bool
tonelliShanksProperty3 p'@(unPrime -> p)
  = p `mod` 4 /= 1
  || rt ^ 2 `mod` p == p - 1
  where
    rt : _ = sqrtsModPrime (-1) p'

tonelliShanksSpecialCases :: Assertion
tonelliShanksSpecialCases =
  assertEqual "OEIS A002224" [6, 32, 219, 439, 1526, 2987, 22193, 11740, 13854, 91168, 326277, 232059, 3230839, 4379725, 11754394, 32020334, 151024619, 345641931, 373671108, 1857111865, 8110112775, 4184367042] rts
  where
    ps :: [Integer]
    ps = [17, 73, 241, 1009, 2689, 8089, 33049, 53881, 87481, 483289, 515761, 1083289, 3818929, 9257329, 22000801, 48473881, 175244281, 427733329, 898716289, 8114538721, 9176747449, 23616331489]
    rts = map (head . sqrtsModPrime 2 . fromJust . isPrime) ps

sqrtsModPrimePowerProperty1 :: AnySign Integer -> (Prime Integer, Power Word) -> Bool
sqrtsModPrimePowerProperty1 (AnySign n) (p'@(unPrime -> p), Power e) = gcd n p > 1
  || all (\rt -> (rt ^ 2 - n) `rem` (p ^ e) == 0) (sqrtsModPrimePower n p' e)

sqrtsModPrimePowerProperty2 :: AnySign Integer -> Power Word -> Bool
sqrtsModPrimePowerProperty2 n e = sqrtsModPrimePowerProperty1 n (fromJust $ isPrime (2 :: Integer), e)

sqrtsModPrimePowerProperty3 :: AnySign Integer -> (Prime Integer, Power Word) -> Bool
sqrtsModPrimePowerProperty3 (AnySign n) (p'@(unPrime -> p), Power e') = nubOrd rts == sort rts
  where
    e = e' `mod` 5
    m = p ^ e
    rts = map (`mod` m) $ sqrtsModPrimePower n p' e

sqrtsModPrimePowerProperty4 :: AnySign Integer -> Power Word -> Bool
sqrtsModPrimePowerProperty4 n e = sqrtsModPrimePowerProperty3 n (fromJust $ isPrime (2 :: Integer), e)

sqrtsModPrimePowerProperty5 :: AnySign Integer -> (Prime Integer, Power Word) -> Bool
sqrtsModPrimePowerProperty5 (AnySign n) (p'@(unPrime -> p), Power e') = all (\rt -> rt >= 0 && rt < m) rts
  where
    e = e' `mod` 5
    m = p ^ e
    rts = sqrtsModPrimePower n p' e

sqrtsModPrimePowerProperty6 :: AnySign Integer -> Power Word -> Bool
sqrtsModPrimePowerProperty6 n e = sqrtsModPrimePowerProperty5 n (fromJust $ isPrime (2 :: Integer), e)

sqrtsModPrimePowerSpecialCase1 :: Assertion
sqrtsModPrimePowerSpecialCase1 =
  assertEqual "should be equal" [0, 2] (sort (sqrtsModPrimePower 16 (fromJust (isPrime (2 :: Integer))) 2))

sqrtsModPrimePowerSpecialCase2 :: Assertion
sqrtsModPrimePowerSpecialCase2 =
  assertEqual "should be equal" [4, 5] (sort (sqrtsModPrimePower 16 (fromJust (isPrime (3 :: Integer))) 2))

sqrtsModPrimePowerSpecialCase3 :: Assertion
sqrtsModPrimePowerSpecialCase3 =
  assertEqual "should be equal" [0, 3, 6] (sort (sqrtsModPrimePower 0 (fromJust (isPrime (3 :: Integer))) 2))

sqrtsModPrimePowerSpecialCase4 :: Assertion
sqrtsModPrimePowerSpecialCase4 =
  assertEqual "should be equal" [0, 9, 18] (sort (sqrtsModPrimePower 0 (fromJust (isPrime (3 :: Integer))) 3))

sqrtsModPrimePowerSpecialCase5 :: Assertion
sqrtsModPrimePowerSpecialCase5 =
  assertEqual "should be equal" [0, 4, 8, 12] (sort (sqrtsModPrimePower 0 (fromJust (isPrime (2 :: Integer))) 4))

sqrtsModPrimePowerSpecialCase6 :: Assertion
sqrtsModPrimePowerSpecialCase6 =
  assertEqual "should be equal" [3, 6, 12, 15, 21, 24] (sort (sqrtsModPrimePower 9 (fromJust (isPrime (3 :: Integer))) 3))

sqrtsModPrimePowerSpecialCase7 :: Assertion
sqrtsModPrimePowerSpecialCase7 =
  assertEqual "should be equal" [2, 6] (sort (sqrtsModPrimePower 4 (fromJust (isPrime (2 :: Integer))) 3))

sqrtsModPrimePowerSpecialCase8 :: Assertion
sqrtsModPrimePowerSpecialCase8 =
  assertEqual "should be equal" [1, 3] (sort (sqrtsModPrimePower 1 (fromJust (isPrime (2 :: Integer))) 2))

sqrtsModPrimePowerSpecialCase9 :: Assertion
sqrtsModPrimePowerSpecialCase9 =
  assertEqual "should be equal" [] (sort (sqrtsModPrimePower (-1) (fromJust (isPrime (2 :: Integer))) 2))

sqrtsModPrimePowerSpecialCase10 :: Assertion
sqrtsModPrimePowerSpecialCase10 =
  assertEqual "should be equal" [2, 6, 10, 14] (sort (sqrtsModPrimePower 4 (fromJust (isPrime (2 :: Integer))) 4))

sqrtsModPrimePowerSpecialCase11 :: Assertion
sqrtsModPrimePowerSpecialCase11 =
  assertEqual "should be equal" [4,12,20,28,36,44,52,60] (sort (sqrtsModPrimePower 16 (fromJust (isPrime (2 :: Integer))) 6))

sqrtsModFactorisationProperty1 :: AnySign Integer -> [(Prime Integer, Power Word)] -> Bool
sqrtsModFactorisationProperty1 (AnySign n) (take 10 . map unwrapPP -> pes'@(map (first unPrime) -> pes))
  = nubOrd ps /= sort ps || all
    (\rt -> all (\(p, e) -> (rt ^ 2 - n) `rem` (p ^ e) == 0) pes)
    (take 1000 $ sqrtsModFactorisation n pes')
  where
    ps = map fst pes

sqrtsModFactorisationProperty2 :: AnySign Integer -> [(Prime Integer, Power Word)] -> Bool
sqrtsModFactorisationProperty2 (AnySign n) (take 10 . map unwrapPP -> pes'@(map (first unPrime) -> pes))
  = nubOrd ps /= sort ps || nubOrd rts == sort rts
  where
    ps = map fst pes
    m = product $ map (\(p, e) -> p ^ e) pes
    rts = map (`mod` m) $ take 1000 $ sqrtsModFactorisation n pes'

sqrtsModFactorisationProperty3 :: AnySign Integer -> [(Prime Integer, Power Word)] -> Bool
sqrtsModFactorisationProperty3 (AnySign n) (take 10 . map unwrapPP -> pes'@(map (first unPrime) -> pes))
  = nubOrd ps /= sort ps || all (\rt -> rt >= 0 && rt < m) rts
  where
    ps = map fst pes
    m = product $ map (\(p, e) -> p ^ e) pes
    rts = take 1000 $ sqrtsModFactorisation n pes'

sqrtsModFactorisationSpecialCase1 :: Assertion
sqrtsModFactorisationSpecialCase1 =
  assertEqual "should be equal" [0]
    (sqrtsModFactorisation 0 $ map (first (fromJust . isPrime)) [(2 :: Integer, 1), (3, 1), (5, 1)])

sqrtsModFactorisationSpecialCase2 :: Assertion
sqrtsModFactorisationSpecialCase2 =
  assertEqual "should be equal" [0]
    (sqrtsModFactorisation 0 $ map (first (fromJust . isPrime)) [(3 :: Integer, 1), (5, 1)])

sqrtsModProperty1 :: AnySign Integer -> Positive Natural -> Bool
sqrtsModProperty1 (AnySign n) (Positive m) = case n `modulo` m of
  SomeMod x -> sort (sqrtsMod sfactors x) == filter (\rt -> rt * rt == x) [minBound .. maxBound]

testSuite :: TestTree
testSuite = testGroup "Sqrt"
  [ testGroup "sqrtsModPrime"
    [ testSmallAndQuick "matches jacobi"   sqrtsModPrimeProperty1
    , testSmallAndQuick "is residue"       sqrtsModPrimeProperty2
    , testSmallAndQuick "distinct"         sqrtsModPrimeProperty3
    , testSmallAndQuick "bounded"          sqrtsModPrimeProperty4
    ]
  , testGroup "tonelliShanks"
    [ testSmallAndQuick "generic"          tonelliShanksProperty1
    , testSmallAndQuick "smallest residue" tonelliShanksProperty2
    , testSmallAndQuick "-1"               tonelliShanksProperty3
    , testCase          "OEIS A002224"     tonelliShanksSpecialCases
    ]
  , testGroup "sqrtsModPrimePower"
    [ testSmallAndQuick "generic"        sqrtsModPrimePowerProperty1
    , testSmallAndQuick "_  2 _"         sqrtsModPrimePowerProperty2
    , testSmallAndQuick "distinct"       sqrtsModPrimePowerProperty3
    , testSmallAndQuick "_  2 _"         sqrtsModPrimePowerProperty4
    , testSmallAndQuick "bounded"        sqrtsModPrimePowerProperty5
    , testSmallAndQuick "_  2 _"         sqrtsModPrimePowerProperty6
    , testCase          "16 2 2"         sqrtsModPrimePowerSpecialCase1
    , testCase          "16 3 2"         sqrtsModPrimePowerSpecialCase2
    , testCase          "0  3 2"         sqrtsModPrimePowerSpecialCase3
    , testCase          "0  3 3"         sqrtsModPrimePowerSpecialCase4
    , testCase          "0  2 4"         sqrtsModPrimePowerSpecialCase5
    , testCase          "9  3 3"         sqrtsModPrimePowerSpecialCase6
    , testCase          "4  2 3"         sqrtsModPrimePowerSpecialCase7
    , testCase          "1  2 2"         sqrtsModPrimePowerSpecialCase8
    , testCase          "-1 2 2"         sqrtsModPrimePowerSpecialCase9
    , testCase          "4  2 4"         sqrtsModPrimePowerSpecialCase10
    , testCase          "16 2 6"         sqrtsModPrimePowerSpecialCase11
    ]
  , testGroup "sqrtsModFactorisation"
    [ testSmallAndQuick "generic"                 sqrtsModFactorisationProperty1
    , testSmallAndQuick "distinct"                sqrtsModFactorisationProperty2
    , testSmallAndQuick "bounded"                 sqrtsModFactorisationProperty3
    , testCase          "0 [(2,1), (3,1), (5,1)]" sqrtsModFactorisationSpecialCase1
    , testCase          "0 [(3,1), (5,1)]"        sqrtsModFactorisationSpecialCase2
    ]
  , testGroup "sqrtsMod"
    [ testSmallAndQuick "generic" sqrtsModProperty1
    ]
  ]
