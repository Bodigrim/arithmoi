-- |
-- Module:      Math.NumberTheory.ModuliTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Moduli
--

{-# LANGUAGE CPP             #-}
{-# LANGUAGE ViewPatterns    #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.ModuliTests
  ( testSuite
  ) where

import Test.Tasty

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Control.Arrow
import Data.Bits
import Data.Functor.Compose
import Data.List (tails, nub)
import Data.Maybe
import Data.Semigroup

import Math.NumberTheory.Moduli hiding (invertMod)
import Math.NumberTheory.TestUtils

unwrapPP :: (Prime, Power Int) -> (Integer, Int)
unwrapPP (Prime p, Power e) = (p, e)

invertMod :: Integer -> Integer -> Maybe Integer
invertMod x m = getSomeVal <$> invertSomeMod (x `modulo` fromInteger m)

powerMod :: Integral a => Integer -> a -> Integer -> Integer
powerMod b e m = getSomeVal ((b `modulo` fromInteger m) ^ e)

-- https://en.wikipedia.org/wiki/Jacobi_symbol#Properties, item 2
jacobiProperty2 :: (Integral a, Bits a) => AnySign a -> (Compose Positive Odd) a -> Bool
jacobiProperty2 (AnySign a) (Compose (Positive (Odd n)))
  =  a + n < a -- check overflow
  || jacobi a n == jacobi (a + n) n

-- https://en.wikipedia.org/wiki/Jacobi_symbol#Properties, item 3
jacobiProperty3 :: (Integral a, Bits a) => AnySign a -> (Compose Positive Odd) a -> Bool
jacobiProperty3 (AnySign a) (Compose (Positive (Odd n))) = case jacobi a n of
  MinusOne -> a `gcd` n == 1
  Zero     -> a `gcd` n /= 1
  One      -> a `gcd` n == 1

-- https://en.wikipedia.org/wiki/Jacobi_symbol#Properties, item 4
jacobiProperty4 :: (Integral a, Bits a) => AnySign a -> AnySign a -> (Compose Positive Odd) a -> Bool
jacobiProperty4 (AnySign a) (AnySign b) (Compose (Positive (Odd n))) = jacobi (a * b) n == jacobi a n <> jacobi b n

jacobiProperty4_Integer :: AnySign Integer -> AnySign Integer -> (Compose Positive Odd) Integer -> Bool
jacobiProperty4_Integer = jacobiProperty4

-- https://en.wikipedia.org/wiki/Jacobi_symbol#Properties, item 5
jacobiProperty5 :: (Integral a, Bits a) => AnySign a -> (Compose Positive Odd) a -> (Compose Positive Odd) a -> Bool
jacobiProperty5 (AnySign a) (Compose (Positive (Odd m))) (Compose (Positive (Odd n))) = jacobi a (m * n) == jacobi a m <> jacobi a n

jacobiProperty5_Integer :: AnySign Integer -> (Compose Positive Odd) Integer -> (Compose Positive Odd) Integer -> Bool
jacobiProperty5_Integer = jacobiProperty5

-- https://en.wikipedia.org/wiki/Jacobi_symbol#Properties, item 6
jacobiProperty6 :: (Integral a, Bits a) => (Compose Positive Odd) a -> (Compose Positive Odd) a -> Bool
jacobiProperty6 (Compose (Positive (Odd m))) (Compose (Positive (Odd n))) = gcd m n /= 1 || jacobi m n <> jacobi n m == (if m `mod` 4 == 1 || n `mod` 4 == 1 then One else MinusOne)

-- | Check that 'invertMod' inverts numbers modulo.
invertModProperty :: AnySign Integer -> Positive Integer -> Bool
invertModProperty (AnySign k) (Positive m) = case invertMod k m of
  Nothing  -> k `mod` m == 0 || gcd k m > 1
  Just inv -> gcd k m == 1
      && k * inv `mod` m == 1 && 0 <= inv && inv < m

-- | Check that the result of 'powerMod' is between 0 and modulo (non-inclusive).
powerModProperty1 :: (Integral a, Bits a) => NonNegative a -> AnySign Integer -> Positive Integer -> Bool
powerModProperty1 (NonNegative e) (AnySign b) (Positive m)
  =  e < 0 && isNothing (invertMod b m)
  || (0 <= pm && pm < m)
  where
    pm = powerMod b e m

-- | Check that 'powerMod' is multiplicative by first argument.
powerModProperty2 :: (Integral a, Bits a) => NonNegative a -> AnySign Integer -> AnySign Integer -> Positive Integer -> Bool
powerModProperty2 (NonNegative e) (AnySign b1) (AnySign b2) (Positive m)
  =  e < 0 && (isNothing (invertMod b1 m) || isNothing (invertMod b2 m))
  || pm1 * pm2 `mod` m == pm12
  where
    pm1  = powerMod b1  e m
    pm2  = powerMod b2  e m
    pm12 = powerMod (b1 * b2) e m

-- | Check that 'powerMod' is additive by second argument.
powerModProperty3 :: (Integral a, Bits a) => NonNegative a -> NonNegative a -> AnySign Integer -> Positive Integer -> Bool
powerModProperty3 (NonNegative e1) (NonNegative e2) (AnySign b) (Positive m)
  =  (e1 < 0 || e2 < 0) && isNothing (invertMod b m)
  || e2 >= 0 && e1 + e2 < e1 -- check overflow
  || e1 >= 0 && e1 + e2 < e2 -- check overflow
  || e2 <= 0 && e1 + e2 > e1 -- check overflow
  || e1 <= 0 && e1 + e2 > e2 -- check overflow
  || pm1 * pm2 `mod` m == pm12
  where
    pm1  = powerMod b e1 m
    pm2  = powerMod b e2 m
    pm12 = powerMod b (e1 + e2) m

-- | Specialized to trigger 'powerModInteger'.
powerModProperty1_Integer :: NonNegative Integer -> AnySign Integer -> Positive Integer -> Bool
powerModProperty1_Integer = powerModProperty1

-- | Specialized to trigger 'powerModInteger'.
powerModProperty2_Integer :: NonNegative Integer -> AnySign Integer -> AnySign Integer -> Positive Integer -> Bool
powerModProperty2_Integer = powerModProperty2

-- | Specialized to trigger 'powerModInteger'.
powerModProperty3_Integer :: NonNegative Integer -> NonNegative Integer -> AnySign Integer -> Positive Integer -> Bool
powerModProperty3_Integer = powerModProperty3

-- | Check that 'chineseRemainder' is defined iff modulos are coprime.
--   Also check that the result is a solution of input modular equations.
chineseRemainderProperty :: [(Integer, Positive Integer)] -> Bool
chineseRemainderProperty rms' = case chineseRemainder rms of
  Nothing -> not areCoprime
  Just n  -> areCoprime && map (n `mod`) ms == zipWith mod rs ms
  where
    rms = map (second getPositive) rms'
    (rs, ms) = unzip rms
    areCoprime = all (== 1) [ gcd m1 m2 | (m1 : m2s) <- tails ms, m2 <- m2s ]

-- | Check that 'chineseRemainder' matches 'chineseRemainder2'.
chineseRemainder2Property :: Integer -> Positive Integer -> Integer -> Positive Integer -> Bool
chineseRemainder2Property r1 (Positive m1) r2 (Positive m2) = gcd m1 m2 /= 1
  || Just (chineseRemainder2 (r1, m1) (r2, m2)) == chineseRemainder [(r1, m1), (r2, m2)]

-- | Check that 'sqrtMod' is defined iff a quadratic residue exists.
--   Also check that the result is a solution of input modular equation.
sqrtModPProperty :: AnySign Integer -> Prime -> Bool
sqrtModPProperty (AnySign n) (Prime p) = case sqrtModP n p of
  Nothing -> jacobi n p == MinusOne
  Just rt -> (p == 2 || jacobi n p /= MinusOne) && rt ^ 2 `mod` p == n `mod` p

sqrtModPListProperty :: AnySign Integer -> Prime -> Bool
sqrtModPListProperty (AnySign n) (Prime p) = all (\rt -> rt ^ 2 `mod` p == n `mod` p) (sqrtModPList n p)

sqrtModP'Property :: Positive Integer -> Prime -> Bool
sqrtModP'Property (Positive n) (Prime p) = (p /= 2 && jacobi n p /= One) || rt ^ 2 `mod` p == n `mod` p
  where
    rt = sqrtModP' n p

tonelliShanksProperty :: Positive Integer -> Prime -> Bool
tonelliShanksProperty (Positive n) (Prime p) = p `mod` 4 /= 1 || jacobi n p /= One || rt ^ 2 `mod` p == n `mod` p
  where
    rt = tonelliShanks n p

sqrtModPPProperty :: AnySign Integer -> (Prime, Power Int) -> Bool
sqrtModPPProperty (AnySign n) (Prime p, Power e) = gcd n p > 1 || case sqrtModPP n (p, e) of
  Nothing -> True
  Just rt -> rt ^ 2 `mod` (p ^ e) == n `mod` (p ^ e)

sqrtModPPListProperty :: AnySign Integer -> (Prime, Power Int) -> Bool
sqrtModPPListProperty (AnySign n) (Prime p, Power e) = gcd n p > 1
  || all (\rt -> rt ^ 2 `mod` (p ^ e) == n `mod` (p ^ e)) (sqrtModPPList n (p, e))

sqrtModFProperty :: AnySign Integer -> [(Prime, Power Int)] -> Bool
sqrtModFProperty (AnySign n) (map unwrapPP -> pes) = case sqrtModF n pes of
  Nothing -> True
  Just rt -> all (\(p, e) -> rt ^ 2 `mod` (p ^ e) == n `mod` (p ^ e)) pes

sqrtModFListProperty :: AnySign Integer -> [(Prime, Power Int)] -> Bool
sqrtModFListProperty (AnySign n) (map unwrapPP -> pes)
  = nub ps /= ps || all
    (\rt -> all (\(p, e) -> rt ^ 2 `mod` (p ^ e) == n `mod` (p ^ e)) pes)
    (sqrtModFList n pes)
  where
    ps = map fst pes

testSuite :: TestTree
testSuite = testGroup "Moduli"
  [ testGroup "jacobi"
    [ testSameIntegralProperty "same modulo n"                jacobiProperty2
    , testSameIntegralProperty "consistent with gcd"          jacobiProperty3
    , testSmallAndQuick        "multiplicative 1"             jacobiProperty4_Integer
    , testSmallAndQuick        "multiplicative 2"             jacobiProperty5_Integer
    , testSameIntegralProperty "law of quadratic reciprocity" jacobiProperty6
    ]
  , testSmallAndQuick "invertMod" invertModProperty
  , testGroup "powerMod"
    [ testGroup "generic"
      [ testIntegralProperty "bounded between 0 and m"  powerModProperty1
      , testIntegralProperty "multiplicative by base"   powerModProperty2
      , testSameIntegralProperty "additive by exponent" powerModProperty3
      ]
    , testGroup "Integer"
      [ testSmallAndQuick "bounded between 0 and m" powerModProperty1_Integer
      , testSmallAndQuick "multiplicative by base"  powerModProperty2_Integer
      , testSmallAndQuick "additive by exponent"    powerModProperty3_Integer
      ]
    ]
    , testSmallAndQuick "chineseRemainder"  chineseRemainderProperty
    , testSmallAndQuick "chineseRemainder2" chineseRemainder2Property
    , testGroup "sqrtMod"
      [ testSmallAndQuick "sqrtModP"      sqrtModPProperty
      , testSmallAndQuick "sqrtModPList"  sqrtModPListProperty
      , testSmallAndQuick "sqrtModP'"     sqrtModP'Property
      , testSmallAndQuick "tonelliShanks" tonelliShanksProperty
      , testSmallAndQuick "sqrtModPP"     sqrtModPPProperty
      , testSmallAndQuick "sqrtModPPList" sqrtModPPListProperty
      , testSmallAndQuick "sqrtModF"      sqrtModFProperty
      , testSmallAndQuick "sqrtModFList"  sqrtModFListProperty
      ]
  ]
