-- |
-- Module:      Math.NumberTheory.Moduli.Cbrt
-- Copyright:   (c) 2020 Federico Bongiorno
-- Licence:     MIT
-- Maintainer:  Federico Bongiorno <federicobongiorno97@gmail.com>
--
-- Test for Math.NumberTheory.Moduli.Cbrt
--

module Math.NumberTheory.Moduli.CbrtTests
  ( testSuite
  ) where

import Math.NumberTheory.Moduli.Cbrt
import Math.NumberTheory.Quadratic.EisensteinIntegers
import Math.NumberTheory.Primes
import qualified Data.Euclidean as A
import Data.List (genericReplicate)
import Test.Tasty
import Math.NumberTheory.TestUtils

-- Checks multiplicative property of numerators. In details,
-- @cubicSymbol1 alpha1 alpha2 beta@ checks that
-- @(cubicSymbol alpha1 beta) <> (cubicSymbol alpha2 beta) == (cubicSymbol alpha1*alpha2 beta)@
cubicSymbol1 :: EisensteinInteger -> EisensteinInteger -> EisensteinInteger -> Bool
cubicSymbol1 alpha1 alpha2 beta = isBadDenominator beta || cubicSymbolNumerator alpha1 alpha2 beta

cubicSymbolNumerator :: EisensteinInteger -> EisensteinInteger -> EisensteinInteger -> Bool
cubicSymbolNumerator alpha1 alpha2 beta = (symbol1 <> symbol2) == symbolProduct
  where
    symbol1 = cubicSymbol alpha1 beta
    symbol2 = cubicSymbol alpha2 beta
    symbolProduct = cubicSymbol alphaProduct beta
    alphaProduct = alpha1 * alpha2

-- Checks multiplicative property of denominators. In details,
-- @cubicSymbol2 alpha beta1 beta2@ checks that
-- @(cubicSymbol alpha beta1) <> (cubicSymbol alpha beta2) == (cubicSymbol alpha beta1*beta2)@
cubicSymbol2 :: EisensteinInteger -> EisensteinInteger -> EisensteinInteger -> Bool
cubicSymbol2 alpha beta1 beta2 = isBadDenominator beta1 || isBadDenominator beta2 || cubicSymbolDenominator alpha beta1 beta2

cubicSymbolDenominator :: EisensteinInteger -> EisensteinInteger -> EisensteinInteger -> Bool
cubicSymbolDenominator alpha beta1 beta2 = (symbol1 <> symbol2) == symbolProduct
  where
    symbol1 = cubicSymbol alpha beta1
    symbol2 = cubicSymbol alpha beta2
    symbolProduct = cubicSymbol alpha betaProduct
    betaProduct = beta1 * beta2

-- Checks that `cubicSymbol` agrees with the computational definition
-- <https://en.wikipedia.org/wiki/Cubic_reciprocity#Definition here>
-- when the denominator is prime.
cubicSymbol3 :: EisensteinInteger -> Prime EisensteinInteger -> Bool
cubicSymbol3 alpha prime = isBadDenominator beta || cubicSymbol alpha beta == cubicSymbolPrime alpha beta
    where beta = unPrime prime

cubicSymbolPrime :: EisensteinInteger -> EisensteinInteger -> CubicSymbol
cubicSymbolPrime alpha beta = findCubicSymbol residue beta
  where
    residue = foldr f 1 listOfAlphas
    f x y = (x * y) `A.rem` beta
    listOfAlphas = genericReplicate alphaExponent alpha
    -- Exponent is defined to be 1/3*(@betaNorm@ - 1).
    alphaExponent = betaNorm `div` 3
    betaNorm = norm beta

isBadDenominator :: EisensteinInteger -> Bool
isBadDenominator x = modularNorm == 0
  where
    modularNorm = norm x `mod` 3

-- This complication is necessary because it may happen that the residue field
-- of @beta@ has characteristic two. In this case 1=-1 and the Euclidean algorithm
-- can return both. Therefore it is not enough to pattern match for the values
-- which give a well defined @cubicSymbol@.
findCubicSymbol :: EisensteinInteger -> EisensteinInteger -> CubicSymbol
findCubicSymbol residue beta
  | residue `A.rem` beta == 0             = Zero
  | (residue - ω) `A.rem` beta == 0       = Omega
  | (residue + 1 + ω) `A.rem` beta == 0   = OmegaSquare
  | (residue - 1) `A.rem` beta == 0       = One
  | otherwise                             = error "Math.NumberTheory.Moduli.Cbrt: invalid EisensteinInteger."

testSuite :: TestTree
testSuite = testGroup "CubicSymbol"
  [ testSmallAndQuick "multiplicative property of numerators" cubicSymbol1
  , testSmallAndQuick "multiplicative property of denominators" cubicSymbol2
  , testSmallAndQuick "cubic residue with prime denominator" cubicSymbol3
  ]
