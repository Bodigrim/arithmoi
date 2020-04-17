module Math.NumberTheory.Moduli.CubicSymbolTests
  ( testSuite
  ) where

import Math.NumberTheory.Moduli.CubicSymbol
import Math.NumberTheory.Quadratic.EisensteinIntegers
import Test.Tasty
import Math.NumberTheory.TestUtils

-- Checks multiplicative property of the numerators
cubicSymbol1 :: EisensteinInteger -> EisensteinInteger -> EisensteinInteger -> Bool
cubicSymbol1 alpha1 alpha2 beta = (modularNorm == 0) || cubicSymbolNumerator alpha1 alpha2 beta
  where
    modularNorm = norm beta `mod` 3

cubicSymbolNumerator :: EisensteinInteger -> EisensteinInteger -> EisensteinInteger -> Bool
cubicSymbolNumerator alpha1 alpha2 beta = (symbol1 <> symbol2) == symbolProduct
  where
    symbol1 = cubicSymbol alpha1 beta
    symbol2 = cubicSymbol alpha2 beta
    symbolProduct = cubicSymbol alphaProduct beta
    alphaProduct = alpha1 * alpha2

-- Checks multiplicative property of the denominators
cubicSymbol2 :: EisensteinInteger -> EisensteinInteger -> EisensteinInteger -> Bool
cubicSymbol2 alpha beta1 beta2 = (modularNorm1 == 0) || (modularNorm2 == 0) || cubicSymbolDenominator alpha beta1 beta2
  where
    (modularNorm1, modularNorm2) = (norm1 `mod` 3, norm2 `mod` 3)
    (norm1, norm2) = (norm beta1, norm beta2)

cubicSymbolDenominator :: EisensteinInteger -> EisensteinInteger -> EisensteinInteger -> Bool
cubicSymbolDenominator alpha beta1 beta2 = (symbol1 <> symbol2) == symbolProduct
  where
    symbol1 = cubicSymbol alpha beta1
    symbol2 = cubicSymbol alpha beta2
    symbolProduct = cubicSymbol alpha betaProduct
    betaProduct = beta1 * beta2

testSuite :: TestTree
testSuite = testGroup "CubicSymbol"
  [ testSmallAndQuick "multiplicative property of numerators" cubicSymbol1
  , testSmallAndQuick "multiplicative property of denominators" cubicSymbol2
  ]
