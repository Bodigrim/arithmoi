module Math.NumberTheory.Moduli.CubicSymbolTests
  ( testSuite
  ) where

import qualified Math.NumberTheory.Moduli.CubicSymbol as C
import qualified Math.NumberTheory.Quadratic.EisensteinIntegers as E
import Test.Tasty                                     (TestTree, testGroup)
import Math.NumberTheory.TestUtils

-- Checks multiplicative property of the numerator
cubicSymbol1 :: E.EisensteinInteger -> E.EisensteinInteger -> E.EisensteinInteger -> Bool
cubicSymbol1 alpha1 alpha2 beta = (modularNorm == 0) || cubicSymbolNumerator alpha1 alpha2 beta
    where modularNorm = norm `mod` 3
          norm = E.norm beta

cubicSymbolNumerator :: E.EisensteinInteger -> E.EisensteinInteger -> E.EisensteinInteger -> Bool
cubicSymbolNumerator alpha1 alpha2 beta = (symbol1 <> symbol2) == symbolProduct
    where symbol1 = C.cubicSymbol alpha1 beta
          symbol2 = C.cubicSymbol alpha2 beta
          symbolProduct = C.cubicSymbol alphaProduct beta
          alphaProduct = alpha1 * alpha2

-- Checks multiplicative property of the denominator
cubicSymbol2 :: E.EisensteinInteger -> E.EisensteinInteger -> E.EisensteinInteger -> Bool
cubicSymbol2 alpha beta1 beta2 = (modularNorm1 == 0) || (modularNorm2 == 0) || cubicSymbolDenominator alpha beta1 beta2
    where (modularNorm1, modularNorm2) = (norm1 `mod` 3, norm2 `mod` 3)
          (norm1, norm2) = (E.norm beta1, E.norm beta2)

cubicSymbolDenominator :: E.EisensteinInteger -> E.EisensteinInteger -> E.EisensteinInteger -> Bool
cubicSymbolDenominator alpha beta1 beta2 = (symbol1 <> symbol2) == symbolProduct
    where symbol1 = C.cubicSymbol alpha beta1
          symbol2 = C.cubicSymbol alpha beta2
          symbolProduct = C.cubicSymbol alpha betaProduct
          betaProduct = beta1 * beta2

testSuite :: TestTree
testSuite = testGroup "CubicSymbol" $
    [ testSmallAndQuick "Multiplicative property of numerator." cubicSymbol1
    , testSmallAndQuick "Multiplicative property of denominator." cubicSymbol2
    ]
