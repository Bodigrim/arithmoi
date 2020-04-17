module Math.NumberTheory.Moduli.CubicSymbolTests
  ( testSuite
  ) where

import Math.NumberTheory.Moduli.CubicSymbol
import Math.NumberTheory.Quadratic.EisensteinIntegers
import Math.NumberTheory.Primes
import qualified Data.Euclidean as A
import Data.List
import Test.Tasty
import Math.NumberTheory.TestUtils

-- Checks multiplicative property of the numerators
cubicSymbol1 :: EisensteinInteger -> EisensteinInteger -> EisensteinInteger -> Bool
cubicSymbol1 alpha1 alpha2 beta = isBadDenominator beta || cubicSymbolNumerator alpha1 alpha2 beta

cubicSymbolNumerator :: EisensteinInteger -> EisensteinInteger -> EisensteinInteger -> Bool
cubicSymbolNumerator alpha1 alpha2 beta = (symbol1 <> symbol2) == symbolProduct
  where
    symbol1 = cubicSymbol alpha1 beta
    symbol2 = cubicSymbol alpha2 beta
    symbolProduct = cubicSymbol alphaProduct beta
    alphaProduct = alpha1 * alpha2

-- Checks multiplicative property of the denominators
cubicSymbol2 :: EisensteinInteger -> EisensteinInteger -> EisensteinInteger -> Bool
cubicSymbol2 alpha beta1 beta2 = isBadDenominator beta1 || isBadDenominator beta2 || cubicSymbolDenominator alpha beta1 beta2

cubicSymbolDenominator :: EisensteinInteger -> EisensteinInteger -> EisensteinInteger -> Bool
cubicSymbolDenominator alpha beta1 beta2 = (symbol1 <> symbol2) == symbolProduct
  where
    symbol1 = cubicSymbol alpha beta1
    symbol2 = cubicSymbol alpha beta2
    symbolProduct = cubicSymbol alpha betaProduct
    betaProduct = beta1 * beta2

-- Checks that the cubic symbol is correct when the denominator is primebeta
-- as explanined in § 3.3.2 in https://en.wikipedia.org/wiki/Cubic_reciprocity
cubicSymbol3 :: EisensteinInteger -> Prime EisensteinInteger -> Bool
cubicSymbol3 alpha prime = isBadDenominator beta || isNotDivisible || cubicSymbol alpha beta == cubicSymbolPrime alpha beta
    where beta = unPrime prime
          isNotDivisible = alpha `A.rem` beta == 0

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
-- which give a well defined cubicSymbol.
findCubicSymbol :: EisensteinInteger -> EisensteinInteger -> CubicSymbol
findCubicSymbol residue beta
  | residue `A.rem` beta == 0             = Zero
  | (residue - ω) `A.rem` beta == 0       = Omega
  | (residue + 1 + ω) `A.rem` beta == 0   = OmegaSquare
  | (residue - 1) `A.rem` beta == 0       = One
  | otherwise                             = error "Math.NumberTheory.Moduli.CubicSymbol: invalid EisensteinInteger."

testSuite :: TestTree
testSuite = testGroup "CubicSymbol"
  [ testSmallAndQuick "multiplicative property of numerators" cubicSymbol1
  , testSmallAndQuick "multiplicative property of denominators" cubicSymbol2
  , testSmallAndQuick "cubic residue with prime denominator" cubicSymbol3
  ]
