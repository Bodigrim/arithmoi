-- |
-- Module:       Math.NumberTheory.Moduli.DiscreteLogarithm
-- Copyright:    (c) 2018 Bhavik Mehta
-- License:      MIT
-- Maintainer:   Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:    Provisional
-- Portability:  Non-portable
--
-- Tests for Math.NumberTheory.DirichletCharacters
--

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Math.NumberTheory.DirichletCharactersTests where

import Test.Tasty

import Data.Proxy
import Data.Ratio
import Numeric.Natural
import Data.Semigroup
import Data.Complex
import Data.List (genericLength)
import Data.Maybe (isJust)
import qualified Data.Vector as V

import GHC.TypeNats.Compat (SomeNat(..), someNatVal, KnownNat, natVal)

import Math.NumberTheory.ArithmeticFunctions (totient)
import Math.NumberTheory.DirichletCharacters
import qualified Math.NumberTheory.Moduli.Jacobi as J
import Math.NumberTheory.Moduli.Class (SomeMod(..), modulo)
import Math.NumberTheory.TestUtils (testSmallAndQuick, Positive(..))
import Math.NumberTheory.Primes

rootOfUnityTest :: Integer -> Positive Integer -> Bool
rootOfUnityTest n (Positive d) = toComplex ((d `div` gcd n d) `stimes` toRootOfUnity (n % d)) == (1 :: Complex Double)

-- | This tests property 6 from https://en.wikipedia.org/wiki/Dirichlet_character#Axiomatic_definition
dirCharOrder :: forall n. KnownNat n => DirichletCharacter n -> Bool
dirCharOrder chi = isPrincipal (totient n `stimes` chi)
  where n = natVal @n Proxy

-- | Tests wikipedia's property 3 (note 1,2,5 are essentially enforced by the type system).
testMultiplicative :: KnownNat n => DirichletCharacter n -> Natural -> Natural -> Bool
testMultiplicative chi (fromIntegral -> a) (fromIntegral -> b) = chiAB == chiAchiB
  where chi' = generalEval chi
        chiAB = chi' (a*b)
        chiAchiB = (<>) <$> chi' a <*> chi' b

-- | Test property 4 from wikipedia
testAtOne :: KnownNat n => DirichletCharacter n -> Bool
testAtOne chi = evaluate chi mempty == mempty

dirCharProperty :: (forall n. KnownNat n => DirichletCharacter n -> a) -> Positive Natural -> Natural -> a
dirCharProperty test (Positive n) i =
  case someNatVal n of
    SomeNat (Proxy :: Proxy n) -> test chi
      where chi = indexToChar @n (i `mod` totient n)

-- | There should be totient(n) characters
countCharacters :: Positive Natural -> Bool
countCharacters (Positive n) =
  case someNatVal n of
    SomeNat (Proxy :: Proxy n) ->
      genericLength (allChars @n) == totient n

-- | The principal character should be 1 if gcd k n is 1 and 0 otherwise
principalCase :: Positive Natural -> Positive Integer -> Bool
principalCase (Positive n) (Positive k) =
  case k `modulo` n of
    SomeMod a -> generalEval chi a == if gcd k (fromIntegral n) > 1
                                         then Zero
                                         else mempty
      where chi = principalChar
    InfMod{} -> False

-- | Test the orthogonality relations https://en.wikipedia.org/wiki/Dirichlet_character#Character_orthogonality
orthogonality1 :: forall n. KnownNat n => DirichletCharacter n -> Bool
orthogonality1 chi = magnitude (total - correct) < (1e-13 :: Double)
  where n = natVal @n Proxy
        total = sum [toFunction chi a | a <- [0..n-1]]
        correct = if isPrincipal chi
                     then fromIntegral $ totient n
                     else 0

orthogonality2 :: Positive Natural -> Integer -> Bool
orthogonality2 (Positive n) a =
  case a `modulo` n of
    SomeMod a' -> magnitude (total - correct) < (1e-13 :: Double)
      where total = sum [asNumber toComplex (generalEval chi a') | chi <- allChars]
            correct = if a' == 1
                         then fromIntegral $ totient n
                         else 0
    InfMod {} -> False

-- | Manually confirm isRealCharacter is correct (in both directions)
realityCheck :: KnownNat n => DirichletCharacter n -> Bool
realityCheck chi = isJust (isRealCharacter chi) == isReal'
  where isReal' = and [real (generalEval chi t) | t <- [minBound..maxBound]]
        real Zero = True
        real (NonZero t) = t <> t == mempty

-- | Induced characters agree with the original character.
inducedCheck :: forall d. KnownNat d => DirichletCharacter d -> Positive Natural -> Natural -> Bool
inducedCheck chi (Positive k) i =
  case someNatVal (d*k) of
    SomeNat (Proxy :: Proxy n) ->
      case induced @n chi of
        Just chi2 -> if (fromIntegral i) `gcd` (d*k) > 0
                        then True
                        else generalEval chi (fromIntegral i) == generalEval chi2 (fromIntegral i)
        Nothing -> False
  where d = natVal @d Proxy

-- | The jacobi character agrees with the jacobi symbol
jacobiCheck :: Positive Natural -> Bool
jacobiCheck (Positive n) =
  case someNatVal (2*n+1) of
    SomeNat (Proxy :: Proxy n) ->
      case jacobiCharacter @n of
        Just chi -> and [toRealFunction chi (fromIntegral j) == J.symbolToIntegral (J.jacobi j (2*n+1)) | j <- [0..2*n]]
        _ -> False


-- | Bulk evaluation agrees with pointwise evaluation
allEvalCheck :: forall n. KnownNat n => DirichletCharacter n -> Bool
allEvalCheck chi = V.generate (fromIntegral $ natVal @n Proxy) (generalEval chi . fromIntegral) == allEval chi

-- | Primitive checker is correct (in both directions)
-- primitiveCheck :: forall n. KnownNat n => DirichletCharacter n -> Bool
-- primitiveCheck = if n > 5
--                     then (==) <$> isPrimitive <*> isPrimitive'
--                     else const True
--   where isPrimitive' chi = not $ any (periodic (allEval chi)) primeFactors
--         n = fromIntegral (natVal @n Proxy)
--         primeFactors = map (unPrime . fst) $ factorise n
--         periodic v k = and [allEqual [v V.! j | j <- [i,i + n `div` k .. n-1]] | i <- [0..k-1]]
--         allEqual :: Eq a => [a] -> Bool
--         allEqual = and . (zipWith (==) <*> tail)

testSuite :: TestTree
testSuite = testGroup "DirichletCharacters"
  [ testSmallAndQuick "RootOfUnity contains roots of unity" rootOfUnityTest
  , testSmallAndQuick "Dirichlet characters divide the right order" (dirCharProperty dirCharOrder)
  , testSmallAndQuick "Dirichlet characters are multiplicative" (dirCharProperty testMultiplicative)
  , testSmallAndQuick "Dirichlet characters are 1 at 1" (dirCharProperty testAtOne)
  , testSmallAndQuick "Right number of Dirichlet characters" countCharacters
  , testSmallAndQuick "Principal character behaves as expected" principalCase
  , testSmallAndQuick "Orthogonality relation 1" (dirCharProperty orthogonality1)
  , testSmallAndQuick "Orthogonality relation 2" orthogonality2
  , testSmallAndQuick "Real character checking is valid" (dirCharProperty realityCheck)
  , testSmallAndQuick "Jacobi character matches symbol" jacobiCheck
  , testSmallAndQuick "Induced character is correct" (dirCharProperty inducedCheck)
  , testSmallAndQuick "Bulk evaluation matches pointwise" (dirCharProperty allEvalCheck)
  -- , testSmallAndQuick "Primitive character checking is valid" (dirCharProperty primitiveCheck)
  ]
