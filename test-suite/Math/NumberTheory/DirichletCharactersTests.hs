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
import Data.List (genericLength, genericReplicate)
import Data.Maybe (mapMaybe, isJust)

import GHC.TypeNats.Compat (SomeNat(..), someNatVal, KnownNat, natVal)

import Math.NumberTheory.ArithmeticFunctions (totient)
import Math.NumberTheory.DirichletCharacters
import Math.NumberTheory.Moduli.Jacobi
import Math.NumberTheory.Moduli.Class (SomeMod(..), modulo)
import Math.NumberTheory.TestUtils (testSmallAndQuick, Positive(..))

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

-- | The principal character should be 1 at all phi(n) places
principalCase :: Positive Natural -> Bool
principalCase (Positive n) =
  case someNatVal n of
    SomeNat (Proxy :: Proxy n) ->
      mapMaybe (generalEval chi) [minBound..maxBound] == genericReplicate (totient n) mempty
        where chi = principalChar @n

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
      where total = sum [maybe 0 toComplex (generalEval chi a') | chi <- allChars]
            correct = if a' == 1
                         then fromIntegral $ totient n
                         else 0
    InfMod {} -> False

-- | Manually confirm isRealCharacter is correct (in both directions)
realityCheck :: KnownNat n => DirichletCharacter n -> Bool
realityCheck chi = isJust (isRealCharacter chi) == isReal'
  where isReal' = and [real (generalEval chi t) | t <- [minBound..maxBound]]
        real Nothing = True
        real (Just t) = t <> t == mempty

-- | Induced characters agree with the original character.
inducedCheck :: forall d. KnownNat d => DirichletCharacter d -> Positive Natural -> Bool
inducedCheck chi (Positive k) =
  case someNatVal (d*k) of
    SomeNat (Proxy :: Proxy n) ->
      case chi2 of
        Just chi2' -> and [generalEval chi2' (fromIntegral j) == generalEval chi (fromIntegral j) | j <- [0..d*k-1], gcd j (d*k) == 1]
        _ -> False
        where chi2 = induced @n chi
  where d = natVal @d Proxy

-- | The jacobi character agrees with the jacobi symbol
jacobiCheck :: Positive Natural -> Bool
jacobiCheck (Positive n) =
  case someNatVal (2*n+1) of
    SomeNat (Proxy :: Proxy n) ->
      case jacobiCharacter @n of
        Just chi -> and [toRealFunction chi (fromIntegral j) == symbolToIntegral (jacobi j (2*n+1)) | j <- [0..2*n]]
        _ -> False

-- | Primitive checker is correct (in both directions)
primitiveCheck :: forall n. KnownNat n => DirichletCharacter n -> Bool
primitiveCheck = (==) <$> isPrimitive <*> isPrimitive'
  where isPrimitive' chi = error "TODO: this"
        n = natVal @n Proxy

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
  -- , testSmallAndQuick "Primitive character checking is valid" (dirCharProperty primitiveCheck)
  ]
