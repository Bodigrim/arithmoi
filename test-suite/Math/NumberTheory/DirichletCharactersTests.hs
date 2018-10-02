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

module Math.NumberTheory.DirichletCharactersTests where

import Test.Tasty

import Data.Proxy
import Data.Ratio
import Numeric.Natural
import Data.Semigroup
import Data.Complex
import Data.List (nub, genericLength, genericReplicate)
import Data.Maybe (mapMaybe)

import GHC.TypeNats.Compat (SomeNat(..), someNatVal, KnownNat)

import Math.NumberTheory.ArithmeticFunctions (totient)
import Math.NumberTheory.DirichletCharacters
-- import Math.NumberTheory.Moduli (Mod, getNatVal)
import Math.NumberTheory.TestUtils (testSmallAndQuick, Positive(..))

rootOfUnityTest :: Integer -> Positive Integer -> Bool
rootOfUnityTest n (Positive d) = toComplex ((d `div` gcd n d) `stimes` toRootOfUnity (n % d)) == (1 :: Complex Double)

-- | This tests property 6 from https://en.wikipedia.org/wiki/Dirichlet_character#Axiomatic_definition
dirCharOrder :: Positive Natural -> Natural -> Bool
dirCharOrder (Positive n) i = case someNatVal n of
                                SomeNat (Proxy :: Proxy n) -> (totient n) `stimes` chi == principalChar
                                  where chi = fromIndex (i `mod` (totient n)) :: DirichletCharacter n

-- | Tests wikipedia's property 3 (note 1,2,5 are essentially enforced by the type system).
testMultiplicative :: KnownNat n => DirichletCharacter n -> Natural -> Natural -> Bool
testMultiplicative chi a b = chiAB == chiAchiB
  where chi' = generalEval chi
        a' = fromIntegral a
        b' = fromIntegral b
        chiAB = chi' (a'*b')
        chiAchiB = (<>) <$> chi' a' <*> chi' b'

testAtOne :: KnownNat n => DirichletCharacter n -> Bool
testAtOne chi = evaluate chi mempty == mempty

dirCharProperty :: (forall n. KnownNat n => DirichletCharacter n -> a) -> Positive Natural -> Natural -> a
dirCharProperty test (Positive n) i = case someNatVal n of
                                        SomeNat (Proxy :: Proxy n) -> test chi
                                          where chi = fromIndex (i `mod` (totient n)) :: DirichletCharacter n

countCharacters :: Positive Natural -> Bool
countCharacters (Positive n) = case someNatVal n of
                                 SomeNat (Proxy :: Proxy n) ->
                                   genericLength (nub [minBound :: DirichletCharacter n .. maxBound]) == totient n

principalCase :: Positive Natural -> Bool
principalCase (Positive n) = case someNatVal n of
                             SomeNat (Proxy :: Proxy n) -> mapMaybe (generalEval chi) [minBound..maxBound] == genericReplicate (totient n) mempty
                               where chi = principalChar :: DirichletCharacter n

testSuite :: TestTree
testSuite = testGroup "DirichletCharacters"
  [ testSmallAndQuick "RootOfUnity contains roots of unity" rootOfUnityTest
  , testSmallAndQuick "Dirichlet characters divide the right order" dirCharOrder
  , testSmallAndQuick "Dirichlet characters are multiplicative" (dirCharProperty testMultiplicative)
  , testSmallAndQuick "Dirichlet characters are 1 at 1" (dirCharProperty testAtOne)
  , testSmallAndQuick "Right number of Dirichlet characters" countCharacters
  , testSmallAndQuick "Principal character behaves as expected" principalCase
  ]
