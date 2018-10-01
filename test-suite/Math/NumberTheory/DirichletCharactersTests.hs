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

module Math.NumberTheory.DirichletCharactersTests where

import Test.Tasty

import Data.Proxy
import Data.Ratio
import Numeric.Natural
import Data.Semigroup
import Data.Complex

import GHC.TypeNats.Compat (SomeNat(..), someNatVal)

import Math.NumberTheory.ArithmeticFunctions (totient)
import Math.NumberTheory.DirichletCharacters
-- import Math.NumberTheory.Moduli (Mod, getNatVal)
import Math.NumberTheory.TestUtils (testSmallAndQuick, Positive(..))

rootOfUnityTest :: Integer -> Positive Integer -> Bool
rootOfUnityTest n (Positive d) = toComplex ((d `div` gcd n d) `stimes` toRootOfUnity (n % d)) == (1 :: Complex Double)

-- | This tests property 6 from https://en.wikipedia.org/wiki/Dirichlet_character#Axiomatic_definition
dirCharOrder :: Positive Natural -> Natural -> Bool
dirCharOrder (Positive n) i = case someNatVal n of
                                SomeNat (Proxy :: Proxy n) -> (totient n) `stimes` chi == trivialChar
                                  where chi = fromIndex (i `mod` (totient n)) :: DirichletCharacter n

-- | Tests wikipedia's property 3 (note 1,2,5 are essentially enforced by the type system).
dirCharMultiplicative :: Positive Natural -> Natural -> Natural -> Natural -> Bool
dirCharMultiplicative (Positive n) i a b = case someNatVal n of
                                             SomeNat (Proxy :: Proxy n) -> let chiAchiB = (<>) <$> chi' a' <*> chi' b'
                                                                               chiAB = chi' (a'*b')
                                                                            in chiAB == chiAchiB
                                               where chi = fromIndex (i `mod` (totient n)) :: DirichletCharacter n
                                                     chi' = generalEval chi
                                                     a' = fromIntegral a
                                                     b' = fromIntegral b

dirCharAtOne :: Positive Natural -> Natural -> Bool
dirCharAtOne (Positive n) i = case someNatVal n of
                                SomeNat (Proxy :: Proxy n) -> evaluate chi mempty == mempty
                                  where chi = fromIndex (i `mod` (totient n)) :: DirichletCharacter n

testSuite :: TestTree
testSuite = testGroup "DirichletCharacters"
  [ testSmallAndQuick "RootOfUnity contains roots of unity" rootOfUnityTest
  , testSmallAndQuick "Dirichlet characters have the right order" dirCharOrder
  , testSmallAndQuick "Dirichlet characters are multiplicative" dirCharMultiplicative
  , testSmallAndQuick "Dirichlet characters are 1 at 1" dirCharMultiplicative
  ]
