-- |
-- Module:       Math.NumberTheory.Moduli.DiscreteLogarithm
-- Copyright:    (c) 2018 Bhavik Mehta
-- License:      MIT
-- Maintainer:   Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Tests for Math.NumberTheory.DirichletCharacters
--

{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Math.NumberTheory.DirichletCharactersTests where

import Test.Tasty

import Data.Complex
import Data.List (genericLength)
import Data.Maybe (isJust, mapMaybe)
import Data.Proxy
import Data.Ratio
import Data.Semigroup
import qualified Data.Vector as V
import Numeric.Natural

import GHC.TypeNats.Compat (SomeNat(..), someNatVal, KnownNat, natVal, sameNat)
import Data.Type.Equality

import Math.NumberTheory.ArithmeticFunctions (totient, divisorsList)
import Math.NumberTheory.DirichletCharacters
import qualified Math.NumberTheory.Moduli.Sqrt as J
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
  where chi' = evalGeneral chi
        chiAB = chi' (a*b)
        chiAchiB = (<>) <$> chi' a <*> chi' b

-- | Test property 4 from wikipedia
testAtOne :: KnownNat n => DirichletCharacter n -> Bool
testAtOne chi = eval chi mempty == mempty

dirCharProperty :: (forall n. KnownNat n => DirichletCharacter n -> a) -> Positive Natural -> Natural -> a
dirCharProperty test (Positive n) i =
  case someNatVal n of
    SomeNat (Proxy :: Proxy n) -> test chi
      where chi = indexToChar @n (i `mod` totient n)

realCharProperty :: (forall n. KnownNat n => RealCharacter n -> a) -> Positive Natural -> Int -> a
realCharProperty test (Positive n) i =
  case someNatVal n of
    SomeNat (Proxy :: Proxy n) -> test chi
      where chi = chars !! (i `mod` length chars)
            chars = mapMaybe isRealCharacter [principalChar @n .. maxBound]

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
    SomeMod a -> evalGeneral chi a == if gcd k (fromIntegral n) > 1
                                         then Zero
                                         else mempty
      where chi = principalChar
    InfMod{} -> False

-- | Test the orthogonality relations https://en.wikipedia.org/wiki/Dirichlet_character#Character_orthogonality
orthogonality1 :: forall n. KnownNat n => DirichletCharacter n -> Bool
orthogonality1 chi = magnitude (total - correct) < (1e-13 :: Double)
  where n = natVal @n Proxy
        total = sum [orZeroToNum toComplex (evalGeneral chi a) | a <- [0 .. maxBound]]
        correct = if isPrincipal chi
                     then fromIntegral $ totient n
                     else 0

orthogonality2 :: Positive Natural -> Integer -> Bool
orthogonality2 (Positive n) a =
  case a `modulo` n of
    SomeMod a' -> magnitude (total - correct) < (1e-13 :: Double)
      where total = sum [orZeroToNum toComplex (evalGeneral chi a') | chi <- allChars]
            correct = if a' == 1
                         then fromIntegral $ totient n
                         else 0
    InfMod {} -> False

-- | Manually confirm isRealCharacter is correct (in both directions)
realityCheck :: KnownNat n => DirichletCharacter n -> Bool
realityCheck chi = isJust (isRealCharacter chi) == isReal'
  where isReal' = and [real (evalGeneral chi t) | t <- [minBound..maxBound]]
        real Zero = True
        real (NonZero t) = t <> t == mempty

-- | Check real character evaluation matches normal evaluation
realEvalCheck :: KnownNat n => RealCharacter n -> Int -> Bool
realEvalCheck chi i' = fromIntegral (toRealFunction chi i) == (orZeroToNum toComplex (evalGeneral (getRealChar chi) i) :: Complex Double)
  where i = fromIntegral i'

-- | The jacobi character agrees with the jacobi symbol
jacobiCheck :: Positive Natural -> Bool
jacobiCheck (Positive n) =
  case someNatVal (2*n+1) of
    SomeNat (Proxy :: Proxy n) ->
      case jacobiCharacter @n of
        Just chi -> and [toRealFunction chi (fromIntegral j) == J.symbolToNum (J.jacobi j (2*n+1)) | j <- [0..2*n]]
        _ -> False

-- | Bulk evaluation agrees with pointwise evaluation
evalAllCheck :: forall n. KnownNat n => DirichletCharacter n -> Bool
evalAllCheck chi = V.generate (fromIntegral $ natVal @n Proxy) (evalGeneral chi . fromIntegral) == evalAll chi

-- | Induced characters agree with the original character.
-- (Except for when d=1, where chi(0) = 1, which is true for no other d)
inducedCheck :: forall d. KnownNat d => DirichletCharacter d -> Positive Natural -> Bool
inducedCheck chi (Positive k) =
  case someNatVal (d*k) of
    SomeNat (Proxy :: Proxy n) ->
      case induced @n chi of
        Just chi2 -> and (V.izipWith matchedValue (V.concat (replicate (fromIntegral k) (evalAll chi))) (evalAll chi2))
        Nothing -> False
  where d = natVal @d Proxy
        matchedValue i x1 x2 = if gcd (fromIntegral i) (d*k) > 1
                                  then x2 == Zero
                                  else x2 == x1

-- | Primitive checker is correct (in both directions)
primitiveCheck :: forall n. KnownNat n => DirichletCharacter n -> Bool
primitiveCheck chi = isJust (isPrimitive chi) == isPrimitive'
  where isPrimitive' = all testModulus possibleModuli
        n = fromIntegral (natVal @n Proxy) :: Int
        possibleModuli = init (divisorsList n)
        table = evalAll chi
        testModulus d = not $ null [a | a <- [1..n-1], gcd a n == 1, a `mod` d == 1 `mod` d, table V.! a /= mempty]

-- | Ensure that makePrimitive gives primitive characters
makePrimitiveCheck :: DirichletCharacter n -> Bool
makePrimitiveCheck chi = case makePrimitive chi of
                            WithNat chi' -> isJust (isPrimitive (getPrimitiveChar chi'))

-- | sameNat also ensures the two new moduli are the same
makePrimitiveIdem :: DirichletCharacter n -> Bool
makePrimitiveIdem chi = case makePrimitive chi of
                          WithNat (chi' :: PrimitiveCharacter n') ->
                            case makePrimitive (getPrimitiveChar chi') of
                              WithNat (chi'' :: PrimitiveCharacter n'') ->
                                case sameNat (Proxy :: Proxy n') (Proxy :: Proxy n'') of
                                  Just Refl -> chi' == chi''
                                  Nothing -> False

orderCheck :: DirichletCharacter n -> Bool
orderCheck chi = isPrincipal (n `stimes` chi) && and [not (isPrincipal (i `stimes` chi)) | i <- [1..n-1]]
  where n = orderChar chi

fromTableCheck :: forall n. KnownNat n => DirichletCharacter n -> Bool
fromTableCheck chi = isJust (fromTable @n (evalAll chi))

-- A bunch of functions making sure that every function which can produce a character (in
-- particular by fiddling internal representation) produces a valid character
indexToCharValid :: KnownNat n => DirichletCharacter n -> Bool
indexToCharValid = validChar

principalCharValid :: Positive Natural -> Bool
principalCharValid (Positive n) =
  case someNatVal n of
    SomeNat (Proxy :: Proxy n) -> validChar (principalChar @n)

mulCharsValid :: KnownNat n => DirichletCharacter n -> DirichletCharacter n -> Bool
mulCharsValid chi1 chi2 = validChar (chi1 <> chi2)

mulCharsValid' :: Positive Natural -> Natural -> Natural -> Bool
mulCharsValid' (Positive n) i j =
  case someNatVal n of
    SomeNat (Proxy :: Proxy n) ->
      mulCharsValid (indexToChar @n (i `mod` totient n)) (indexToChar @n (j `mod` totient n))

stimesCharValid :: KnownNat n => DirichletCharacter n -> Int -> Bool
stimesCharValid chi n = validChar (n `stimes` chi)

succValid :: KnownNat n => DirichletCharacter n -> Bool
succValid = validChar . succ

inducedValid :: forall d. KnownNat d => DirichletCharacter d -> Positive Natural -> Bool
inducedValid chi (Positive k) =
  case someNatVal (d*k) of
    SomeNat (Proxy :: Proxy n) ->
      case induced @n chi of
        Just chi2 -> validChar chi2
        Nothing -> False
  where d = natVal @d Proxy

jacobiValid :: Positive Natural -> Bool
jacobiValid (Positive n) =
  case someNatVal (2*n+1) of
    SomeNat (Proxy :: Proxy n) ->
      case jacobiCharacter @n of
        Just chi -> validChar (getRealChar chi)
        _ -> False

makePrimitiveValid :: DirichletCharacter n -> Bool
makePrimitiveValid chi = case makePrimitive chi of
                            WithNat chi' -> validChar (getPrimitiveChar chi')

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
  , testSmallAndQuick "Real character checking is correct" (dirCharProperty realityCheck)
  , testSmallAndQuick "Real character evaluation is accurate" (realCharProperty realEvalCheck)
  , testSmallAndQuick "Jacobi character matches symbol" jacobiCheck
  , testSmallAndQuick "Bulk evaluation matches pointwise" (dirCharProperty evalAllCheck)
  , testSmallAndQuick "Induced character is correct" (dirCharProperty inducedCheck)
  , testSmallAndQuick "Primitive character checking is correct" (dirCharProperty primitiveCheck)
  , testSmallAndQuick "makePrimitive produces primitive character" (dirCharProperty makePrimitiveCheck)
  , testSmallAndQuick "makePrimitive is idempotent" (dirCharProperty makePrimitiveIdem)
  , testSmallAndQuick "Calculates correct order" (dirCharProperty orderCheck)
  , testSmallAndQuick "Can construct from table" (dirCharProperty fromTableCheck)
  , testGroup "Creates valid characters"
    [ testSmallAndQuick "indexToChar" (dirCharProperty indexToCharValid)
    , testSmallAndQuick "principalChar" principalCharValid
    , testSmallAndQuick "mulChars" mulCharsValid'
    , testSmallAndQuick "stimesChar" (dirCharProperty stimesCharValid)
    , testSmallAndQuick "succ" (dirCharProperty succValid)
    , testSmallAndQuick "induced" (dirCharProperty inducedValid)
    , testSmallAndQuick "jacobi" jacobiValid
    , testSmallAndQuick "makePrimitive" (dirCharProperty makePrimitiveValid)
    ]
  ]
