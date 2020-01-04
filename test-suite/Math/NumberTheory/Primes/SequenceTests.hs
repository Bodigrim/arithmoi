{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Math.NumberTheory.Primes.SequenceTests
  ( testSuite
  ) where

import Test.Tasty

import Data.Bits
import Data.Maybe
import Data.Proxy
import Numeric.Natural

import Math.NumberTheory.Primes
import Math.NumberTheory.Primes.Counting (nthPrime, primeCount)
import Math.NumberTheory.TestUtils

nextPrimeProperty
  :: (Bits a, Integral a, UniqueFactorisation a)
  => AnySign a
  -> Bool
nextPrimeProperty (AnySign n) = unPrime (nextPrime n) >= n

precPrimeProperty
  :: (Bits a, Integral a, UniqueFactorisation a)
  => Positive a
  -> Bool
precPrimeProperty (Positive n) = n <= 2 || unPrime (precPrime n) <= n

toEnumProperty
  :: forall a.
     (Enum (Prime a), Integral a)
  => Proxy a
  -> Int
  -> Bool
toEnumProperty _ n = n <= 0 || unPrime (toEnum n :: Prime a) == fromInteger (unPrime (nthPrime (toInteger n)))

fromEnumProperty
  :: (Enum (Prime a), Integral a)
  => Prime a
  -> Bool
fromEnumProperty p = fromEnum p == fromInteger (primeCount (toInteger (unPrime p)))

succProperty
  :: (Enum a, Enum (Prime a), Num a, UniqueFactorisation a)
  => Prime a
  -> Bool
succProperty p = all (isNothing . isPrime) [unPrime p + 1 .. unPrime (succ p) - 1]

predProperty
  :: (Enum a, Enum (Prime a), Ord a, Num a, UniqueFactorisation a)
  => Prime a
  -> Bool
predProperty p = unPrime p <= 2 || all (isNothing . isPrime) [unPrime (pred p) + 1 .. unPrime p - 1]

enumFromProperty
  :: (Ord a, Enum (Prime a))
  => Prime a
  -> Prime a
  -> Bool
enumFromProperty p q = [p..q] == takeWhile (<= q) [p..]

enumFromToProperty
  :: (Eq a, Enum a, Enum (Prime a), UniqueFactorisation a)
  => Prime a
  -> Prime a
  -> Bool
enumFromToProperty p q = [p..q] == mapMaybe isPrime [unPrime p .. unPrime q]

enumFromThenProperty
  :: (Show a, Ord a, Enum (Prime a))
  => Prime a
  -> Prime a
  -> Prime a
  -> Bool
enumFromThenProperty p q r = case p `compare` q of
  LT -> enumFromThenTo p q r == takeWhile (<= r) (enumFromThen p q)
  EQ -> True
  GT -> enumFromThenTo p q r == takeWhile (>= r) (enumFromThen p q)

enumFromThenToProperty
  :: (Ord a, Enum a, Enum (Prime a), UniqueFactorisation a, Show a)
  => Prime a
  -> Prime a
  -> Prime a
  -> Bool
enumFromThenToProperty p q r
  | p == q && q <= r = True
  | otherwise
  = [p, q .. r] == mapMaybe isPrime [unPrime p, unPrime q .. unPrime r]

testSuite :: TestTree
testSuite = testGroup "Sequence"
  [ testIntegralPropertyNoLarge "nextPrime" nextPrimeProperty
  , testIntegralPropertyNoLarge "precPrime" precPrimeProperty
  , testGroup "toEnum"
    [ testSmallAndQuick "Int" (toEnumProperty (Proxy @Int))
    , testSmallAndQuick "Word" (toEnumProperty (Proxy @Word))
    , testSmallAndQuick "Integer" (toEnumProperty (Proxy @Integer))
    , testSmallAndQuick "Natural" (toEnumProperty (Proxy @Natural))
    ]
  , testGroup "fromEnum"
    [ testSmallAndQuick "Int" (fromEnumProperty @Int)
    , testSmallAndQuick "Word" (fromEnumProperty @Word)
    , testSmallAndQuick "Integer" (fromEnumProperty @Integer)
    , testSmallAndQuick "Natural" (fromEnumProperty @Natural)
    ]
  , testGroup "succ"
    [ testSmallAndQuick "Int" (succProperty @Int)
    , testSmallAndQuick "Word" (succProperty @Word)
    , testSmallAndQuick "Integer" (succProperty @Integer)
    , testSmallAndQuick "Natural" (succProperty @Natural)
    ]
  , testGroup "pred"
    [ testSmallAndQuick "Int" (predProperty @Int)
    , testSmallAndQuick "Word" (predProperty @Word)
    , testSmallAndQuick "Integer" (predProperty @Integer)
    , testSmallAndQuick "Natural" (predProperty @Natural)
    ]
  , testGroup "enumFrom"
    [ testSmallAndQuick "Int" (enumFromProperty @Int)
    , testSmallAndQuick "Word" (enumFromProperty @Word)
    , testSmallAndQuick "Integer" (enumFromProperty @Integer)
    , testSmallAndQuick "Natural" (enumFromProperty @Natural)
    ]
  , testGroup "enumFromTo"
    [ testSmallAndQuick "Int" (enumFromToProperty @Int)
    , testSmallAndQuick "Word" (enumFromToProperty @Word)
    , testSmallAndQuick "Integer" (enumFromToProperty @Integer)
    , testSmallAndQuick "Natural" (enumFromToProperty @Natural)
    ]
  , testGroup "enumFromThen"
    [ testSmallAndQuick "Int" (enumFromThenProperty @Int)
    , testSmallAndQuick "Word" (enumFromThenProperty @Word)
    , testSmallAndQuick "Integer" (enumFromThenProperty @Integer)
    , testSmallAndQuick "Natural" (enumFromThenProperty @Natural)
    ]
  , testGroup "enumFromThenTo"
    [ testSmallAndQuick "Int" (enumFromThenToProperty @Int)
    , testSmallAndQuick "Word" (enumFromThenToProperty @Word)
    , testSmallAndQuick "Integer" (enumFromThenToProperty @Integer)
    , testSmallAndQuick "Natural" (enumFromThenToProperty @Natural)
    ]
  ]
