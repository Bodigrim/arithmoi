-- |
-- Module:      Math.NumberTheory.Primes.Sequence
-- Copyright:   (c) 2016-2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Math.NumberTheory.Primes.Sequence
  () where

import Data.Bits
import Data.Coerce
import Data.Maybe
import Numeric.Natural

import Math.NumberTheory.Primes.Counting
import Math.NumberTheory.Primes.Types
import Math.NumberTheory.Primes.Sieve.Eratosthenes
import Math.NumberTheory.Primes
import Math.NumberTheory.Utils.FromIntegral
import Math.NumberTheory.Utils (toWheel30, fromWheel30)

data Algorithm = IsPrime | Sieve

chooseAlgorithm :: Integral a => a -> a -> Algorithm
chooseAlgorithm from to
  | to <= fromIntegral sieveRange
  && to < from + truncate (sqrt (fromIntegral from) :: Double)
  = IsPrime
  | to > fromIntegral sieveRange
  && to < from + truncate (0.036 * sqrt (fromIntegral from) + 40000 :: Double)
  = IsPrime
  | otherwise
  = Sieve

succGeneric :: (Bits a, Integral a, UniqueFactorisation a) => Prime a -> Prime a
succGeneric = \case
  Prime 2 -> Prime 3
  Prime 3 -> Prime 5
  Prime 5 -> Prime 7
  Prime p -> head $ mapMaybe isPrime $ map fromWheel30 [toWheel30 p + 1 ..]

predGeneric :: (Bits a, Integral a, UniqueFactorisation a) => Prime a -> Prime a
predGeneric = \case
  Prime 2 -> error "Enum.pred{Prime}: tried to take `pred' of 2"
  Prime 3 -> Prime 2
  Prime 5 -> Prime 3
  Prime 7 -> Prime 5
  Prime p -> head $ mapMaybe isPrime $ map fromWheel30 [toWheel30 p - 1, toWheel30 p - 2 ..]

-- 'dropWhile' is important, because 'psieveFrom' can actually contain primes less than p.
enumFromGeneric :: Integral a => Prime a -> [Prime a]
enumFromGeneric p@(Prime p')
  = coerce
  $ dropWhile (< p)
  $ concatMap primeList
  $ psieveFrom
  $ toInteger p'

enumFromToGeneric :: (Bits a, Integral a, UniqueFactorisation a) => Prime a -> Prime a -> [Prime a]
enumFromToGeneric p@(Prime p') q@(Prime q') = takeWhile (<= q) $ dropWhile (< p) $
  case chooseAlgorithm p' q' of
    IsPrime -> Prime 2 : Prime 3 : Prime 5 : mapMaybe isPrime (map fromWheel30 [toWheel30 p' .. toWheel30 q'])
    Sieve   ->
      if q' < fromIntegral sieveRange
      then           primeList $ primeSieve $ toInteger q'
      else concatMap primeList $ psieveFrom $ toInteger p'

enumFromThenGeneric :: (Bits a, Integral a, UniqueFactorisation a) => Prime a -> Prime a -> [Prime a]
enumFromThenGeneric p@(Prime p') (Prime q') = case p' `compare` q' of
  LT -> filter (\(Prime r') -> (r' - p') `mod` delta == 0) $ enumFromGeneric p
    where
      delta = q' - p'
  EQ -> repeat p
  GT -> filter (\(Prime r') -> (p' - r') `mod` delta == 0) $ reverse $ enumFromToGeneric (Prime 2) p
    where
      delta = p' - q'

enumFromThenToGeneric :: (Bits a, Integral a, UniqueFactorisation a) => Prime a -> Prime a -> Prime a -> [Prime a]
enumFromThenToGeneric p@(Prime p') (Prime q') r = case p' `compare` q' of
  LT -> filter (\(Prime r') -> (r' - p') `mod` delta == 0) $ enumFromToGeneric p r
    where
      delta = q' - p'
  EQ -> repeat p
  GT -> filter (\(Prime r') -> (p' - r') `mod` delta == 0) $ reverse $ enumFromToGeneric (Prime 2) p
    where
      delta = p' - q'

instance Enum (Prime Integer) where
  toEnum = nthPrime . intToInteger
  fromEnum = integerToInt . primeCount . unPrime
  succ = succGeneric
  pred = predGeneric
  enumFrom = enumFromGeneric
  enumFromTo = enumFromToGeneric
  enumFromThen = enumFromThenGeneric
  enumFromThenTo = enumFromThenToGeneric

instance Enum (Prime Natural) where
  toEnum = Prime . integerToNatural . unPrime . nthPrime . intToInteger
  fromEnum = integerToInt . primeCount . naturalToInteger . unPrime
  succ = succGeneric
  pred = predGeneric
  enumFrom = enumFromGeneric
  enumFromTo = enumFromToGeneric
  enumFromThen = enumFromThenGeneric
  enumFromThenTo = enumFromThenToGeneric

instance Enum (Prime Int) where
  toEnum n = if p > intToInteger maxBound
    then error $ "Enum.toEnum{Prime}: " ++ show n ++ "th prime = " ++ show p ++ " is out of bounds of Int"
    else Prime (integerToInt p)
    where
      Prime p = nthPrime (intToInteger n)
  fromEnum = integerToInt . primeCount . intToInteger . unPrime
  succ = succGeneric -- FIXME
  pred = predGeneric
  enumFrom = enumFromGeneric -- FIXME
  enumFromTo = enumFromToGeneric
  enumFromThen = enumFromThenGeneric -- FIXME
  enumFromThenTo = enumFromThenToGeneric

instance Enum (Prime Word) where
  toEnum n = if p > wordToInteger maxBound
    then error $ "Enum.toEnum{Prime}: " ++ show n ++ "th prime = " ++ show p ++ " is out of bounds of Word"
    else Prime (integerToWord p)
    where
      Prime p = nthPrime (intToInteger n)
  fromEnum = integerToInt . primeCount . wordToInteger . unPrime
  succ = succGeneric -- FIXME
  pred = predGeneric
  enumFrom = enumFromGeneric -- FIXME
  enumFromTo = enumFromToGeneric
  enumFromThen = enumFromThenGeneric -- FIXME
  enumFromThenTo = enumFromThenToGeneric
