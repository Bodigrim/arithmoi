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
  ( PrimeSequence(..)
  ) where

import Data.Bits
import Data.Coerce
import Numeric.Natural

import Math.NumberTheory.Primes.Counting
import Math.NumberTheory.Primes.Types
import Math.NumberTheory.Primes.Sieve.Eratosthenes
import Math.NumberTheory.Primes.Testing
import Math.NumberTheory.Utils.FromIntegral

data Algorithm = IsPrime | Sieve

chooseAlgorithm :: Natural -> Natural -> Algorithm
chooseAlgorithm from to
  | to <= fromIntegral sieveRange
  && to < from + truncate (sqrt (fromIntegral from) :: Double)
  = IsPrime
  | to > fromIntegral sieveRange
  && to < from + truncate (0.036 * sqrt (fromIntegral from) + 40000 :: Double)
  = IsPrime
  | otherwise
  = Sieve

instance Enum PrimeNat where
  toEnum   = PrimeNat . fromInteger . nthPrime . toEnum
  fromEnum = fromEnum . primeCount . toInteger . unPrimeNat

  succ = PrimeNat . \case
    PrimeNat 2 -> 3
    PrimeNat 3 -> 5
    PrimeNat 5 -> 7
    PrimeNat p -> head $ filter (isPrime . toInteger) $ map fromWheel30 [toWheel30 p + 1 ..]

  pred = PrimeNat . \case
    PrimeNat 2 -> error "Enum.pred{Prime}: tried to take `pred' of 2"
    PrimeNat 3 -> 2
    PrimeNat 5 -> 3
    PrimeNat 7 -> 5
    PrimeNat p -> head $ filter (isPrime . toInteger) $ map fromWheel30 [toWheel30 p - 1, toWheel30 p - 2 ..]

  -- 'dropWhile' is important, because 'psieveFrom' can actually contain primes less than p.
  enumFrom (PrimeNat p)
    = coerce
    $ dropWhile (< p)
    $ concatMap primeList
    $ psieveFrom
    $ naturalToInteger p

  enumFromTo p@(PrimeNat p') q@(PrimeNat q') = takeWhile (<= q) $ dropWhile (< p) $
    case chooseAlgorithm p' q' of
      IsPrime -> map PrimeNat $ 2 : 3 : 5 : filter (isPrime . toInteger) (map fromWheel30 [toWheel30 p' .. toWheel30 q'])
      Sieve   -> (coerce :: [Natural] -> [PrimeNat]) $
        if q' < fromIntegral sieveRange
        then           primeList $ primeSieve $ naturalToInteger q'
        else concatMap primeList $ psieveFrom $ naturalToInteger p'

  enumFromThen p@(PrimeNat p') (PrimeNat q') = case p' `compare` q' of
    LT -> filter (\(PrimeNat r') -> (r' - p') `mod` delta == 0) $ enumFrom p
      where
        delta = q' - p'
    EQ -> repeat p
    GT -> filter (\(PrimeNat r') -> (p' - r') `mod` delta == 0) $ reverse $ enumFromTo (PrimeNat 2) p
      where
        delta = p' - q'

  enumFromThenTo p@(PrimeNat p') (PrimeNat q') r = case p' `compare` q' of
    LT -> filter (\(PrimeNat r') -> (r' - p') `mod` delta == 0) $ enumFromTo p r
      where
        delta = q' - p'
    EQ -> repeat p
    GT -> filter (\(PrimeNat r') -> (p' - r') `mod` delta == 0) $ reverse $ enumFromTo (PrimeNat 2) p
      where
        delta = p' - q'

bigToSmall :: PrimeNat -> Prm
bigToSmall = Prm . (fromIntegral :: Natural -> Word) . unPrimeNat

smallToBig :: Prm -> PrimeNat
smallToBig = PrimeNat . (fromIntegral :: Word -> Natural) . unPrm

instance Enum Prm where
  toEnum   = bigToSmall . toEnum
  fromEnum = fromEnum . smallToBig
  succ     = bigToSmall . succ . smallToBig
  pred     = bigToSmall . succ . smallToBig
  enumFrom             = map bigToSmall . enumFrom . smallToBig
  enumFromTo p q       = map bigToSmall $ enumFromTo (smallToBig p) (smallToBig q)
  enumFromThen p q     = map bigToSmall $ enumFromThen (smallToBig p) (smallToBig q)
  enumFromThenTo p q r = map bigToSmall $ enumFromThenTo (smallToBig p) (smallToBig q) (smallToBig r)

-- | This typeclass is mostly used with list generators.
--   Use @[nextPrime n .. precPrime m]@ to get all primes between
--   @n@ and @m@. E. g.,
--
-- > > [nextPrime (100 :: Int) .. precPrime (150 :: Int)]
-- > [Prm 101,Prm 103,Prm 107,Prm 109,Prm 113,Prm 127,Prm 131,Prm 137,Prm 139,Prm 149]
--
--   Other list generators are also available: @[nextPrime n ..]@ generates an infinite list of primes.
class PrimeSequence a where
  -- | Smallest prime, greater or equal to argument.
  --
  -- > nextPrime (-100) ==    2
  -- > nextPrime  1000  == 1009
  -- > nextPrime  1009  == 1009
  nextPrime :: a -> Prime a
  -- | Largest prime, less or equal to argument. Undefined, when argument < 2.
  --
  -- > precPrime 100 == 97
  -- > precPrime  97 == 97
  precPrime :: a -> Prime a

instance PrimeSequence Integer where
  nextPrime n
    | n <= 2    = PrimeNat 2
    | n <= 3    = PrimeNat 3
    | n <= 5    = PrimeNat 5
    | otherwise = PrimeNat $ fromInteger $ head $ filter isPrime $
                    dropWhile (< n) $ map fromWheel30 [toWheel30 n ..]
                    -- dropWhile is important, because fromWheel30 (toWheel30 n) may appear to be < n.
                    -- E. g., fromWheel30 (toWheel30 94) == 97

  precPrime n
    | n < 2     = error $ "precPrime: tried to take `precPrime` of " ++ show n
    | n < 3     = PrimeNat 2
    | n < 5     = PrimeNat 3
    | n < 7     = PrimeNat 5
    | otherwise = PrimeNat $ fromInteger $ head $ filter isPrime $
                    dropWhile (> n) $ map fromWheel30 [toWheel30 n, toWheel30 n - 1 ..]
                    -- dropWhile is important, because fromWheel30 (toWheel30 n) may appear to be > n.
                    -- E. g., fromWheel30 (toWheel30 100) == 101

instance PrimeSequence Natural where
  nextPrime = nextPrime . toInteger
  precPrime = precPrime . toInteger

instance PrimeSequence Int where
  nextPrime n
    | n <= 2    = Prm 2
    | n <= 3    = Prm 3
    | n <= 5    = Prm 5
    | otherwise = Prm $ intToWord $ head $ filter (isPrime . toInteger) $
                    dropWhile (< n) $ map fromWheel30 [toWheel30 n ..]
                    -- dropWhile is important, because fromWheel30 (toWheel30 n) may appear to be < n.
                    -- E. g., fromWheel30 (toWheel30 94) == 97

  precPrime n
    | n < 2     = error $ "precPrime: tried to take `precPrime` of " ++ show n
    | n < 3     = Prm 2
    | n < 5     = Prm 3
    | n < 7     = Prm 5
    | otherwise = Prm $ intToWord $ head $ filter (isPrime . toInteger) $
                    dropWhile (> n) $ map fromWheel30 [toWheel30 n, toWheel30 n - 1 ..]
                    -- dropWhile is important, because fromWheel30 (toWheel30 n) may appear to be > n.
                    -- E. g., fromWheel30 (toWheel30 100) == 101

instance PrimeSequence Word where
  nextPrime = nextPrime . wordToInt
  precPrime = precPrime . wordToInt

-------------------------------------------------------------------------------
-- Helpers for mapping to rough numbers and back.
-- Copypasted from Data.BitStream.WheelMapping

toWheel30 :: (Integral a, Bits a) => a -> a
toWheel30 i = q `shiftL` 3 + (r + r `shiftR` 4) `shiftR` 2
  where
    (q, r) = i `quotRem` 30

fromWheel30 :: (Num a, Bits a) => a -> a
fromWheel30 i = ((i `shiftL` 2 - i `shiftR` 2) .|. 1)
              + ((i `shiftL` 1 - i `shiftR` 1) .&. 2)
