-- |
-- Module:      Math.NumberTheory.Primes
-- Copyright:   (c) 2016-2018 Andrew.Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Math.NumberTheory.Primes
    ( Prime
    , unPrime
    , toPrimeIntegral
    , nextPrime
    , precPrime
    , UniqueFactorisation(..)
    , factorBack
    , -- * Old interface
      primes
    , -- * Temporary
      atkinPrimeList
    , atkinSieve
    ) where

import Data.Bits
import Data.Coerce
import Data.Maybe
import Data.Word
import Numeric.Natural

import Math.NumberTheory.Primes.Counting (nthPrime, primeCount)
import qualified Math.NumberTheory.Primes.Factorisation.Montgomery as F (factorise)
import qualified Math.NumberTheory.Primes.Testing.Probabilistic as T (isPrime)
import Math.NumberTheory.Primes.Sieve.Atkin
import Math.NumberTheory.Primes.Sieve.Eratosthenes (primes, sieveRange, primeList, psieveFrom, primeSieve)
import Math.NumberTheory.Primes.Small
import Math.NumberTheory.Primes.Types
import Math.NumberTheory.Utils (toWheel30, fromWheel30)
import Math.NumberTheory.Utils.FromIntegral

-- | A class for unique factorisation domains.
class Num a => UniqueFactorisation a where
  -- | Factorise a number into a product of prime powers.
  -- Factorisation of 0 is an undefined behaviour. Otherwise
  -- following invariants hold:
  --
  -- > abs n == abs (product (map (\(p, k) -> unPrime p ^ k) (factorise n)))
  -- > all ((> 0) . snd) (factorise n)
  --
  -- >>> factorise (1 :: Integer)
  -- []
  -- >>> factorise (-1 :: Integer)
  -- []
  -- >>> factorise (6 :: Integer)
  -- [(Prime 2,1),(Prime 3,1)]
  -- >>> factorise (-108 :: Integer)
  -- [(Prime 2,2),(Prime 3,3)]
  --
  -- This function is a replacement
  -- for 'Math.NumberTheory.Primes.Factorisation.factorise'.
  -- If you were looking for the latter, please import
  -- "Math.NumberTheory.Primes.Factorisation" instead of this module.
  --
  -- __Warning:__ there are no guarantees of any particular
  -- order of prime factors, do not expect them to be ascending. E. g.,
  --
  -- >>> factorise 10251562501
  -- [(Prime 101701,1),(Prime 100801,1)]
  factorise :: a -> [(Prime a, Word)]
  -- | Check whether an argument is prime.
  -- If it is then return an associated prime.
  --
  -- >>> isPrime (3 :: Integer)
  -- Just (Prime 3)
  -- >>> isPrime (4 :: Integer)
  -- Nothing
  -- >>> isPrime (-5 :: Integer)
  -- Just (Prime 5)
  --
  -- This function is a replacement
  -- for 'Math.NumberTheory.Primes.Testing.isPrime'.
  -- If you were looking for the latter, please import
  -- "Math.NumberTheory.Primes.Testing" instead of this module.
  isPrime   :: a -> Maybe (Prime a)

instance UniqueFactorisation Int where
  factorise = coerce . F.factorise
  isPrime n = if T.isPrime (toInteger n) then Just (Prime $ abs n) else Nothing

instance UniqueFactorisation Word where
  factorise = coerce . F.factorise
  isPrime n = if T.isPrime (toInteger n) then Just (Prime n) else Nothing

instance UniqueFactorisation Integer where
  factorise = coerce . F.factorise
  isPrime n = if T.isPrime n then Just (Prime $ abs n) else Nothing

instance UniqueFactorisation Natural where
  factorise = coerce . F.factorise
  isPrime n = if T.isPrime (toInteger n) then Just (Prime n) else Nothing

-- | Restore a number from its factorisation.
factorBack :: Num a => [(Prime a, Word)] -> a
factorBack = product . map (\(p, k) -> unPrime p ^ k)

-- | Smallest prime, greater or equal to argument.
--
-- > nextPrime (-100) ==    2
-- > nextPrime  1000  == 1009
-- > nextPrime  1009  == 1009
nextPrime :: (Bits a, Integral a, UniqueFactorisation a) => a -> Prime a
nextPrime n
  | n <= 2    = Prime 2
  | n <= 3    = Prime 3
  | n <= 5    = Prime 5
  | otherwise = head $ mapMaybe isPrime $
                  dropWhile (< n) $ map fromWheel30 [toWheel30 n ..]
                  -- dropWhile is important, because fromWheel30 (toWheel30 n) may appear to be < n.
                  -- E. g., fromWheel30 (toWheel30 94) == 97

-- | Largest prime, less or equal to argument. Undefined, when argument < 2.
--
-- > precPrime 100 == 97
-- > precPrime  97 == 97
precPrime :: (Bits a, Integral a, UniqueFactorisation a) => a -> Prime a
precPrime n
  | n < 2     = error "precPrime: tried to take `precPrime` of an argument less than 2"
  | n < 3     = Prime 2
  | n < 5     = Prime 3
  | n < 7     = Prime 5
  | otherwise = head $ mapMaybe isPrime $
                  dropWhile (> n) $ map fromWheel30 [toWheel30 n, toWheel30 n - 1 ..]
                  -- dropWhile is important, because fromWheel30 (toWheel30 n) may appear to be > n.
                  -- E. g., fromWheel30 (toWheel30 100) == 101

-------------------------------------------------------------------------------
-- Prime sequences

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
  Prime p -> head $ mapMaybe (isPrime . fromWheel30) [toWheel30 p + 1 ..]

succGenericBounded
  :: (Bits a, Integral a, UniqueFactorisation a, Bounded a)
  => Prime a
  -> Prime a
succGenericBounded = \case
  Prime 2 -> Prime 3
  Prime 3 -> Prime 5
  Prime 5 -> Prime 7
  Prime p -> case mapMaybe (isPrime . fromWheel30) [toWheel30 p + 1 .. toWheel30 maxBound] of
    []    -> error "Enum.succ{Prime}: tried to take `succ' near `maxBound'"
    q : _ -> q

predGeneric :: (Bits a, Integral a, UniqueFactorisation a) => Prime a -> Prime a
predGeneric = \case
  Prime 2 -> error "Enum.pred{Prime}: tried to take `pred' of 2"
  Prime 3 -> Prime 2
  Prime 5 -> Prime 3
  Prime 7 -> Prime 5
  Prime p -> head $ mapMaybe (isPrime . fromWheel30) [toWheel30 p - 1, toWheel30 p - 2 ..]

-- 'dropWhile' is important, because 'psieveFrom' can actually contain primes less than p.
enumFromGeneric :: Integral a => Prime a -> [Prime a]
enumFromGeneric p@(Prime p')
  = coerce
  $ dropWhile (< p)
  $ concat
  $ takeWhile (not . null)
  $ map primeList
  $ psieveFrom
  $ toInteger p'

smallPrimesLimit :: Integral a => a
smallPrimesLimit = fromIntegral (maxBound :: Word16)

enumFromToGeneric :: (Bits a, Integral a, UniqueFactorisation a) => Prime a -> Prime a -> [Prime a]
enumFromToGeneric p@(Prime p') q@(Prime q')
  | p' <= smallPrimesLimit, q' <= smallPrimesLimit
  = map (Prime . fromIntegral) $ smallPrimesFromTo (fromIntegral p') (fromIntegral q')
  | p' <= smallPrimesLimit
  = map (Prime . fromIntegral) (smallPrimesFromTo (fromIntegral p') smallPrimesLimit)
  ++ enumFromToGeneric' (nextPrime smallPrimesLimit) q
  | otherwise
  = enumFromToGeneric' p q

enumFromToGeneric'
  :: (Bits a, Integral a, UniqueFactorisation a)
  => Prime a
  -> Prime a
  -> [Prime a]
enumFromToGeneric' p@(Prime p') q@(Prime q') = takeWhile (<= q) $ dropWhile (< p) $
  case chooseAlgorithm p' q' of
    IsPrime -> Prime 2 : Prime 3 : Prime 5 : mapMaybe (isPrime . fromWheel30) [toWheel30 p' .. toWheel30 q']
    Sieve   ->
      if q' < fromIntegral sieveRange
      then           primeList $ primeSieve $ toInteger q'
      else concatMap primeList $ psieveFrom $ toInteger p'

enumFromThenGeneric :: (Bits a, Integral a, UniqueFactorisation a) => Prime a -> Prime a -> [Prime a]
enumFromThenGeneric p@(Prime p') (Prime q') = case p' `compare` q' of
  LT -> filter (\(Prime r') -> (r' - p') `rem` delta == 0) $ enumFromGeneric p
    where
      delta = q' - p'
  EQ -> repeat p
  GT -> filter (\(Prime r') -> (p' - r') `rem` delta == 0) $ reverse $ enumFromToGeneric (Prime 2) p
    where
      delta = p' - q'

enumFromThenToGeneric :: (Bits a, Integral a, UniqueFactorisation a) => Prime a -> Prime a -> Prime a -> [Prime a]
enumFromThenToGeneric p@(Prime p') (Prime q') r@(Prime r') = case p' `compare` q' of
  LT -> filter (\(Prime t') -> (t' - p') `rem` delta == 0) $ enumFromToGeneric p r
    where
      delta = q' - p'
  EQ -> if p' <= r' then repeat p else []
  GT -> filter (\(Prime t') -> (p' - t') `rem` delta == 0) $ reverse $ enumFromToGeneric r p
    where
      delta = p' - q'

instance Enum (Prime Integer) where
  toEnum = nthPrime
  fromEnum = integerToInt . primeCount . unPrime
  succ = succGeneric
  pred = predGeneric
  enumFrom = enumFromGeneric
  enumFromTo = enumFromToGeneric
  enumFromThen = enumFromThenGeneric
  enumFromThenTo = enumFromThenToGeneric

instance Enum (Prime Natural) where
  toEnum = Prime . integerToNatural . unPrime . nthPrime
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
      Prime p = nthPrime n
  fromEnum = integerToInt . primeCount . intToInteger . unPrime
  succ = succGenericBounded
  pred = predGeneric
  enumFrom = enumFromGeneric
  enumFromTo = enumFromToGeneric
  enumFromThen = enumFromThenGeneric
  enumFromThenTo = enumFromThenToGeneric

instance Bounded (Prime Int) where
  minBound = Prime 2
  maxBound = precPrime maxBound

instance Enum (Prime Word) where
  toEnum n = if p > wordToInteger maxBound
    then error $ "Enum.toEnum{Prime}: " ++ show n ++ "th prime = " ++ show p ++ " is out of bounds of Word"
    else Prime (integerToWord p)
    where
      Prime p = nthPrime n
  fromEnum = integerToInt . primeCount . wordToInteger . unPrime
  succ = succGenericBounded
  pred = predGeneric
  enumFrom = enumFromGeneric
  enumFromTo = enumFromToGeneric
  enumFromThen = enumFromThenGeneric
  enumFromThenTo = enumFromThenToGeneric

instance Bounded (Prime Word) where
  minBound = Prime 2
  maxBound = precPrime maxBound
