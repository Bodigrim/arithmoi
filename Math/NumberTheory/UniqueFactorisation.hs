-- |
-- Module:      Math.NumberTheory.UniqueFactorisation
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- An abstract type class for unique factorisation domains.
--

{-# LANGUAGE CPP               #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DefaultSignatures #-}

module Math.NumberTheory.UniqueFactorisation
  ( Prime
  , unPrime
  , UniqueFactorisation(..)
  , nextPrime
  , precPrime
  ) where

import Control.Arrow
import Data.Bits
import Data.Coerce
import Data.Maybe

import qualified Math.NumberTheory.Primes.Factorisation as F (factorise)
import qualified Math.NumberTheory.Primes.Testing.Probabilistic as T (isPrime)
import Math.NumberTheory.Primes.Types
import Math.NumberTheory.Utils (toWheel30, fromWheel30)
import Math.NumberTheory.Utils.FromIntegral

import Numeric.Natural

-- | A class for unique factorisation domains.
class UniqueFactorisation a where
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
  factorise = map (Prime . integerToInt *** id) . F.factorise . intToInteger
  isPrime n = if T.isPrime (toInteger n) then Just (Prime $ abs n) else Nothing

instance UniqueFactorisation Word where
  factorise = map (coerce integerToWord *** id) . F.factorise . wordToInteger
  isPrime n = if T.isPrime (toInteger n) then Just (Prime n) else Nothing

instance UniqueFactorisation Integer where
  factorise = coerce . F.factorise
  isPrime n = if T.isPrime n then Just (Prime $ abs n) else Nothing

instance UniqueFactorisation Natural where
  factorise = map (coerce integerToNatural *** id) . F.factorise . naturalToInteger
  isPrime n = if T.isPrime (toInteger n) then Just (Prime n) else Nothing

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
  | n < 2     = error $ "precPrime: tried to take `precPrime` of an argument less than 2"
  | n < 3     = Prime 2
  | n < 5     = Prime 3
  | n < 7     = Prime 5
  | otherwise = head $ mapMaybe isPrime $
                  dropWhile (> n) $ map fromWheel30 [toWheel30 n, toWheel30 n - 1 ..]
                  -- dropWhile is important, because fromWheel30 (toWheel30 n) may appear to be > n.
                  -- E. g., fromWheel30 (toWheel30 100) == 101
