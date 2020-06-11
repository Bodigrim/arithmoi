-- |
-- Module:      Math.NumberTheory.Prefactored
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Type for numbers, accompanied by their factorisation.
--

{-# LANGUAGE TypeFamilies  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Math.NumberTheory.Prefactored
  ( Prefactored(prefValue, prefFactors)
  , fromValue
  , fromFactors
  ) where

import Prelude hiding ((^), gcd)
import Control.Arrow
import Data.Euclidean
import Data.Semigroup
import Data.Semiring (Semiring(..), Mul(..), (^))
import qualified Data.Semiring as Semiring
import Unsafe.Coerce

import Math.NumberTheory.Euclidean.Coprimes
import Math.NumberTheory.Primes
import Math.NumberTheory.Primes.Types

-- | A container for a number and its pairwise coprime (but not neccessarily prime)
-- factorisation.
-- It is designed to preserve information about factors under multiplication.
-- One can use this representation to speed up prime factorisation
-- and computation of arithmetic functions.
--
-- For instance, let @p@ and @q@ be big primes:
--
-- >>> let p = 1000000000000000000000000000057 :: Integer
-- >>> let q = 2000000000000000000000000000071 :: Integer
--
-- It would be difficult to compute the totient function
-- of their product as is, because once we multiplied them
-- the information of factors is lost and
-- 'Math.NumberTheory.ArithmeticFunctions.totient' (@p@ * @q@)
-- would take ages. Things become different if we simply
-- change types of @p@ and @q@ to prefactored ones:
--
-- >>> let p = 1000000000000000000000000000057 :: Prefactored Integer
-- >>> let q = 2000000000000000000000000000071 :: Prefactored Integer
--
-- Now the 'Math.NumberTheory.ArithmeticFunctions.totient' function
-- can be computed instantly:
--
-- >>> import Math.NumberTheory.ArithmeticFunctions
-- >>> prefValue $ totient (p^2 * q^3)
-- 8000000000000000000000000001752000000000000000000000000151322000000000000000000000006445392000000000000000000000135513014000000000000000000001126361040
-- >>> prefValue $ totient $ totient (p^2 * q^3)
-- 2133305798262843681544648472180210822742702690942899511234131900112583590230336435053688694839034890779375223070157301188739881477320529552945446912000
--
-- Let us look under the hood:
--
-- >>> import Math.NumberTheory.ArithmeticFunctions
-- >>> prefFactors $ totient (p^2 * q^3)
-- Coprimes {unCoprimes = [(1000000000000000000000000000057,1),(41666666666666666666666666669,1),(2000000000000000000000000000071,2),(111111111111111111111111111115,1),(2,4),(3,3)]}
-- >>> prefFactors $ totient $ totient (p^2 * q^3)
-- Coprimes {unCoprimes = [(39521,1),(227098769,1),(22222222222222222222222222223,1),(2000000000000000000000000000071,1),(361696272343,1),(85331809838489,1),(6046667,1),(199937,1),(5,3),(41666666666666666666666666669,1),(2,22),(3,8)]}
--
-- Pairwise coprimality of factors is crucial, because it allows
-- us to process them independently, possibly even
-- in parallel or concurrent fashion.
--
-- Following invariant is guaranteed to hold:
--
-- > abs (prefValue x) = abs (product (map (uncurry (^)) (prefFactors x)))
data Prefactored a = Prefactored
  { prefValue   :: a
    -- ^ Number itself.
  , prefFactors :: Coprimes a Word
    -- ^ List of pairwise coprime (but not neccesarily prime) factors,
    -- accompanied by their multiplicities.
  } deriving (Eq, Show)

-- | Create 'Prefactored' from a given number.
--
-- >>> fromValue 123
-- Prefactored {prefValue = 123, prefFactors = Coprimes {unCoprimes = [(123,1)]}}
fromValue :: (Eq a, GcdDomain a) => a -> Prefactored a
fromValue a = Prefactored a (singleton a 1)

-- | Create 'Prefactored' from a given list of pairwise coprime
-- (but not neccesarily prime) factors with multiplicities.
--
-- >>> fromFactors (splitIntoCoprimes [(140, 1), (165, 1)])
-- Prefactored {prefValue = 23100, prefFactors = Coprimes {unCoprimes = [(28,1),(33,1),(5,2)]}}
-- >>> fromFactors (splitIntoCoprimes [(140, 2), (165, 3)])
-- Prefactored {prefValue = 88045650000, prefFactors = Coprimes {unCoprimes = [(28,2),(33,3),(5,5)]}}
fromFactors :: Semiring a => Coprimes a Word -> Prefactored a
fromFactors as = Prefactored (getMul $ foldMap (\(a, k) -> Mul $ a ^ k) (unCoprimes as)) as

instance (Eq a, GcdDomain a) => Semiring (Prefactored a) where
  Prefactored v1 _ `plus` Prefactored v2 _
    = fromValue (v1 `plus` v2)
  Prefactored v1 f1 `times` Prefactored v2 f2
    = Prefactored (v1 `times` v2) (f1 <> f2)
  fromNatural n = fromValue (fromNatural n)

instance (Eq a, Num a, GcdDomain a) => Num (Prefactored a) where
  Prefactored v1 _ + Prefactored v2 _
    = fromValue (v1 + v2)
  Prefactored v1 _ - Prefactored v2 _
    = fromValue (v1 - v2)
  Prefactored v1 f1 * Prefactored v2 f2
    = Prefactored (v1 * v2) (f1 <> f2)
  negate (Prefactored v f) = Prefactored (negate v) f
  abs (Prefactored v f)    = Prefactored (abs v) f
  signum (Prefactored v _) = Prefactored (signum v) mempty
  fromInteger n = fromValue (fromInteger n)

instance (Eq a, GcdDomain a, UniqueFactorisation a) => UniqueFactorisation (Prefactored a) where
  factorise (Prefactored _ f)
    = concatMap (\(x, xm) -> map (\(p, k) -> (Prime $ fromValue $ unPrime p, k * xm)) (factorise x)) (unCoprimes f)
  isPrime (Prefactored _ f) = case unCoprimes f of
    [(n, 1)] -> Prime . fromValue . unPrime <$> isPrime n
    _        -> Nothing
