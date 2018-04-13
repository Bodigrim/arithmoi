-- |
-- Module:      Math.NumberTheory.Prefactored
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Type for numbers, accompanied by their factorisation.
--

{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Math.NumberTheory.Prefactored
  ( Prefactored(prefValue, prefFactors)
  , fromValue
  , fromFactors
  ) where

import Control.Arrow

import Data.Semigroup

import Math.NumberTheory.GCD (Coprimes, splitIntoCoprimes, toList, singleton)
import Math.NumberTheory.UniqueFactorisation

-- | A container for a number and its pairwise coprime (but not neccessarily prime)
-- factorisation.
-- It is designed to preserve information about factors under multiplication.
-- One can use this representation to speed up prime factorisation
-- and computation of arithmetic functions.
--
-- For instance, let @p@ and @q@ be big primes:
--
-- > > let p, q :: Integer
-- > >     p = 1000000000000000000000000000057
-- > >     q = 2000000000000000000000000000071
--
-- It would be  difficult to compute prime factorisation of their product
-- as is:
-- 'factorise' would take ages. Things become different if we simply
-- change types of @p@ and @q@ to prefactored ones:
--
-- > > let p, q :: Prefactored Integer
-- > >     p = 1000000000000000000000000000057
-- > >     q = 2000000000000000000000000000071
--
-- Now prime factorisation is done instantly:
--
-- > > factorise (p * q)
-- > [(PrimeNat 1000000000000000000000000000057, 1), (PrimeNat 2000000000000000000000000000071, 1)]
-- > > factorise (p^2 * q^3)
-- > [(PrimeNat 1000000000000000000000000000057, 2), (PrimeNat 2000000000000000000000000000071, 3)]
--
-- Moreover, we can instantly compute 'totient' and its iterations.
-- It works fine, because output of 'totient' is also prefactored.
--
-- > > prefValue $ totient (p^2 * q^3)
-- > 8000000000000000000000000001752000000000000000000000000151322000000000000000000000006445392000000000000000000000135513014000000000000000000001126361040
-- > > prefValue $ totient $ totient (p^2 * q^3)
-- > 2133305798262843681544648472180210822742702690942899511234131900112583590230336435053688694839034890779375223070157301188739881477320529552945446912000
--
-- Let us look under the hood:
--
-- > > prefFactors $ totient (p^2 * q^3)
-- > [(2, 4), (41666666666666666666666666669, 1), (3, 3), (111111111111111111111111111115, 1),
-- > (1000000000000000000000000000057, 1), (2000000000000000000000000000071, 2)]
-- > > prefFactors $ totient $ totient (p^2 * q^3)
-- > [(2, 22), (39521, 1), (5, 3), (199937, 1), (3, 8), (6046667, 1), (227098769, 1),
-- > (85331809838489, 1), (361696272343, 1), (22222222222222222222222222223, 1),
-- > (41666666666666666666666666669, 1), (2000000000000000000000000000071, 1)]
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
  } deriving (Show)

-- | Create 'Prefactored' from a given number.
--
-- > > fromValue 123
-- > Prefactored {prefValue = 123, prefFactors = [(123, 1)]}
fromValue :: a -> Prefactored a
fromValue a = Prefactored a (singleton a 1)

-- | Create 'Prefactored' from a given list of pairwise coprime
-- (but not neccesarily prime) factors with multiplicities.
-- If you cannot ensure coprimality, use 'splitIntoCoprimes'.
--
-- > > fromFactors (splitIntoCoprimes [(140, 1), (165, 1)])
-- > Prefactored {prefValue = 23100, prefFactors = [(5, 2), (28, 1), (33, 1)]}
-- > > fromFactors (splitIntoCoprimes [(140, 2), (165, 3)])
-- > Prefactored {prefValue = 88045650000, prefFactors = [(5, 5), (28, 2), (33, 3)]}
fromFactors :: Integral a => [(a, Word)] -> Prefactored a
fromFactors as = Prefactored (product (map (uncurry (^)) as)) (splitIntoCoprimes as)

instance (Integral a, UniqueFactorisation a) => Num (Prefactored a) where
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

type instance Prime (Prefactored a) = Prime a

instance UniqueFactorisation a => UniqueFactorisation (Prefactored a) where
  unPrime p = fromValue (unPrime p)
  factorise (Prefactored _ f)
    = concatMap (\(x, xm) -> map (second (* xm)) (factorise x)) (toList f)
  isPrime (Prefactored _ f) = case toList f of
    [(n, 1)] -> isPrime n
    _        -> Nothing
