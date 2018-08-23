-- |
-- Module:      Math.NumberTheory.EisensteinIntegers
-- Copyright:   (c) 2018 Alexandre Rodrigues Baldé
-- Licence:     MIT
-- Maintainer:  Alexandre Rodrigues Baldé <alexandrer_b@outlook.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- This module exports functions for manipulating Eisenstein integers, including
-- computing their prime factorisations.
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes    #-}

module Math.NumberTheory.EisensteinIntegers
  ( EisensteinInteger(..)
  , ω
  , conjugate
  , norm

  -- * Division and remainder functions
  , divE
  , divModE
  , modE
  , quotRemE
  , quotE
  , remE

  , gcdE

  -- * Primality functions
  , findPrime
  , isPrime
  ) where

import GHC.Generics                               (Generic)

import qualified Math.NumberTheory.Moduli         as Moduli
import Math.NumberTheory.Moduli.Sqrt              (FieldCharacteristic(..))
import Math.NumberTheory.Primes.Types             (PrimeNat(..))
import qualified Math.NumberTheory.Primes.Testing as Testing
import Math.NumberTheory.Utils.FromIntegral       (integerToNatural)

infix 6 :+

-- |An Eisenstein integer is a + bω, where a and b are both integers.
data EisensteinInteger = (:+) { real :: !Integer, imag :: !Integer }
    deriving (Eq, Ord, Generic)

-- | The imaginary unit for Eisenstein integers, where
--
-- > ω == (-1/2) + ((sqrt 3)/2)ι == exp(2*pi*ι/3)
-- and ι is the usual imaginary unit with ι² == -1.
ω :: EisensteinInteger
ω = 0 :+ 1

instance Show EisensteinInteger where
    show (a :+ b)
        | b == 0     = show a
        | a == 0     = s ++ b'
        | otherwise  = show a ++ op ++ b'
        where
            b' = if abs b == 1 then "ω" else show (abs b) ++ "*ω"
            op = if b > 0 then "+" else "-"
            s  = if b > 0 then "" else "-"

instance Num EisensteinInteger where
    (+) (a :+ b) (c :+ d) = (a + c) :+ (b + d)
    (*) (a :+ b) (c :+ d) = (a * c - b * d) :+ (b * c + a * d - b * d)
    abs z@(a :+ b)
        | a == 0 && b == 0                                  = z            -- origin
        | a > b && b >= 0                                   = z            -- first sextant: 0 ≤ Arg(η) < π/3
        | b >= a && a > 0                                   = (-ω) * z     -- second sextant: π/3 ≤ Arg(η) < 2π/3
        | b > 0 && 0 >= a                                   = (-1 - ω) * z -- third sextant: 2π/3 ≤ Arg(η) < π
        | a < b && b <= 0                                   = - z          -- fourth sextant: -π < Arg(η) < -2π/3 or Arg(η) = π
        | b <= a && a < 0                                   = ω * z        -- fifth sextant: -2π/3 ≤ Arg(η) < -π/3
        | otherwise                                         = (1 + ω) * z  -- sixth sextant: -π/3 ≤ Arg(η) < 0
    negate (a :+ b) = (-a) :+ (-b)
    fromInteger n = n :+ 0
    signum z@(a :+ b)
        | a == 0 && b == 0 = z               -- hole at origin
        | otherwise        = z `divE` abs z

-- |Simultaneous 'quot' and 'rem'.
quotRemE
    :: EisensteinInteger
    -> EisensteinInteger
    -> (EisensteinInteger, EisensteinInteger)
quotRemE = divHelper quot

-- |Eisenstein integer division, truncating toward zero.
quotE :: EisensteinInteger -> EisensteinInteger -> EisensteinInteger
n `quotE` d = q where (q,_) = quotRemE n d

-- |Eisenstein integer remainder, satisfying
--
-- > (x `quotE` y)*y + (x `remE` y) == x
remE :: EisensteinInteger -> EisensteinInteger -> EisensteinInteger
n `remE`  d = r where (_,r) = quotRemE n d

-- | Simultaneous 'div' and 'mod' of Eisenstein integers.
divModE
    :: EisensteinInteger
    -> EisensteinInteger
    -> (EisensteinInteger, EisensteinInteger)
divModE = divHelper div

-- | Eisenstein integer division, truncating toward negative infinity.
divE :: EisensteinInteger -> EisensteinInteger -> EisensteinInteger
n `divE` d = q where (q,_) = divModE n d

-- | Eisenstein integer remainder, satisfying
--
-- > (x `divE` y)*y + (x `modE` y) == x
modE :: EisensteinInteger -> EisensteinInteger -> EisensteinInteger
n `modE` d = r where (_,r) = divModE n d

-- | Function that does most of the underlying work for @divMod@ and
-- @quotRem@, apart from choosing the specific integer division algorithm.
-- This is instead done by the calling function (either @divMod@ which uses
-- @div@, or @quotRem@, which uses @quot@.)
divHelper
    :: (Integer -> Integer -> Integer)
    -> EisensteinInteger
    -> EisensteinInteger
    -> (EisensteinInteger, EisensteinInteger)
divHelper divide g h =
    let nr :+ ni = g * conjugate h
        denom = norm h
        q = divide nr denom :+ divide ni denom
        p = h * q
    in (q, g - p)

-- | Conjugate a Eisenstein integer.
conjugate :: EisensteinInteger -> EisensteinInteger
conjugate (a :+ b) = (a - b) :+ (-b)

-- | The square of the magnitude of a Eisenstein integer.
norm :: EisensteinInteger -> Integer
norm (a :+ b) = a*a - a * b + b*b

-- | Checks if a given @EisensteinInteger@ is prime. @EisensteinInteger@s
-- whose norm is a prime congruent to @0@ or @1@ modulo 3 are prime.
-- See <http://thekeep.eiu.edu/theses/2467 Bandara, Sarada, "An Exposition of the Eisenstein Integers" (2016)>,
-- page 12.
isPrime :: EisensteinInteger -> Bool
isPrime e
    | e == 0                     = False
    -- Special case, @1 - ω@ is the only Eisenstein prime with norm @3@, and
    -- @abs (1 - ω) = 2 + ω@.
    | a' == 2 && b' == 1         = True
    | b' == 0 && a' `mod` 3 == 2 = Testing.isPrime a'
    | nE `mod` 3 == 1            = Testing.isPrime nE
    | otherwise = False
  where nE       = norm e
        a' :+ b' = abs e

-- | Compute the GCD of two Eisenstein integers. The result is always
-- in the first sextant.
gcdE :: EisensteinInteger -> EisensteinInteger -> EisensteinInteger
gcdE g h = gcdE' (abs g) (abs h)

gcdE' :: EisensteinInteger -> EisensteinInteger -> EisensteinInteger
gcdE' g h
    | h == 0    = g -- done recursing
    | otherwise = gcdE' h (abs (g `modE` h))

-- | Find an Eisenstein integer whose norm is the given prime number
-- in the form @3k + 1@ using a modification of the
-- <http://www.ams.org/journals/mcom/1972-26-120/S0025-5718-1972-0314745-6/S0025-5718-1972-0314745-6.pdf Hermite-Serret algorithm>.
findPrime :: Integer -> EisensteinInteger
findPrime p = case Moduli.sqrtModMaybe (9*k*k - 1) (FieldCharacteristic (PrimeNat . integerToNatural $ p) 1) of
    Nothing   -> error "findPrime: argument must be prime p = 6k + 1"
    Just zed  -> gcdE (p :+ 0) ((zed - 3 * k) :+ 1)
    where
        k :: Integer
        k = p `div` 6