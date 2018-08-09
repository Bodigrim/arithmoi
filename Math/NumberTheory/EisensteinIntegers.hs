-- |
-- Module:      Math.NumberTheory.EisensteinIntegers
-- Licence:     MIT
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- This module exports functions for manipulating Eisenstein integers, including
-- computing their prime factorisations.
--

{-# LANGUAGE DeriveGeneric #-}

module Math.NumberTheory.EisensteinIntegers
  ( EisensteinInteger(..)
  ,  ω
  , divE
  ) where

import GHC.Generics (Generic)

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
    -- An Eisenstein integer @a :+ b@, with @a, b@ integers, can we written as
    -- @(2*a - b) / 2 + ((b * sqrt 3) * ι) / 2@, but this number is in the
    -- same quadrant as @(2*a - b) / 2 + (b * ι) / 2@, and this one in the
    -- same as @(2*a - b) + b * ι@. Divisions or floating points are not
    -- necessary.
    abs z@(x :+ y) = abs' (2*x - y) x
      where
        abs' a b
            | a == 0 && b == 0 =   z             -- origin
            | a >  0 && b >= 0 =   z             -- first quadrant: (0, inf) x [0, inf)ω
            | a <= 0 && b >  0 =   b  :+ (-a)    -- second quadrant: (-inf, 0] x (0, inf)ω
            | a <  0 && b <= 0 = (-a) :+ (-b)    -- third quadrant: (-inf, 0) x (-inf, 0]ω
            | otherwise        = (-b) :+   a     -- fourth quadrant: [0, inf) x (-inf, 0)ω
    negate (a :+ b) = (-a) :+ (-b)
    fromInteger n = n :+ 0
    signum z@(a :+ b)
        | a == 0 && b == 0 = z               -- hole at origin
        | otherwise        = z `divE` abs z

-- | Eisenstein integer division, truncating toward negative infinity.
divE :: EisensteinInteger -> EisensteinInteger -> EisensteinInteger
divE = undefined