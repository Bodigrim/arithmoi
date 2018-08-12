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
  , divE
  , divModE
  , divModE'
  , eisensteinToComplex
  , modE
  , norm
  ) where

import qualified Data.Complex                     as C
import Data.Ratio                                 ((%))
import Debug.Trace                                (trace)
import GHC.Generics                               (Generic)

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

eisensteinToComplex :: forall a . RealFloat a => EisensteinInteger -> C.Complex a
eisensteinToComplex (a :+ b) = (a' - b' / 2) C.:+ ((b' * sqrt 3) / 2)
  where
    a' = fromInteger a
    b' = fromInteger b

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

-- | Function to return the real part of an @EisensteinInteger@ @α + βω@ when
-- written in the form @a + bι@, where @α, β@ are @Integer@s and @a, b@ are
-- real numbers.
realEisen :: EisensteinInteger -> Rational
realEisen (a :+ b) = fromInteger a - (b % 2)

divModE'
    :: EisensteinInteger
    -> EisensteinInteger
    -> (EisensteinInteger, EisensteinInteger)
divModE' g h =
    let nr :+ ni = g * conjugate h
        denom = norm h
        q = div nr denom :+ div ni denom
        p = h * q
    in (q, g - p)


-- | Simultaneous 'div' and 'mod' of Eisenstein integers.
-- The algorithm used here was derived from
-- <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.892.1219&rep=rep1&type=pdf NTRU over the Eisenstein Integers>
-- by K. Jarvis, Theorem 4.1.1.
divModE
    :: EisensteinInteger
    -> EisensteinInteger
    -> (EisensteinInteger, EisensteinInteger)
divModE alfa beta | norm rho1 < norm rho2 = res1
                  | norm rho1 > norm rho2 = res2
                  | norm rho1 == norm rho2 && 
                    realEisen gamma1 > realEisen gamma2 = res1
                  | otherwise = res2
  where
    res1 = trace show' (gamma1, rho1)
    res2 = trace show' (gamma2, rho2)
    show' = "norm ρ1: " ++ show (norm rho1) ++ "\n" ++
            "norm ρ2: " ++ show (norm rho2) ++ "\n" ++
            "ρ1': " ++ show rho1' ++ "\n" ++
            "ρ2': " ++ show rho2' ++ "\n" ++
            "ρ1' * β': " ++ show (beta' * rho1') ++ "\n" ++
            "ρ2' * β': " ++ show (beta' * rho2') ++ "\n" ++
            "(γ1,ρ1): " ++ show (gamma1, rho1) ++ "\n" ++
            "(γ2,ρ2): " ++ show (gamma2, rho2) ++ "\n" ++
            "Re(γ1): " ++ show (realEisen gamma1) ++ "\n" ++
            "Re(γ2): " ++ show (realEisen gamma2) ++ "\n"

    -- @sqrt 3@ is used many times throughout the function, as such it's
    -- calculated here once.
    sqrt3                :: Double
    sqrt3                = sqrt 3

    -- First step of assignments performed in the Division Algorithm from
    -- Theorem 4.1.1.
    alfa'              = eisensteinToComplex alfa
    beta'              = eisensteinToComplex beta
    a1 C.:+ b1         = alfa' / beta'
    a2 C.:+ b2         = (alfa' / beta') - eisensteinToComplex ω

    -- @round@s a @Double@ and returns that as another @Double@.
    dToIToD            :: Double -> Double
    dToIToD            = (fromIntegral :: Integer -> Double) . round

    -- Second step of assignments performed in the Division Algorithm from
    -- Theorem 4.1.1.
    properFraction'    :: Double -> (Integer, Double)
    properFraction'    = properFraction
    rho'               :: Double -> Double -> C.Complex Double
    rho' a' b'         = (snd $ properFraction' a') C.:+ (b' - sqrt3 * dToIToD (b' / sqrt3))
    rho1'              = rho' a1 b1
    rho2'              = rho' a2 b2

    -- Converts a complex number in the form @a + bι@, where @a, b@ are
    -- @Double@s, into an @EisensteinInteger@ in the form @α + βω@, where
    -- @α, β@ are @Integer@s.
    toEisen            :: C.Complex Double -> EisensteinInteger
    toEisen (x C.:+ y) = round (x + y / sqrt3) :+ round (y * 2 / sqrt3)

    -- Third step of assignments performed in the Division Algorithm from
    -- Theorem 4.1.1.
    rho1               = toEisen $ beta' * rho1'
    b1sqrt3'           = round $ b1 / sqrt3
    gamma1             = ((round a1) + b1sqrt3') :+ (2 * b1sqrt3')
    rho2               = toEisen $ beta' * rho2'
    b2sqrt3'           = round $ b2 / sqrt3
    gamma2             = ((round a2) + b2sqrt3') :+ (1 + 2 * b2sqrt3')


-- | Eisenstein integer division, truncating toward negative infinity.
divE :: EisensteinInteger -> EisensteinInteger -> EisensteinInteger
n `divE` d = q where (q,_) = divModE n d

-- | Eisenstein integer remainder, satisfying
--
-- > (x `divE` y)*y + (x `modE` y) == x
modE :: EisensteinInteger -> EisensteinInteger -> EisensteinInteger
n `modE` d = r where (_,r) = divModE n d

-- | Conjugate a Eisenstein integer.
conjugate :: EisensteinInteger -> EisensteinInteger
conjugate (a :+ b) = (a - b) :+ (-b)

-- | The square of the magnitude of a Eisenstein integer.
norm :: EisensteinInteger -> Integer
norm (a :+ b) = a*a - a * b + b*b