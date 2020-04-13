{-# LANGUAGE LambdaCase #-}

module Math.NumberTheory.Moduli.CubicSymbol
    ( CubicSymbol(..)
    , conj
    , cubicSymbol
    ) where

import qualified Math.NumberTheory.Quadratic.EisensteinIntegers as E
import qualified Math.NumberTheory.Primes as P
import qualified Data.Euclidean as A
import qualified Data.Semigroup as S
import qualified Data.List as L



data CubicSymbol = Zero | Omega | OmegaSquare | One deriving (Eq)

instance S.Semigroup CubicSymbol where
    (<>) Zero _                    = Zero
    (<>) _ Zero                    = Zero
    (<>) One y                     = y
    (<>) x One                     = x
    (<>) Omega Omega               = OmegaSquare
    (<>) Omega OmegaSquare         = One
    (<>) OmegaSquare Omega         = One
    (<>) OmegaSquare OmegaSquare   = Omega


instance Show CubicSymbol where
    show = \case
        Zero         -> "0"
        Omega        -> "ω"
        OmegaSquare  -> "ω²"
        One          -> "1"
        otherwise    -> error ""


-- CHANGE INT TO INTEGERS MOD 6?
-- IS THIS MORE EFFICIENT THAN SEMIRING.STIMES?

exponentiation :: CubicSymbol -> Int -> CubicSymbol
exponentiation Zero _ = Zero
exponentiation One _ = One
exponentiation _ 0 = One
exponentiation Omega 1 = Omega
exponentiation Omega 2 = OmegaSquare
exponentiation OmegaSquare 1 = OmegaSquare
exponentiation OmegaSquare 2 = Omega


-- EXHAUST ALL CASES
conj :: CubicSymbol -> CubicSymbol
conj = \case
    Zero          -> Zero
    Omega         -> OmegaSquare
    OmegaSquare   -> Omega
    One           -> One
    otherwise     -> error ""

-- The algorithm cubicSymbol takes two Eisentein numbers @alpha@ and @beta@ and returns
-- their cubic symbol. It is divided in the following steps.

-- 1) Check whether @beta@ is coprime to 3.
-- 2) Replace @alpha@ by the remainder of @alpha@ mod @beta@
--    This does not affect the cubic symbol.
-- 3) Replace @alpha@ and @beta@ by their associated primary
--    divisors and keep track of how the cubic symbol changes
-- 4) Invoke cubic reciprocity and swap @alpha@ with @beta@.
--    Note both numbers have to be primary.
-- 5) If one of the two numbers is a unit or zero stop,
--    multiplying by the relevant cubic symbol, else go to 2).

-- AVOID USING GUARDS IN FUNCTION?
-- CHANGE NAME OF FUNCTION TO CUBICRESIDUE?
-- CAN THIS RETURN MAYBE TYPE?

-- This function takes two Eisenstein integers and returns their cubic residue character.
-- Note that the second argument must be coprime to 3 else the algorithm returns an error. 
cubicSymbol :: E.EisensteinInteger -> E.EisensteinInteger -> CubicSymbol
cubicSymbol alpha beta
    -- This checks whether beta is coprime to 3, i.e. divisible by @1 - ω@
    -- In particular, it returns an error if beta == 0
    | (betaNorm `mod` 3 == 0)    = error ""
    -- It is necessary to check now whether @alpha == 0@ or @betaNorm == 1@ since later,
    -- cubic reciprocity will be assumed to invert the arguments.
    | betaNorm == 1              = Zero
    | alpha == 0                 = Zero
    | otherwise                  = cubicSymbolHelper alpha beta
        where betaNorm = E.norm beta


cubicSymbolHelper :: E.EisensteinInteger -> E.EisensteinInteger -> CubicSymbol
-- This happens when alpha and beta have a common factor. Note that, @alpha@
-- and @beta@ are swapped because cubic reciprocity is called later on.
-- Note this cannot be called in the first step of the recursion
cubicSymbolHelper beta 0 = Zero
-- This happens when they are coprime. Note that the associated primary number
-- of any unit is 1, hence it is enough to wirte this case. Furthermore,
-- if @beta == 1@, then @alpha == 1@. 
-- Note this cannot be called in the first step of the recursion
cubicSymbolHelper beta 1 = One
-- This is the cubic reciprocity law
cubicSymbolHelper alpha beta = (cubicSymbolHelper primaryBeta primaryRemainder) <> newSymbol
    where (primaryRemainder, primaryBeta, symbolExponent) = extractPrimaryContributions remainder beta
          remainder = A.rem alpha beta
          newSymbol = exponentiation Omega (symbolExponent `mod` 3) -- temporary, ideally change to integers modulo 6


-- This function takes two Eisenstein intgers @alpha@ and @beta@ and returns three
-- arguments @(gamma, delta, contribution)@. @gamma@ and @delta@ are the associated
-- primary numbers to alpha and beta respectively. @contribution@ is a an integer
-- defined mod 6 which measures the difference between the cubic residue of @alpha@
-- and @beta with respect to the cubic residue of @gamma@ and @delta@
extractPrimaryContributions :: E.EisensteinInteger -> E.EisensteinInteger -> (E.EisensteinInteger, E.EisensteinInteger, Int)
extractPrimaryContributions alpha beta = (gamma, delta, contribution)
    where contribution = j*m - i*m -i*n
          m = fromIntegral mInt -- Change this to integers modulo 6 (or 3)
          n = fromIntegral nInt
          mInt E.:+ nInt = A.quot (delta - 1) 3
          (i, gamma) = getPrimaryDecomposition alphaThreeFree 
          (_, delta) = getPrimaryDecomposition beta
          (j, alphaThreeFree) = factoriseBadPrime alpha



-- This function takes an Eisenstein number @e@ and returns @(exponent, quotient)@
-- where exponent is the largest integer such that @(1 - ω)^exponent@ divides @e@.
-- @quotient@ is the quotient of @e@ by @(1 - ω)^exponent@
factoriseBadPrime :: E.EisensteinInteger -> (Int, E.EisensteinInteger) 
factoriseBadPrime e = (exponent, quotient)
    where exponent = divideBy3 norm
          norm = E.norm e
          quotient = A.quot e badPowerPrime
          badPowerPrime = badPrime ^ exponent
          badPrime = 1 E.:+ (-1)

-- CAN AVOID GUARDS?

-- This function checks how many times 3 divides an integer @x@
-- It need not check the case when @x <= 0@ as this is never called
divideBy3 :: Integer -> Int
divideBy3 x
    | (x `mod` 3 == 0)    = 1 + (divideBy3 remainder)
    | otherwise           = 0
        where remainder = x `div` 3



-- This function takes an Eisenstein number and returns its primary decomposition @(exponent, factor)@
-- That is, given @e@ coprime with 3, it returns a unique integer (mod 6) @exponent@ and a unique
-- Eisenstein number @factor@ such that @(1 + ω)^exponent * e = 1 + 3*factor@.
-- Note that L.findIndex cannot return Nothing. This happens only if @e@ is
-- not coprime with 3.
getPrimaryDecomposition :: E.EisensteinInteger -> (Int, E.EisensteinInteger)
getPrimaryDecomposition e = (exponent, factor)
    where factor = unit * e
          unit = (1 E.:+ 1)^exponent
          Just exponent = L.findIndex (== 1) listOfRemainders 
          listOfRemainders = Prelude.map (\x -> A.rem x 3) listOfAssociates
          listOfAssociates = E.associates e
