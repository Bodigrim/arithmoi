module Math.NumberTheory.Moduli.CubicSymbol
    ( CubicSymbol(..)
    , conj
    , cubicSymbol
    ) where

import Math.NumberTheory.Quadratic.EisensteinIntegers
import qualified Math.NumberTheory.Quadratic.EisensteinIntegers as E

import qualified Math.NumberTheory.Primes as P
import qualified Data.Euclidean as A
import Data.Semigroup as S



data CubicSymbol = Zero | Omega | OmegaSquare | One deriving (Eq)

instance S.Semigroup CubicSymbol where
    (<>) = multiplication

instance Show CubicSymbol where
    show x
        | x == Zero         = show 0
        | x == Omega        = "ω"
        | x == OmegaSquare  = "ω²"
        | x == One          = show 1


multiplication :: CubicSymbol -> CubicSymbol -> CubicSymbol
multiplication x y
    | x == Zero || y == Zero                   = Zero
    | x == One                                 = y
    | y == One                                 = x
    | x == Omega && y == Omega                 = OmegaSquare
    | x == Omega && y == OmegaSquare           = One
    | x == OmegaSquare && y == Omega           = One
    | x == OmegaSquare && y == OmegaSquare     = Omega
    | otherwise                                = error ""

exponentiation :: CubicSymbol -> Integer -> CubicSymbol
exponentiation symbol exponent
    | symbol == Zero || symbol == One          = symbol
    | remainder == 0                           = One
    | symbol == Omega && remainder == 1        = Omega
    | symbol == Omega && remainder == 2        = OmegaSquare
    | symbol == OmegaSquare && remainder == 1  = OmegaSquare
    | symbol == OmegaSquare && remainder == 1  = Omega
    | otherwise                                = error ""
        where remainder = exponent `rem` 3

conj :: CubicSymbol -> CubicSymbol
conj x
    | x == Zero || x == One      = x
    | x == Omega                 = OmegaSquare
    | x == OmegaSquare           = Omega
    | otherwise                  = error ""



-- Converts Eisenstein integer to CubicSymbol value.
toCubicSymbol :: E.EisensteinInteger -> CubicSymbol
toCubicSymbol x
    | x == 0               = Zero
    | x == 0 E.:+ 1        = Omega
    | x == (-1) E.:+ (-1)  = OmegaSquare
    | x == 1               = One
    | otherwise            = error ""


cubicSymbol :: E.EisensteinInteger -> E.EisensteinInteger -> CubicSymbol
cubicSymbol alpha n = Prelude.foldr (g) One factorisation
    where factorisation = P.factorise n
          g factor symbol = newSymbol <> symbol
              where newSymbol = primePowerCubicSymbol alpha factor



primePowerCubicSymbol :: E.EisensteinInteger -> (P.Prime E.EisensteinInteger, Word) -> CubicSymbol
primePowerCubicSymbol alpha (primeNumber, wordExponent) = exponentiation symbol exponent
    where exponent = Prelude.fromIntegral wordExponent
          symbol =  primeCubicSymbol alpha prime
          prime = P.unPrime primeNumber


primeCubicSymbol :: E.EisensteinInteger -> E.EisensteinInteger -> CubicSymbol
primeCubicSymbol alpha prime
    -- Check whether @prime@ satifies norm and primality conditions.
    | primeNorm == 3                 = error "This Eisenstein number is not coprime to 3."
    -- Return 0 if @prime@ divides @alpha@
    | otherwise                      = toCubicSymbol residue

        -- A.rem returns the remainder of the Euclidean algorithm.
        where primeNorm = E.norm prime
              residue = Prelude.foldr (f) 1 listOfAlphas
              f = \x y -> A.rem (x * y) prime
              -- The function @take@ does not accept Integer values.
              listOfAlphas = [alpha | index <- [1..alphaExponent]]
              -- Exponent is defined to be 1/3*(@primeNorm@ - 1).
              alphaExponent = primeNorm `div` 3
              