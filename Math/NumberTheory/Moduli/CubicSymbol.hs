module Math.NumberTheory.Moduli.CubicSymbol where

import Math.NumberTheory.Quadratic.EisensteinIntegers
import qualified Math.NumberTheory.Quadratic.EisensteinIntegers as E

import qualified Math.NumberTheory.Primes as P
import qualified Data.Euclidean as A

data CubicSymbol = Zero | Omega | OmegaSquare | One


cubicSymbol :: EisensteinInteger -> EisensteinInteger -> CubicSymbol

cubicSymbol alpha prime
    -- Check whether @prime@ satifies norm and primality conditions.
    | primeNorm == 3                 = error ""
    | (P.isPrime prime) == Nothing   = error ""
    -- Return 0 if @prime@ divides @alpha@
    | otherwise                      = omegaSymbol residue

        -- A.rem returns the remainder of the Euclidean algorithm.
        where residue = A.rem alphaPower prime
              -- Converts Eisenstein integer to CubicSymbol value.
              omegaSymbol x
                  | x == 0               = Zero
                  | x == 0 E.:+ 1        = Omega
                  | x == (-1) E.:+ (-1)  = OmegaSquare
                  | x == 1               = One
                  | otherwise            = error ""

              primeNorm = E.norm prime
              alphaPower = alpha^alphaExponent
              -- Exponent is defined to be 1/3*(@primeNorm@ - 1).
              alphaExponent = primeNorm `div` 3
