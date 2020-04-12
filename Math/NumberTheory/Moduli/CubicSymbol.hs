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
    (<>) = multiplication

instance Show CubicSymbol where
    show = \case
        Zero         -> show 0
        Omega        -> "ω"
        OmegaSquare  -> "ω²"
        One          -> show 1


multiplication :: CubicSymbol -> CubicSymbol -> CubicSymbol
multiplication Zero _ = Zero
multiplication _ Zero = Zero
multiplication One y = y
multiplication x One = x
multiplication Omega Omega = OmegaSquare
multiplication Omega OmegaSquare = One
multiplication OmegaSquare Omega = One
multiplication OmegaSquare OmegaSquare = Omega



exponentiation :: CubicSymbol -> Integer -> CubicSymbol
exponentiation Zero _ = Zero
exponentiation One _ = One
exponentiation _ 0 = One
exponentiation Omega 1 = Omega
exponentiation Omega 2 = OmegaSquare
exponentiation OmegaSquare 1 = OmegaSquare
exponentiation OmegaSquare 2 = Omega



conj :: CubicSymbol -> CubicSymbol
conj = \case
    Zero          -> Zero
    Omega         -> OmegaSquare
    OmegaSquare   -> Omega
    One           -> One

-- Converts Eisenstein integer to CubicSymbol value.
toCubicSymbol :: E.EisensteinInteger -> CubicSymbol
toCubicSymbol = \case
    0               -> Zero
    0 E.:+ 1        -> Omega
    (-1) E.:+ (-1)  -> OmegaSquare
    1               -> One




cubicSymbol :: E.EisensteinInteger -> E.EisensteinInteger -> CubicSymbol
cubicSymbol alpha n = Prelude.foldr (g) One factorisation
    where factorisation = P.factorise n
          g factor symbol = newSymbol <> symbol
              where newSymbol = primePowerCubicSymbol alpha factor



primePowerCubicSymbol :: E.EisensteinInteger -> (P.Prime E.EisensteinInteger, Word) -> CubicSymbol
primePowerCubicSymbol alpha (primeNumber, wordExponent) = exponentiation symbol remainder
    where remainder = exponent `rem` 3
          exponent = Prelude.fromIntegral wordExponent
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
              listOfAlphas = L.genericReplicate alphaExponent alpha
              -- Exponent is defined to be 1/3*(@primeNorm@ - 1).
              alphaExponent = primeNorm `div` 3
              