{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}



module Math.NumberTheory.Moduli.CubicSymbol
      ( CubicSymbol(..)
      , conj
      , cubicSymbol
      )where



import qualified Math.NumberTheory.Quadratic.EisensteinIntegers as E
    ( EisensteinInteger(..)
    , ω
    , norm
    , ids
    )
import qualified Math.NumberTheory.Utils.FromIntegral as T (wordToInt, wordToInteger)
import qualified Math.NumberTheory.Utils as U (splitOff)
import qualified Data.Mod.Word as M (Mod (..), unMod)
import qualified Data.Euclidean as A (quot, rem)
import qualified Data.Semigroup as S (stimes)
import qualified Data.List as L (findIndex)





data CubicSymbol = Zero | Omega | OmegaSquare | One deriving (Eq)



instance Semigroup CubicSymbol where
    Zero <> _                    = Zero
    _ <> Zero                    = Zero
    One <> y                     = y
    x <> One                     = x
    Omega <> Omega               = OmegaSquare
    Omega <> OmegaSquare         = One
    OmegaSquare <> Omega         = One
    OmegaSquare <> OmegaSquare   = Omega



instance Show CubicSymbol where
    show = \case
        Zero         -> "0"
        Omega        -> "ω"
        OmegaSquare  -> "ω²"
        One          -> "1"



conj :: CubicSymbol -> CubicSymbol
conj = \case
    Zero          -> Zero
    Omega         -> OmegaSquare
    OmegaSquare   -> Omega
    One           -> One





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



-- This function takes two Eisenstein integers and returns their cubic residue character.
-- Note that the second argument must be coprime to 3 else the algorithm returns an error.
cubicSymbol :: E.EisensteinInteger -> E.EisensteinInteger -> CubicSymbol
cubicSymbol alpha beta = case ((E.norm beta) `mod` 3) of
    -- This checks whether beta is coprime to 3, i.e. divisible by @1 - ω@
    -- In particular, it returns an error if @beta == 0@
    0 -> error "Math.NumberTheory.Moduli.CubicSymbol: denominator is not coprime to 3."
    _ -> cubicSymbolHelper alpha beta



cubicSymbolHelper :: E.EisensteinInteger -> E.EisensteinInteger -> CubicSymbol
cubicSymbolHelper alpha beta = (cubicReciprocity primaryRemainder primaryBeta) <> newSymbol
    where (primaryRemainder, primaryBeta, symbolExponent) = extractPrimaryContributions remainder beta
          remainder = A.rem alpha beta
          newSymbol = exponentiation (unmodularExponent) Omega
          unmodularExponent = T.wordToInt (M.unMod symbolExponent)
          exponentiation = \k x -> if k == 0 then One else S.stimes k x



-- This function first checks if its arguments are zero or units. If they are not,
-- it invokes cubic reciprocity by calling cubicSymbolHelper with swapped arguments.
cubicReciprocity :: E.EisensteinInteger -> E.EisensteinInteger -> CubicSymbol
-- Note @cubicReciprocity 0 1 = One@. It turns out it is better to adopt this convention.
cubicReciprocity _ 1 = One
-- Checks if first argument is zero. Note @betaPrimary@ cannot be zero.
cubicReciprocity 0 _ = Zero
-- This checks if the first argument is a unit. Because it's primary,
-- it is enough to pattern match with 1.
cubicReciprocity 1 _ = One
-- Otherwise, cubic reciprocity is called.
cubicReciprocity alpha beta = cubicSymbolHelper beta alpha



-- This function takes two Eisenstein intgers @alpha@ and @beta@ and returns three
-- arguments @(gamma, delta, contribution)@. @gamma@ and @delta@ are the associated
-- primary numbers to alpha and beta respectively. @contribution@ is a an integer
-- defined mod 6 which measures the difference between the cubic residue of @alpha@
-- and @beta with respect to the cubic residue of @gamma@ and @delta@
extractPrimaryContributions :: E.EisensteinInteger -> E.EisensteinInteger -> (E.EisensteinInteger, E.EisensteinInteger, M.Mod 3)
extractPrimaryContributions alpha beta = (gamma, delta, contribution)
    where contribution = j*m - i*m -i*n
          [i, j, m, n] = map (conversion) [iInt, jInt, mInt, nInt]
          conversion = \x -> (fromIntegral x) :: M.Mod 3
          mInt E.:+ nInt = A.quot (delta - 1) 3
          (iInt, gamma) = getPrimaryDecomposition alphaThreeFree
          (_, delta) = getPrimaryDecomposition beta
          (jInt, alphaThreeFree) = factoriseBadPrime alpha



-- This function takes an Eisenstein number @e@ and returns @(powerPrime, quotient)@
-- where exponent is the largest integer such that @(1 - ω)^powerPrime@ divides @e@.
-- @quotient@ is the quotient of @e@ by @(1 - ω)^powerPrime@
factoriseBadPrime :: E.EisensteinInteger -> (Integer, E.EisensteinInteger)
factoriseBadPrime e = (powerPrime, quotient)
    where quotient = A.quot e badPowerPrime
          badPowerPrime = badPrime ^ powerPrime
          badPrime = 1 - E.ω
          powerPrime = T.wordToInteger (fst wordExponent)
          wordExponent = U.splitOff 3 norm
          norm = E.norm e



-- This function takes an Eisenstein number and returns its primary decomposition @(powerUnit, factor)@
-- That is, given @e@ coprime with 3, it returns a unique integer (mod 6) @powerUnit@ and a unique
-- Eisenstein number @factor@ such that @(1 + ω)^powerUnit * e = 1 + 3*factor@.
-- Note that L.findIndex cannot return Nothing. This happens only if @e@ is
-- not coprime with 3. This cannot happen since @factoriseBadPrime@ is called before.
getPrimaryDecomposition :: E.EisensteinInteger -> (Integer, E.EisensteinInteger)
-- This is the case where a common factor between @alpha@ and @beta@ is detected.
-- In this instance @cubicReciprocity@ will return @Zero@.
-- Strictly speaking, this is not a primary decomposition.
getPrimaryDecomposition 0 = (0, 0)
getPrimaryDecomposition e = (toInteger powerUnit, factor)
    where factor = unit * e
          unit = (1 + E.ω)^powerUnit
          powerUnit = case findPowerUnit of
              Just u  -> u
              Nothing -> error "Math.NumberTheory.Moduli.CubicSymbol: primary decomposition failed."
          findPowerUnit = L.findIndex (== 1) listOfRemainders
          listOfRemainders = map (\x -> A.rem x 3) listOfAssociates
          -- Note that the associates in @listOfAssociates@ are ordered in the following way:
          -- The i^th element of @listOfAssociate@ is @e * (1 + ω)^i@ starting from i = 0@
          -- That is @e@ times the i^th unit counting anticlockwise starting with 1.
          listOfAssociates = map (*e) E.ids
