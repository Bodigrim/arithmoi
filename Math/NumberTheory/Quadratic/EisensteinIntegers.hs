-- |
-- Module:      Math.NumberTheory.EisensteinIntegers
-- Copyright:   (c) 2018 Alexandre Rodrigues Baldé
-- Licence:     MIT
-- Maintainer:  Alexandre Rodrigues Baldé <alexandrer_b@outlook.com>
--
-- This module exports functions for manipulating Eisenstein integers, including
-- computing their prime factorisations.
--

{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeFamilies   #-}

module Math.NumberTheory.Quadratic.EisensteinIntegers
  ( EisensteinInteger(..)
  , ω
  , conjugate
  , norm
  , associates
  , ids

  -- * Primality functions
  , findPrime
  , primes
  ) where

import Prelude hiding (quot, quotRem, gcd)
import Control.DeepSeq
import Data.Coerce
import Data.Euclidean
import Data.List                                       (mapAccumL, partition)
import Data.Maybe
import Data.Ord                                        (comparing)
import qualified Data.Semiring as S
import GHC.Generics                                    (Generic)

import Math.NumberTheory.Moduli.Sqrt
import Math.NumberTheory.Primes.Types
import qualified Math.NumberTheory.Primes as U
import Math.NumberTheory.Utils                          (mergeBy)
import Math.NumberTheory.Utils.FromIntegral

infix 6 :+

-- | An Eisenstein integer is @a + bω@, where @a@ and @b@ are both integers.
data EisensteinInteger = !Integer :+ !Integer
    deriving (Eq, Ord, Generic)

instance NFData EisensteinInteger

-- | The imaginary unit for Eisenstein integers, where
--
-- > ω == (-1/2) + ((sqrt 3)/2)ι == exp(2*pi*ι/3)
-- and @ι@ is the usual imaginary unit with @ι² == -1@.
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
    (*) (a :+ b) (c :+ d) = (a * c - b * d) :+ (b * (c - d) + a * d)
    abs = fst . absSignum
    negate (a :+ b) = (-a) :+ (-b)
    fromInteger n = n :+ 0
    signum = snd . absSignum

instance S.Semiring EisensteinInteger where
    plus          = (+)
    times         = (*)
    zero          = 0 :+ 0
    one           = 1 :+ 0
    fromNatural n = fromIntegral n :+ 0

instance S.Ring EisensteinInteger where
    negate = negate

-- | Returns an @EisensteinInteger@'s sign, and its associate in the first
-- sextant.
absSignum :: EisensteinInteger -> (EisensteinInteger, EisensteinInteger)
absSignum 0 = (0, 0)
absSignum z@(a :+ b)
  -- first sextant: 0 ≤ Arg(z) < π/3
  | a > b && b >= 0 = (z, 1)
  -- second sextant: π/3 ≤ Arg(z) < 2π/3
  | b >= a && a > 0 = (b :+ (b - a), 1 :+ 1)
  -- third sextant: 2π/3 ≤ Arg(z) < π
  | b > 0 && 0 >= a = ((b - a) :+ (-a), 0 :+ 1)
  -- fourth sextant: -π ≤ Arg(z) < -2π/3
  | a < b && b <= 0 = (-z, -1)
  -- fifth sextant: -2π/3 ≤ Arg(η) < -π/3
  | b <= a && a < 0 = ((-b) :+ (a - b), (-1) :+ (-1))
  -- sixth sextant: -π/3 ≤ Arg(η) < 0
  | otherwise       = ((a - b) :+ a, 0 :+ (-1))

-- | List of all Eisenstein units, counterclockwise across all sextants,
-- starting with @1@.
ids :: [EisensteinInteger]
ids = take 6 (iterate ((1 + ω) *) 1)

-- | Produce a list of an @EisensteinInteger@'s associates.
associates :: EisensteinInteger -> [EisensteinInteger]
associates e = map (e *) ids

instance GcdDomain EisensteinInteger

instance Euclidean EisensteinInteger where
  degree = fromInteger . norm
  quotRem x (d :+ 0) = quotRemInt x d
  quotRem x y = (q, x - q * y)
    where
      (q, _) = quotRemInt (x * conjugate y) (norm y)

quotRemInt :: EisensteinInteger -> Integer -> (EisensteinInteger, EisensteinInteger)
quotRemInt z   1  = ( z, 0)
quotRemInt z (-1) = (-z, 0)
quotRemInt (a :+ b) c = (qa :+ qb, (ra - bumpA) :+ (rb - bumpB))
  where
    halfC    = abs c `quot` 2
    bumpA    = signum a * halfC
    bumpB    = signum b * halfC
    (qa, ra) = (a + bumpA) `quotRem` c
    (qb, rb) = (b + bumpB) `quotRem` c

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
isPrime e | e == 0                     = False
          -- Special case, @1 - ω@ is the only Eisenstein prime with norm @3@,
          --  and @abs (1 - ω) = 2 + ω@.
          | a' == 2 && b' == 1         = True
          | b' == 0 && a' `mod` 3 == 2 = isJust $ U.isPrime a'
          | nE `mod` 3 == 1            = isJust $ U.isPrime nE
          | otherwise = False
  where nE       = norm e
        a' :+ b' = abs e

-- | Remove @1 - ω@ factors from an @EisensteinInteger@, and calculate that
-- prime's multiplicity in the number's factorisation.
divideByThree :: EisensteinInteger -> (Word, EisensteinInteger)
divideByThree = go 0
  where
    go :: Word -> EisensteinInteger -> (Word, EisensteinInteger)
    go !n z@(a :+ b) | r1 == 0 && r2 == 0 = go (n + 1) (q1 :+ q2)
                     | otherwise          = (n, abs z)
      where
        -- @(a + a - b) :+ (a + b)@ is @z * (2 :+ 1)@, and @z * (2 :+ 1)/3@
        -- is the same as @z / (1 :+ (-1))@.
        (q1, r1) = divMod (a + a - b) 3
        (q2, r2) = divMod (a + b) 3

-- | Find an Eisenstein integer whose norm is the given prime number
-- in the form @3k + 1@.
--
-- >>> import Math.NumberTheory.Primes (nextPrime)
-- >>> findPrime (nextPrime 7)
-- Prime 3+2*ω
findPrime :: Prime Integer -> U.Prime EisensteinInteger
findPrime p = case (r, sqrtsModPrime (9 * q * q - 1) p) of
  (1, z : _) -> Prime $ abs $ gcd (unPrime p :+ 0) ((z - 3 * q) :+ 1)
  _ -> error "findPrime: argument must be prime p = 6k + 1"
  where
    (q, r) = unPrime p `quotRem` 6

-- | An infinite list of Eisenstein primes. Uses primes in @Z@ to exhaustively
-- generate all Eisenstein primes in order of ascending norm.
--
-- * Every prime is in the first sextant, so the list contains no associates.
-- * Eisenstein primes from the whole complex plane can be generated by
-- applying 'associates' to each prime in this list.
--
-- >>> take 10 primes
-- [Prime 2+ω,Prime 2,Prime 3+2*ω,Prime 3+ω,Prime 4+3*ω,Prime 4+ω,Prime 5+3*ω,Prime 5+2*ω,Prime 5,Prime 6+5*ω]
primes :: [Prime EisensteinInteger]
primes = coerce $ (2 :+ 1) : mergeBy (comparing norm) l r
  where
    leftPrimes, rightPrimes :: [Prime Integer]
    (leftPrimes, rightPrimes) = partition (\p -> unPrime p `mod` 3 == 2) [U.nextPrime 2 ..]
    rightPrimes' = filter (\prime -> unPrime prime `mod` 3 == 1) $ tail rightPrimes
    l = [unPrime p :+ 0 | p <- leftPrimes]
    r = [g | p <- rightPrimes', let x :+ y = unPrime (findPrime p), g <- [x :+ y, x :+ (x - y)]]

-- | [Implementation notes for factorise function]
--
-- Compute the prime factorisation of a Eisenstein integer.
--
--     1. This function works by factorising the norm of an Eisenstein integer
--        and then, for each prime factor, finding the Eisenstein prime whose norm
--        is said prime factor with @findPrime@.
--     2. This is only possible because the norm function of the Euclidean Domain of
--        Eisenstein integers is multiplicative: @norm (e1 * e2) == norm e1 * norm e2@
--        for any two @EisensteinInteger@s @e1, e2@.
--     3. In the previously mentioned work <http://thekeep.eiu.edu/theses/2467 Bandara, Sarada, "An Exposition of the Eisenstein Integers" (2016)>,
--        in Theorem 8.4 in Chapter 8, a way is given to express any Eisenstein
--        integer @μ@ as @(-1)^a * ω^b * (1 - ω)^c * product [π_i^a_i | i <- [1..N]]@
--        where @a, b, c, a_i@ are nonnegative integers, @N > 1@ is an integer and
--        @π_i@ are Eisenstein primes.
--
-- Aplying @norm@ to both sides of the equation from Theorem 8.4:
--
-- 1. @norm μ = norm ( (-1)^a * ω^b * (1 - ω)^c * product [ π_i^a_i | i <- [1..N]] ) ==@
-- 2. @norm μ = norm ((-1)^a) * norm (ω^b) * norm ((1 - ω)^c) * norm (product [ π_i^a_i | i <- [1..N]]) ==@
-- 3. @norm μ = (norm (-1))^a * (norm ω)^b * (norm (1 - ω))^c * product [ norm (π_i^a_i) | i <- [1..N]] ==@
-- 4. @norm μ = (norm (-1))^a * (norm ω)^b * (norm (1 - ω))^c * product [ (norm π_i)^a_i) | i <- [1..N]] ==@
-- 5. @norm μ = 1^a * 1^b * 3^c * product [ (norm π_i)^a_i) | i <- [1..N]] ==@
-- 6. @norm μ = 3^c * product [ (norm π_i)^a_i) | i <- [1..N]] ==@
--
-- where @a, b, c, a_i@ are nonnegative integers, and @N > 1@ is an integer.
--
-- The remainder of the Eisenstein integer factorisation problem is about
-- finding appropriate Eisenstein primes @[e_i | i <- [1..M]]@ such that
-- @map norm [e_i | i <- [1..M]] == map norm [π_i | i <- [1..N]]@
-- where @ 1 < N <= M@ are integers and @==@ is equality on sets
-- (i.e.duplicates do not matter).
--
-- NB: The reason @M >= N@ is because the prime factors of an Eisenstein integer
-- may include a prime factor and its conjugate (both have the same norm),
-- meaning the number may have more Eisenstein prime factors than its norm has
-- integer prime factors.
factorise :: EisensteinInteger -> [(Prime EisensteinInteger, Word)]
factorise g = concat $
              snd $
              mapAccumL go (abs g) (U.factorise $ norm g)
  where
    go :: EisensteinInteger -> (Prime Integer, Word) -> (EisensteinInteger, [(Prime EisensteinInteger, Word)])
    go z (Prime 3, e)
      | e == n    = (q, [(Prime (2 :+ 1), e)])
      | otherwise = error $ "3 is a prime factor of the norm of z = " ++ show z
                          ++ " with multiplicity " ++ show e
                          ++ " but (1 - ω) only divides z " ++ show n ++ "times."
      where
        -- Remove all @1 :+ (-1)@ (which is associated to @2 :+ 1@) factors
        -- from the argument.
        (n, q) = divideByThree z
    go z (p, e)
      | unPrime p `mod` 3 == 2
      = let e' = e `quot` 2 in (z `quotI` (unPrime p ^ e'), [(Prime (unPrime p :+ 0), e')])

      -- The @`rem` 3 == 0@ case need not be verified because the
      -- only Eisenstein primes whose norm are a multiple of 3
      -- are @1 - ω@ and its associates, which have already been
      -- removed by the above @go z (3, e)@ pattern match.
      -- This @otherwise@ is mandatorily @`mod` 3 == 1@.
      | otherwise   = (z', filter ((> 0) . snd) [(gp, k), (gp', k')])
      where
        gp = findPrime p
        x :+ y = unPrime gp
        -- @gp'@ is @gp@'s conjugate.
        gp' = Prime (x :+ (x - y))
        (k, k', z') = divideByPrime gp gp' (unPrime p) e z

        quotI (a :+ b) n = a `quot` n :+ b `quot` n

-- | Remove @p@ and @conjugate p@ factors from the argument, where
-- @p@ is an Eisenstein prime.
divideByPrime
    :: Prime EisensteinInteger -- ^ Eisenstein prime @p@
    -> Prime EisensteinInteger -- ^ Conjugate of @p@
    -> Integer                 -- ^ Precomputed norm of @p@, of form @4k + 1@
    -> Word                    -- ^ Expected number of factors (either @p@ or @conjugate p@)
                               --   in Eisenstein integer @z@
    -> EisensteinInteger       -- ^ Eisenstein integer @z@
    -> ( Word                  -- Multiplicity of factor @p@ in @z@
       , Word                  -- Multiplicity of factor @conjigate p@ in @z@
       , EisensteinInteger     -- Remaining Eisenstein integer
       )
divideByPrime p p' np k = go k 0
    where
        go :: Word -> Word -> EisensteinInteger -> (Word, Word, EisensteinInteger)
        go 0 d z = (d, d, z)
        go c d z | c >= 2, Just z' <- z `quotEvenI` np = go (c - 2) (d + 1) z'
        go c d z = (d + d1, d + d2, z'')
            where
                (d1, z') = go1 c 0 z
                d2 = c - d1
                z'' = iterate (\g -> fromMaybe err $ (g * unPrime p) `quotEvenI` np) z' !! max 0 (wordToInt d2)

        go1 :: Word -> Word -> EisensteinInteger -> (Word, EisensteinInteger)
        go1 0 d z = (d, z)
        go1 c d z
            | Just z' <- (z * unPrime p') `quotEvenI` np
            = go1 (c - 1) (d + 1) z'
            | otherwise
            = (d, z)

        err = error $ "divideByPrime: malformed arguments" ++ show (p, np, k)

-- | Divide an Eisenstein integer by an even integer.
quotEvenI :: EisensteinInteger -> Integer -> Maybe EisensteinInteger
quotEvenI (x :+ y) n
    | xr == 0 , yr == 0 = Just (xq :+ yq)
    | otherwise         = Nothing
  where
    (xq, xr) = x `quotRem` n
    (yq, yr) = y `quotRem` n

-------------------------------------------------------------------------------

-- | See the source code and Haddock comments for the @factorise@ and @isPrime@
-- functions in this module (they are not exported) for implementation
-- details.
instance U.UniqueFactorisation EisensteinInteger where
  factorise 0 = []
  factorise e = coerce $ factorise e

  isPrime e = if isPrime e then Just (Prime e) else Nothing
