-- |
-- Module:      Math.NumberTheory.GaussianIntegers
-- Copyright:   (c) 2016 Chris Fredrickson, Google Inc.
-- Licence:     MIT
-- Maintainer:  Chris Fredrickson <chris.p.fredrickson@gmail.com>
--
-- This module exports functions for manipulating Gaussian integers, including
-- computing their prime factorisations.
--

{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}

module Math.NumberTheory.Quadratic.GaussianIntegers (
    GaussianInteger(..),
    ι,
    conjugate,
    norm,
    primes,
    findPrime,
) where

import Prelude hiding (quot, quotRem)
import Control.DeepSeq (NFData)
import Data.Coerce
import Data.Euclidean
import Data.List (mapAccumL, partition)
import Data.Maybe
import Data.Ord (comparing)
import qualified Data.Semiring as S
import GHC.Generics

import Math.NumberTheory.Moduli.Sqrt
import Math.NumberTheory.Roots (integerSquareRoot)
import Math.NumberTheory.Primes.Types
import qualified Math.NumberTheory.Primes as U
import Math.NumberTheory.Utils              (mergeBy)
import Math.NumberTheory.Utils.FromIntegral

infix 6 :+
-- |A Gaussian integer is a+bi, where a and b are both integers.
data GaussianInteger = (:+) { real :: !Integer, imag :: !Integer }
    deriving (Eq, Ord, Generic)

instance NFData GaussianInteger

-- |The imaginary unit, where
--
-- > ι .^ 2 == -1
ι :: GaussianInteger
ι = 0 :+ 1

instance Show GaussianInteger where
    show (a :+ b)
        | b == 0     = show a
        | a == 0     = s ++ b'
        | otherwise  = show a ++ op ++ b'
        where
            b' = if abs b == 1 then "ι" else show (abs b) ++ "*ι"
            op = if b > 0 then "+" else "-"
            s  = if b > 0 then "" else "-"

instance Num GaussianInteger where
    (+) (a :+ b) (c :+ d) = (a + c) :+ (b + d)
    (*) (a :+ b) (c :+ d) = (a * c - b * d) :+ (a * d + b * c)
    abs = fst . absSignum
    negate (a :+ b) = (-a) :+ (-b)
    fromInteger n = n :+ 0
    signum = snd . absSignum

instance S.Semiring GaussianInteger where
    plus          = (+)
    times         = (*)
    zero          = 0 :+ 0
    one           = 1 :+ 0
    fromNatural n = fromIntegral n :+ 0

instance S.Ring GaussianInteger where
    negate = negate

absSignum :: GaussianInteger -> (GaussianInteger, GaussianInteger)
absSignum z@(a :+ b)
    | a == 0 && b == 0 =   (z, 0)              -- origin
    | a >  0 && b >= 0 =   (z, 1)              -- first quadrant: (0, inf) x [0, inf)i
    | a <= 0 && b >  0 =   (b  :+ (-a), ι)     -- second quadrant: (-inf, 0] x (0, inf)i
    | a <  0 && b <= 0 = ((-a) :+ (-b), -1)    -- third quadrant: (-inf, 0) x (-inf, 0]i
    | otherwise        = ((-b) :+   a, -ι)     -- fourth quadrant: [0, inf) x (-inf, 0)i

instance GcdDomain GaussianInteger

instance Euclidean GaussianInteger where
    degree = fromInteger . norm
    quotRem = divHelper

divHelper
    :: GaussianInteger
    -> GaussianInteger
    -> (GaussianInteger, GaussianInteger)
divHelper g h = (q, r)
    where
        nr :+ ni = g * conjugate h
        denom = norm h
        q = ((nr + signum nr * denom `quot` 2) `quot` denom) :+ ((ni + signum ni * denom `quot` 2) `quot` denom)
        r = g - h * q

-- |Conjugate a Gaussian integer.
conjugate :: GaussianInteger -> GaussianInteger
conjugate (r :+ i) = r :+ (-i)

-- |The square of the magnitude of a Gaussian integer.
norm :: GaussianInteger -> Integer
norm (x :+ y) = x * x + y * y

-- |Compute whether a given Gaussian integer is prime.
isPrime :: GaussianInteger -> Bool
isPrime g@(x :+ y)
    | x == 0 && y /= 0 = abs y `mod` 4 == 3 && isJust (U.isPrime y)
    | y == 0 && x /= 0 = abs x `mod` 4 == 3 && isJust (U.isPrime x)
    | otherwise        = isJust $ U.isPrime $ norm g

-- |An infinite list of the Gaussian primes. Uses primes in Z to exhaustively
-- generate all Gaussian primes (up to associates), in order of ascending
-- magnitude.
primes :: [U.Prime GaussianInteger]
primes = coerce $ (1 :+ 1) : mergeBy (comparing norm) l r
  where
    leftPrimes, rightPrimes :: [Prime Integer]
    (leftPrimes, rightPrimes) = partition (\p -> unPrime p `mod` 4 == 3) [U.nextPrime 3 ..]
    l = [unPrime p :+ 0 | p <- leftPrimes]
    r = [g | p <- rightPrimes, let Prime (x :+ y) = findPrime p, g <- [x :+ y, y :+ x]]


-- |Find a Gaussian integer whose norm is the given prime number
-- of form 4k + 1 using
-- <http://www.ams.org/journals/mcom/1972-26-120/S0025-5718-1972-0314745-6/S0025-5718-1972-0314745-6.pdf Hermite-Serret algorithm>.
findPrime :: Prime Integer -> U.Prime GaussianInteger
findPrime p = case sqrtsModPrime (-1) p of
    []    -> error "findPrime: an argument must be prime p = 4k + 1"
    z : _ -> Prime $ go (unPrime p) z -- Effectively we calculate gcdG' (p :+ 0) (z :+ 1)
    where
        sqrtp :: Integer
        sqrtp = integerSquareRoot (unPrime p)

        go :: Integer -> Integer -> GaussianInteger
        go g h
            | g <= sqrtp = g :+ h
            | otherwise  = go h (g `mod` h)

-- | Compute the prime factorisation of a Gaussian integer. This is unique up to units (+/- 1, +/- i).
-- Unit factors are not included in the result.
factorise :: GaussianInteger -> [(Prime GaussianInteger, Word)]
factorise g = concat $ snd $ mapAccumL go g (U.factorise $ norm g)
    where
        go :: GaussianInteger -> (Prime Integer, Word) -> (GaussianInteger, [(Prime GaussianInteger, Word)])
        go z (Prime 2, e) = (divideByTwo z, [(Prime (1 :+ 1), e)])
        go z (p, e)
            | unPrime p `mod` 4 == 3
            = let e' = e `quot` 2 in (z `quotI` (unPrime p ^ e'), [(Prime (unPrime p :+ 0), e')])
            | otherwise
            = (z', filter ((> 0) . snd) [(gp, k), (gp', k')])
                where
                    gp = findPrime p
                    (k, k', z') = divideByPrime gp (unPrime p) e z
                    gp' = Prime (abs (conjugate (unPrime gp)))

-- | Remove all (1:+1) factors from the argument,
-- avoiding complex division.
divideByTwo :: GaussianInteger -> GaussianInteger
divideByTwo z@(x :+ y)
    | even x, even y
    = divideByTwo $ z `quotI` 2
    | odd x, odd y
    = (x - y) `quot` 2 :+ (x + y) `quot` 2
    | otherwise
    = z

-- | Remove p and conj p factors from the argument,
-- avoiding complex division.
divideByPrime
    :: Prime GaussianInteger -- ^ Gaussian prime p
    -> Integer               -- ^ Precomputed norm of p, of form 4k + 1
    -> Word                  -- ^ Expected number of factors (either p or conj p)
                             --   in Gaussian integer z
    -> GaussianInteger       -- ^ Gaussian integer z
    -> ( Word                -- Multiplicity of factor p in z
       , Word                -- Multiplicity of factor conj p in z
       , GaussianInteger     -- Remaining Gaussian integer
       )
divideByPrime p np k = go k 0
    where
        go :: Word -> Word -> GaussianInteger -> (Word, Word, GaussianInteger)
        go 0 d z = (d, d, z)
        go c d z
            | c >= 2
            , Just z' <- z `quotEvenI` np
            = go (c - 2) (d + 1) z'
        go c d z = (d + d1, d + d2, z'')
            where
                (d1, z') = go1 c 0 z
                d2 = c - d1
                z'' = head $ drop (wordToInt d2)
                    $ iterate (\g -> fromMaybe err $ (g * unPrime p) `quotEvenI` np) z'

        go1 :: Word -> Word -> GaussianInteger -> (Word, GaussianInteger)
        go1 0 d z = (d, z)
        go1 c d z
            | Just z' <- (z * conjugate (unPrime p)) `quotEvenI` np
            = go1 (c - 1) (d + 1) z'
            | otherwise
            = (d, z)

        err = error $ "divideByPrime: malformed arguments" ++ show (p, np, k)

quotI :: GaussianInteger -> Integer -> GaussianInteger
quotI (x :+ y) n = (x `quot` n :+ y `quot` n)

quotEvenI :: GaussianInteger -> Integer -> Maybe GaussianInteger
quotEvenI (x :+ y) n
    | xr == 0
    , yr == 0
    = Just (xq :+ yq)
    | otherwise
    = Nothing
    where
        (xq, xr) = x `quotRem` n
        (yq, yr) = y `quotRem` n

-------------------------------------------------------------------------------

instance U.UniqueFactorisation GaussianInteger where
  factorise 0 = []
  factorise g = coerce $ factorise g

  isPrime g = if isPrime g then Just (Prime g) else Nothing
