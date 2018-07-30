-- |
-- Module:      Math.NumberTheory.GaussianIntegers
-- Copyright:   (c) 2016 Chris Fredrickson, Google Inc.
-- Licence:     MIT
-- Maintainer:  Chris Fredrickson <chris.p.fredrickson@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- This module exports functions for manipulating Gaussian integers, including
-- computing their prime factorisations.
--

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Math.NumberTheory.GaussianIntegers (
    GaussianInteger(..),
    ι,
    conjugate,
    norm,
    divModG,
    divG,
    modG,
    quotRemG,
    quotG,
    remG,
    (.^),
    isPrime,
    primes,
    gcdG,
    gcdG',
    findPrime',
    factorise,
) where

import Data.List (mapAccumL)
import GHC.Generics

import qualified Math.NumberTheory.Moduli as Moduli
import Math.NumberTheory.Moduli.Sqrt (FieldCharacteristic(..))
import Math.NumberTheory.Primes.Types (PrimeNat(..))
import qualified Math.NumberTheory.Primes.Factorisation as Factorisation
import qualified Math.NumberTheory.Primes.Sieve as Sieve
import qualified Math.NumberTheory.Primes.Testing as Testing
import Math.NumberTheory.Utils.FromIntegral (integerToNatural)

infix 6 :+
infixr 8 .^
-- |A Gaussian integer is a+bi, where a and b are both integers.
data GaussianInteger = (:+) { real :: !Integer, imag :: !Integer }
    deriving (Eq, Ord, Generic)

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
    abs z@(a :+ b)
        | a == 0 && b == 0 =   z             -- origin
        | a >  0 && b >= 0 =   z             -- first quadrant: (0, inf) x [0, inf)i
        | a <= 0 && b >  0 =   b  :+ (-a)    -- second quadrant: (-inf, 0] x (0, inf)i
        | a <  0 && b <= 0 = (-a) :+ (-b)    -- third quadrant: (-inf, 0) x (-inf, 0]i
        | otherwise        = (-b) :+   a     -- fourth quadrant: [0, inf) x (-inf, 0)i
    negate (a :+ b) = (-a) :+ (-b)
    fromInteger n = n :+ 0
    signum z@(a :+ b)
        | a == 0 && b == 0 = z               -- hole at origin
        | otherwise        = z `divG` abs z

-- |Simultaneous 'quot' and 'rem'.
quotRemG :: GaussianInteger -> GaussianInteger -> (GaussianInteger, GaussianInteger)
quotRemG = divHelper quot

-- |Gaussian integer division, truncating toward zero.
quotG :: GaussianInteger -> GaussianInteger -> GaussianInteger
n `quotG` d = q where (q,_) = quotRemG n d

-- |Gaussian integer remainder, satisfying
--
-- > (x `quotG` y)*y + (x `remG` y) == x
remG :: GaussianInteger -> GaussianInteger -> GaussianInteger
n `remG`  d = r where (_,r) = quotRemG n d

-- |Simultaneous 'div' and 'mod'.
divModG :: GaussianInteger -> GaussianInteger -> (GaussianInteger, GaussianInteger)
divModG = divHelper div

-- |Gaussian integer division, truncating toward negative infinity.
divG :: GaussianInteger -> GaussianInteger -> GaussianInteger
n `divG` d = q where (q,_) = divModG n d

-- |Gaussian integer remainder, satisfying
--
-- > (x `divG` y)*y + (x `modG` y) == x
modG :: GaussianInteger -> GaussianInteger -> GaussianInteger
n `modG` d = r where (_,r) = divModG n d

divHelper :: (Integer -> Integer -> Integer) -> GaussianInteger -> GaussianInteger -> (GaussianInteger, GaussianInteger)
divHelper divide g h =
    let nr :+ ni = g * conjugate h
        denom = norm h
        q = divide nr denom :+ divide ni denom
        p = h * q
    in (q, g - p)

-- |Conjugate a Gaussian integer.
conjugate :: GaussianInteger -> GaussianInteger
conjugate (r :+ i) = r :+ (-i)

-- |The square of the magnitude of a Gaussian integer.
norm :: GaussianInteger -> Integer
norm (x :+ y) = x * x + y * y

-- |Compute whether a given Gaussian integer is prime.
isPrime :: GaussianInteger -> Bool
isPrime g@(x :+ y)
    | x == 0 && y /= 0 = abs y `mod` 4 == 3 && Testing.isPrime y
    | y == 0 && x /= 0 = abs x `mod` 4 == 3 && Testing.isPrime x
    | otherwise        = Testing.isPrime $ norm g

-- |An infinite list of the Gaussian primes. Uses primes in Z to exhaustively
-- generate all Gaussian primes, but not quite in order of ascending magnitude.
primes :: [GaussianInteger]
primes = [ g
         | p <- Sieve.primes
         , g <- if p `mod` 4 == 3
                then [p :+ 0]
                else
                    if p == 2
                    then [1 :+ 1]
                    else let x :+ y = findPrime' p
                         in [x :+ y, y :+ x]
         ]

-- |Compute the GCD of two Gaussian integers. Enforces the precondition that each
-- integer must be in the first quadrant (or zero).
gcdG :: GaussianInteger -> GaussianInteger -> GaussianInteger
gcdG g h = gcdG' (abs g) (abs h)

-- |Compute the GCD of two Gauss integers. Does not check the precondition.
gcdG' :: GaussianInteger -> GaussianInteger -> GaussianInteger
gcdG' g h
    | h == 0    = g --done recursing
    | otherwise = gcdG' h (abs (g `modG` h))

-- |Find a Gaussian integer whose norm is the given prime number.
findPrime' :: Integer -> GaussianInteger
findPrime' p = case Moduli.sqrtModMaybe (-1) (FieldCharacteristic (PrimeNat . integerToNatural $ p) 1) of
    Nothing -> error "findPrime': impossible happened"
    Just z  -> gcdG' (p :+ 0) (z :+ 1)

-- |Raise a Gaussian integer to a given power.
(.^) :: (Integral a) => GaussianInteger -> a -> GaussianInteger
a .^ e
    | e < 0 && norm a == 1 =
        case a of
            1    :+ 0 -> 1
            (-1) :+ 0 -> if even e then 1 else (-1)
            0    :+ 1 -> (0 :+ (-1)) .^ (abs e `mod` 4)
            _         -> (0 :+ 1) .^ (abs e `mod` 4)
    | e < 0     = error "Cannot exponentiate non-unit Gaussian Int to negative power"
    | a == 0    = 0
    | e == 0    = 1
    | even e    = s * s
    | otherwise = a * a .^ (e - 1)
    where
    s = a .^ div e 2
{-# DEPRECATED (.^) "Use (^) instead." #-}

-- |Compute the prime factorisation of a Gaussian integer. This is unique up to units (+/- 1, +/- i).
-- Unit factors are not included in the result.
factorise :: GaussianInteger -> [(GaussianInteger, Int)]
factorise g = concat $ snd $ mapAccumL go g (Factorisation.factorise $ norm g)
    where
        go :: GaussianInteger -> (Integer, Int) -> (GaussianInteger, [(GaussianInteger, Int)])
        go z (2, e) = (divideTwos z, [(1 :+ 1, e)])
        go z (p, e)
            | p `mod` 4 == 3
            = let e' = e `quot` 2 in (z `quotI` (p ^ e'), [(p :+ 0, e')])
            | otherwise
            = (z `quotG` (gp ^ k) `quotG` (gp' ^ l), filter ((> 0) . snd) [(gp, k), (gp', l)])
                where
                    gp = findPrime' p
                    (k, _) = trialDivide z gp p
                    gp' = abs (conjugate gp)
                    l = e - k

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

divideTwos :: GaussianInteger -> GaussianInteger
divideTwos z@(x :+ y)
    | even x, even y
    = divideTwos $ z `quotI` 2
    | odd x, odd y
    = (x - y) `quot` 2 :+ (x + y) `quot` 2
    | otherwise
    = z

trialDivide
    :: GaussianInteger -- ^ Number to divide
    -> GaussianInteger -- ^ Gaussian prime
    -> Integer         -- ^ Norm of Gaussian prime, of form 4k + 1
    -> (Int, GaussianInteger)
trialDivide g p np = go 0 g
    where
        go :: Int -> GaussianInteger -> (Int, GaussianInteger)
        go k z
            | Just z' <- z `quotEvenI` np
            = go (k + 1) z'
            | Just z' <- (z * conjugate p) `quotEvenI` np
            = go (k + 1) z'
            | otherwise
            = (k, z)
