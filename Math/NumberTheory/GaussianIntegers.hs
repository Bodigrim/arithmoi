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
    orderedPrimes,
    gcdG,
    gcdG',
    findPrime,
    findPrime',
    factorise,
) where

import Data.List (mapAccumL, partition)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import GHC.Generics

import qualified Math.NumberTheory.Moduli as Moduli
import Math.NumberTheory.Moduli.Sqrt (FieldCharacteristic(..))
import Math.NumberTheory.Powers (integerSquareRoot)
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
-- generate all Gaussian primes (up to associates), but not quite in order of
-- ascending magnitude. See 'orderedPrimes'.
primes :: [GaussianInteger]
primes = [ g
         | p <- Sieve.primes
         , g <- if p `mod` 4 == 3
                then [p :+ 0]
                else
                    if p == 2
                    then [1 :+ 1]
                    else let x :+ y = findPrime p
                         in [x :+ y, y :+ x]
         ]

-- |An infinite list of Gaussian primes. See 'primes'.
orderedPrimes :: [GaussianInteger]
orderedPrimes = (1 :+ 1): mergeBy (comparing norm) l r
  where (leftPrimes, rightPrimes) = partition (\p -> p `mod` 4 == 3) (tail Sieve.primes)
        l = [p :+ 0 | p <- leftPrimes]
        r = [g | p <- rightPrimes, let x :+ y = findPrime p, g <- [x :+ y, y :+ x]]

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy cmp = loop
  where
    loop [] ys  = ys
    loop xs []  = xs
    loop (x:xs) (y:ys)
      = case cmp x y of
         GT -> y : loop (x:xs) ys
         _  -> x : loop xs (y:ys)

-- | Compute the GCD of two Gaussian integers. Result is always
-- in the first quadrant.
gcdG :: GaussianInteger -> GaussianInteger -> GaussianInteger
gcdG g h = gcdG' (abs g) (abs h)

gcdG' :: GaussianInteger -> GaussianInteger -> GaussianInteger
gcdG' g h
    | h == 0    = g -- done recursing
    | otherwise = gcdG' h (abs (g `modG` h))
{-# DEPRECATED gcdG' "Use 'gcdG' instead." #-}

-- |Find a Gaussian integer whose norm is the given prime number
-- of form 4k + 1 using
-- <http://www.ams.org/journals/mcom/1972-26-120/S0025-5718-1972-0314745-6/S0025-5718-1972-0314745-6.pdf Hermite-Serret algorithm>.
findPrime :: Integer -> GaussianInteger
findPrime p = case Moduli.sqrtModMaybe (-1) (FieldCharacteristic (PrimeNat . integerToNatural $ p) 1) of
    Nothing -> error "findPrime: an argument must be prime p = 4k + 1"
    Just z  -> go p z -- Effectively we calculate gcdG' (p :+ 0) (z :+ 1)
    where
        sqrtp :: Integer
        sqrtp = integerSquareRoot p

        go :: Integer -> Integer -> GaussianInteger
        go g h
            | g <= sqrtp = g :+ h
            | otherwise  = go h (g `mod` h)

findPrime' :: Integer -> GaussianInteger
findPrime' = findPrime
{-# DEPRECATED findPrime' "Use 'findPrime' instead." #-}

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
        go z (2, e) = (divideByTwo z, [(1 :+ 1, e)])
        go z (p, e)
            | p `mod` 4 == 3
            = let e' = e `quot` 2 in (z `quotI` (p ^ e'), [(p :+ 0, e')])
            | otherwise
            = (z', filter ((> 0) . snd) [(gp, k), (gp', k')])
                where
                    gp = findPrime p
                    (k, k', z') = divideByPrime gp p e z
                    gp' = abs (conjugate gp)

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
    :: GaussianInteger   -- ^ Gaussian prime p
    -> Integer           -- ^ Precomputed norm of p, of form 4k + 1
    -> Int               -- ^ Expected number of factors (either p or conj p)
                         --   in Gaussian integer z
    -> GaussianInteger   -- ^ Gaussian integer z
    -> ( Int             -- Multiplicity of factor p in z
       , Int             -- Multiplicity of factor conj p in z
       , GaussianInteger -- Remaining Gaussian integer
       )
divideByPrime p np k = go k 0
    where
        go :: Int -> Int -> GaussianInteger -> (Int, Int, GaussianInteger)
        go 0 d z = (d, d, z)
        go c d z
            | c >= 2
            , Just z' <- z `quotEvenI` np
            = go (c - 2) (d + 1) z'
        go c d z = (d + d1, d + d2, z'')
            where
                (d1, z') = go1 c 0 z
                d2 = c - d1
                z'' = head $ drop d2
                    $ iterate (\g -> fromMaybe err $ (g * p) `quotEvenI` np) z'

        go1 :: Int -> Int -> GaussianInteger -> (Int, GaussianInteger)
        go1 0 d z = (d, z)
        go1 c d z
            | Just z' <- (z * conjugate p) `quotEvenI` np
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
