-- |
-- Module:      Math.NumberTheory.GaussianIntegers
-- Copyright:   (c) 2016 Chris Fredrickson
-- Licence:     MIT
-- Maintainer:  Chris Fredrickson <chris.p.fredrickson@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- This module exports functions for manipulating Gaussian integers, including
-- computing their prime factorisations.
--

{-# LANGUAGE BangPatterns #-}

module Math.NumberTheory.GaussianIntegers (
    GaussianInteger((:+)),
    ι,
    real,
    imag,
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
    findPrime,
    findPrime',
    factorise,
) where

import qualified Math.NumberTheory.Moduli as Moduli
import qualified Math.NumberTheory.Powers as Powers
import qualified Math.NumberTheory.Primes.Factorisation as Factorisation
import qualified Math.NumberTheory.Primes.Sieve as Sieve
import qualified Math.NumberTheory.Primes.Testing as Testing

infix 6 :+
infixr 8 .^
-- |A Gaussian integer is a+bi, where a and b are both integers.
data GaussianInteger = (:+) { real :: !Integer, imag :: !Integer } deriving (Eq)

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
-- Checks the precondition that p is prime and that p `mod` 4 /= 3.
findPrime :: Integer -> GaussianInteger
findPrime p
    | p == 2 || (p `mod` 4 == 1 && Testing.isPrime p) = findPrime' p
    | otherwise = error "p must be prime, and not congruent to 3 (mod 4)"

-- |Find a Gaussian integer whose norm is the given prime number. Does not
-- check the precondition.
findPrime' :: Integer -> GaussianInteger
findPrime' p =
    let (Just c) = Moduli.sqrtModP (-1) p
        k  = Powers.integerSquareRoot p
        bs = [1 .. k]
        asbs = map (\b' -> ((b' * c) `mod` p, b')) bs
        (a, b) = head [ (a', b') | (a', b') <- asbs, a' <= k]
    in a :+ b

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

-- |Compute the prime factorisation of a Gaussian integer. This is unique up to units (+/- 1, +/- i).
factorise :: GaussianInteger -> [(GaussianInteger, Int)]
factorise g
    | g == 0    = error "0 has no prime factorisation"
    | g == 1    = []
    | otherwise =
        let helper :: [(Integer, Int)] -> GaussianInteger -> [(GaussianInteger, Int)] -> [(GaussianInteger, Int)]
            helper [] g' fs = (if g' == 1 then [] else [(g', 1)]) ++ fs    -- include the unit, if it isn't 1
            helper ((!p, !e) : pt) g' fs
                | p `mod` 4 == 3 =
                    -- prime factors congruent to 3 mod 4 are simple.
                    let pow = div e 2
                        gp = fromInteger p
                    in helper pt (g' `divG` (gp .^ pow)) ((gp, pow) : fs)
                | otherwise      =
                    -- general case: for every prime factor of the magnitude
                    -- squared, find a Gaussian prime whose magnitude squared
                    -- is that prime. Then find out how many times the original
                    -- number is divisible by that Gaussian prime and its
                    -- conjugate. The order that the prime factors are
                    -- processed doesn't really matter, but it is reversed so
                    -- that the Gaussian primes will be in order of increasing
                    -- magnitude.
                    let gp = findPrime' p
                        (!gNext, !facs) = trialDivide g' [gp, abs $ conjugate gp] []
                    in helper pt gNext (facs ++ fs)
        in helper (reverse . Factorisation.factorise $ norm g) g []

-- Divide a Gaussian integer by a set of (relatively prime) Gaussian integers,
-- as many times as possible, and return the final quotient as well as a count
-- of how many times each factor divided the original.
trialDivide :: GaussianInteger -> [GaussianInteger] -> [(GaussianInteger, Int)] -> (GaussianInteger, [(GaussianInteger, Int)])
trialDivide g [] fs = (g, fs)
trialDivide g (pf : pft) fs
    | g `modG` pf == 0 =
        let (cnt, g') = countEvenDivisions g pf
        in trialDivide g' pft ((pf, cnt) : fs)
    | otherwise    = trialDivide g pft fs

-- Divide a Gaussian integer by a possible factor, and return how many times
-- the factor divided it evenly, as well as the result of dividing the original
-- that many times.
countEvenDivisions :: GaussianInteger -> GaussianInteger -> (Int, GaussianInteger)
countEvenDivisions g pf = helper g 0
    where
    helper :: GaussianInteger -> Int -> (Int, GaussianInteger)
    helper g' acc
        | g' `modG` pf == 0 = helper (g' `divG` pf) (1 + acc)
        | otherwise     = (acc, g')
