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
    findPrime',
    factorise,
) where

import qualified Math.NumberTheory.Moduli as Moduli
import Math.NumberTheory.Moduli.Sqrt (FieldCharacteristic(..), unPrime, toFieldCharacteristic)
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
                    else let x :+ y = findPrime' (fromMaybeError "Impossible error" $ toFieldCharacteristic p) -- safe actually
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
findPrime' :: FieldCharacteristic -> GaussianInteger
findPrime' (FieldCharacteristic prime 1) =
    let (Just c) = Moduli.sqrtModMaybe (-1) (FieldCharacteristic prime 1)
        p = unPrime prime
        k  = Powers.integerSquareRoot p
        bs = [1 .. k]
        asbs = map (\b' -> ((b' * c) `mod` p, b')) bs
        (a, b) = head [ (a', b') | (a', b') <- asbs, a' <= k]
    in a :+ b
findPrime' (FieldCharacteristic _prime _pow) = error "Not a prime number as argument to findPrime'"

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
-- Unit factors are not included in the result.
factorise :: GaussianInteger -> [(GaussianInteger, Int)]
factorise g = helper (Factorisation.factorise $ norm g) g
    where
    helper [] _ = []
    helper ((!p, !e) : pt) g' =
        -- For a given prime factor p of the magnitude squared...
        let (!g'', !facs) = if p `mod` 4 == 3
            then
                -- if the p is congruent to 3 (mod 4), then g' is divisible by
                -- p^(e/2).
                let pow = div e 2
                    gp = fromInteger p
                in (g' `divG` (gp .^ pow), [(gp, pow)])
            else
                -- otherwise: find a Gaussian prime gp for which `norm gp ==
                -- p`. Then do trial divisions to find out how many times g' is
                -- divisible by gp or its conjugate.
                let gp = findPrime' (fromMaybeError "p must be prime" $ toFieldCharacteristic p)
                in trialDivide g' [gp, abs $ conjugate gp]
        in facs ++ helper pt g''

-- Divide a Gaussian integer by a set of (relatively prime) Gaussian integers,
-- as many times as possible, and return the final quotient as well as a count
-- of how many times each factor divided the original.
trialDivide :: GaussianInteger -> [GaussianInteger] -> (GaussianInteger, [(GaussianInteger, Int)])
trialDivide = helper []
    where
    helper fs g [] = (g, fs)
    helper fs g (pf : pft)
        | g `modG` pf == 0 =
            let (cnt, g') = countEvenDivisions g pf
            in helper ((pf, cnt) : fs) g' pft
        | otherwise        = helper fs g pft

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

-- Custom version of @'fromJust'@.
fromMaybeError :: String -> Maybe a -> a
fromMaybeError msg Nothing = error msg
fromMaybeError _ (Just v) = v
