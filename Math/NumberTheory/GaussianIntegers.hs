{-# LANGUAGE BangPatterns #-}
-- |Functions for computing with Gaussian Integers.
module Math.NumberTheory.GaussianIntegers (
    GaussianInteger((:+)),
    real,
    imag,
    conjugate,
    magnitude,
    (.+),
    (.-),
    (.*),
    (./),
    (.%),
    (.^),
    isPrime,
    primes,
    gcd,
    findPrime,
    factorise,
) where

import Prelude hiding (gcd)
import Data.List  (genericLength)
import Data.Ratio ((%))
import qualified Math.NumberTheory.GCD as GCD
import qualified Math.NumberTheory.Moduli as Moduli
import qualified Math.NumberTheory.Powers as Powers
import qualified Math.NumberTheory.Primes.Factorisation as Factorisation
import qualified Math.NumberTheory.Primes.Sieve as Sieve
import qualified Math.NumberTheory.Primes.Testing as Testing

infix 6 :+
infixr 8 .^
infixl 7 .+, .-, .*, ./
-- |A Gaussian integer is a+bi, where a and b are both integers.
data GaussianInteger = Integer :+ Integer deriving (Ord, Eq)

instance Show GaussianInteger where
    show (a :+ b) = show a ++ op ++ b' ++ "i"
        where op = if b >= 0 then "+" else "-"
              b' = if abs b /= 1 then show (abs b) else ""

-- |The real part of a Gaussian integer.
real :: GaussianInteger -> Integer
real (x :+ _) = x

-- |The imaginary part of a Gaussian integer.
imag :: GaussianInteger -> Integer
imag (_ :+ y) = y

-- |Conjugate a Gaussian integer.
conjugate :: GaussianInteger -> GaussianInteger
conjugate (r :+ i) = r :+ (-i)

-- |The square of the magnitude of a Gaussian integer.
magnitude :: GaussianInteger -> Integer
magnitude (x :+ y) = x * x + y * y

-- |Add two Gaussian integers together.
(.+) :: GaussianInteger -> GaussianInteger -> GaussianInteger
(gr :+ gi) .+ (hr :+ hi) = (gr + hr) :+ (gi + hi)

-- |Subtract one Gaussian integer from another.
(.-) :: GaussianInteger -> GaussianInteger -> GaussianInteger
(gr :+ gi) .- (hr :+ hi) = (gr - hr) :+ (gi - hi)

-- |Multiply two Gaussian integers.
(.*) :: GaussianInteger -> GaussianInteger -> GaussianInteger
(gr :+ gi) .* (hr :+ hi) = (gr * hr - hi * gi) :+ (gr * hi + gi * hr)

-- "div" truncates toward -infinity, "quot" truncates toward 0, but we need
-- something that truncates toward the nearest integer. I.e., we want to
-- truncate with "round".
divToNearest :: (Integral a, Integral b) => a -> a -> b
divToNearest x y = round ((x % 1) / (y % 1))

-- |Divide one Gaussian integer by another.
(./) :: GaussianInteger -> GaussianInteger -> GaussianInteger
g ./ h =
    let nr :+ ni = g .* conjugate h
        denom    = magnitude h
    in divToNearest nr denom :+ divToNearest ni denom

-- |Compute the remainder when dividing one Gaussian integer by another.
(.%) :: GaussianInteger -> GaussianInteger -> GaussianInteger
g .% m =
    let q = g ./ m
        p = m .* q
    in g .- p

-- |Compute whether a given Gaussian integer is prime.
isPrime :: GaussianInteger -> Bool
isPrime g@(x :+ y)
    | x == 0 && y /= 0 = abs y `mod` 4 == 3 && Testing.isPrime y
    | y == 0 && x /= 0 = abs x `mod` 4 == 3 && Testing.isPrime x
    | otherwise        = Testing.isPrime $ magnitude g

-- |An infinite list of the Gaussian primes. Uses primes in Z to exhaustively
-- generate all Gaussian primes, but not quite in order of ascending magnitude.
primes :: [GaussianInteger]
primes = [ a' :+ b'
         | p <- Sieve.primes
         , (a', b') <- if p `mod` 4 == 3
             then [ (f a', f b')
                  | a' <- [0, p]
                  , b' <- [0, p]
                  , a' /= b'
                  , f <- [id, negate]
                  ]
             else [ (a', b')
                  | let radius = Powers.integerSquareRoot p
                  , a <- [0 .. radius]
                  , let d = p - a * a
                  , Powers.isSquare d
                  , let b = Powers.integerSquareRoot d
                  , a' <- [a, -a]
                  , b' <- [b, -b]
                  ]
         ]

-- |Compute the GCD of two Gaussian integers.
gcd :: GaussianInteger -> GaussianInteger -> GaussianInteger
gcd g h
    | h == 0 :+ 0 = g --done recursing
    | otherwise = gcd h (g .% h)

-- |Compute the group of units of Zm.
units :: Integer -> [Integer]
units n = filter (GCD.coprime n) [1 .. n - 1]

-- |Compute the primitive roots of Zm.
roots :: Integer -> [Integer]
roots m
    | null us   = []
    | otherwise = [ u | u <- us, order u m == genericLength us]
    where us = units m

-- |Compute the order of x in Zm.
order :: Integer -> Integer -> Integer
order x m = head [ ord
                 | ord <- [1 .. genericLength $ units m]
                 , Moduli.powerMod x ord m == 1
                 ]

-- |Find a Gaussian integer whose magnitude squared is the given prime number.
findPrime :: Integer -> GaussianInteger
findPrime p
    | p == 2 = 1 :+ 1
    | p `mod` 4 == 1 && Testing.isPrime p =
        let r = head $ roots p
            z = Moduli.powerMod r (quot (p - 1) 4) p
        in gcd (p :+ 0) (z :+ 1)
    | otherwise = error "p must be prime, and congruent to 1 (mod 4)"

-- |Raise a Gaussian integer to a given power.
(.^) :: (Integral a) => GaussianInteger -> a -> GaussianInteger
a .^ e
    | e < 0     = error "Cannot exponentiate Gaussian Int to negative power"
    | e == 0    = 1 :+ 0
    | even e    = s .* s
    | otherwise = a .* a .^ (e - 1)
    where
    s = a .^ div e 2

-- |Compute the prime factorization of a Gaussian integer. This is unique up to units (+/- 1, +/- i).
factorise :: GaussianInteger -> [(GaussianInteger, Int)]
factorise g
    | g == 0 :+ 0  = [(g, 1)] -- 0 has no prime factors.
    | otherwise =
        let helper :: [(Integer, Int)] -> GaussianInteger -> [(GaussianInteger, Int)] -> [(GaussianInteger, Int)]
            helper [] g' fs = (g', 1) : fs    -- include the unit.
            helper ((!p, !e) : pt) g' fs
                | p `mod` 4 == 3 =
                    -- prime factors congruent to 3 mod 4 are simple.
                    let pow = div e 2
                        gp = p :+ 0
                    in helper pt (g' ./ gp .^ pow) ((gp, pow) : fs)
                | otherwise      =
                    -- general case: for every prime factor of the magnitude
                    -- squared, find a Gaussian prime whose magnitude squared
                    -- is that prime. Then find out how many times the original
                    -- number is divisible by that Gaussian prime and its
                    -- conjugate. The order that the prime factors are
                    -- processed doesn't really matter, but it is reversed so
                    -- that the Gaussian primes will be in order of increasing
                    -- magnitude.
                    let gp = findPrime p
                        (!gNext, !facs) = trialDivide g' [gp, conjugate gp] []
                    in helper pt gNext (facs ++ fs)
        in helper (reverse . Factorisation.factorise $ magnitude g) g []

-- Divide a Gaussian integer by a set of (relatively prime) Gaussian integers,
-- as many times as possible, and return the final quotient as well as a count
-- of how many times each factor divided the original.
trialDivide :: GaussianInteger -> [GaussianInteger] -> [(GaussianInteger, Int)] -> (GaussianInteger, [(GaussianInteger, Int)])
trialDivide g [] fs = (g, fs)
trialDivide g (pf : pft) fs
    | g .% pf == 0 :+ 0 =
        let (cnt, g') = countEvenDivisions g pf
        in trialDivide g' pft ((pf, cnt) : fs)
    | otherwise       = trialDivide g pft fs

-- Divide a Gaussian integer by a possible factor, and return how many times
-- the factor divided it evenly, as well as the result of dividing the original
-- that many times.
countEvenDivisions :: GaussianInteger -> GaussianInteger -> (Int, GaussianInteger)
countEvenDivisions g pf = helper g 0
    where
    helper :: GaussianInteger -> Int -> (Int, GaussianInteger)
    helper g' acc
        | g' .% pf == 0:+ 0 = helper (g' ./ pf) (1 + acc)
        | otherwise         = (acc, g')
