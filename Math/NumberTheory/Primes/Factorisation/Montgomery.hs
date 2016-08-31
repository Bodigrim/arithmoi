-- |
-- Module:      Math.NumberTheory.Primes.Factorisation.Montgomery
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Factorisation of 'Integer's by the elliptic curve algorithm after Montgomery.
-- The algorithm is explained at
-- <http://programmingpraxis.com/2010/04/23/modern-elliptic-curve-factorization-part-1/>
-- and
-- <http://programmingpraxis.com/2010/04/27/modern-elliptic-curve-factorization-part-2/>
--
-- The implementation is not very optimised, so it is not suitable for factorising numbers
-- with only huge prime divisors. However, factors of 20-25 digits are normally found in
-- acceptable time. The time taken depends, however, strongly on how lucky the curve-picking
-- is. With luck, even large factors can be found in seconds; on the other hand, finding small
-- factors (about 10 digits) can take minutes when the curve-picking is bad.
--
-- Given enough time, the algorithm should be able to factor numbers of 100-120 digits, but it
-- is best suited for numbers of up to 50-60 digits.
{-# LANGUAGE CPP, BangPatterns, MagicHash #-}
{-# OPTIONS_HADDOCK hide #-}
module Math.NumberTheory.Primes.Factorisation.Montgomery
  ( -- *  Complete factorisation functions
    -- ** Functions with input checking
    factorise
  , defaultStdGenFactorisation
    -- ** Functions without input checking
  , factorise'
  , stepFactorisation
  , defaultStdGenFactorisation'
    -- * Partial factorisation
  , smallFactors
  , stdGenFactorisation
  , curveFactorisation
    -- ** Single curve worker
  , montgomeryFactorisation
  , findParms
  ) where

#include "MachDeps.h"

import GHC.Base

import System.Random
import Control.Monad.State.Strict
#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Data.Bits
import Data.Maybe

import GHC.Integer.Logarithms

import Math.NumberTheory.Logarithms
import Math.NumberTheory.Powers.General     (highestPower, largePFPower)
import Math.NumberTheory.Powers.Squares     (integerSquareRoot')
import Math.NumberTheory.Primes.Sieve.Eratosthenes
import Math.NumberTheory.Primes.Sieve.Indexing
import Math.NumberTheory.Primes.Testing.Probabilistic
import Math.NumberTheory.Unsafe
import Math.NumberTheory.Utils

-- | @'factorise' n@ produces the prime factorisation of @n@, including
--   a factor of @(-1)@ if @n < 0@. @'factorise' 0@ is an error and the
--   factorisation of @1@ is empty. Uses a 'StdGen' produced in an arbitrary
--   manner from the bit-pattern of @n@.
factorise :: Integer -> [(Integer,Int)]
factorise n
    | n < 0     = (-1,1):factorise (-n)
    | n == 0    = error "0 has no prime factorisation"
    | n == 1    = []
    | otherwise = factorise' n

-- | Like 'factorise', but without input checking, hence @n > 1@ is required.
factorise' :: Integer -> [(Integer,Int)]
factorise' n = defaultStdGenFactorisation' (mkStdGen $ fromInteger n `xor` 0xdeadbeef) n

-- | @'stepFactorisation'@ is like 'factorise'', except that it doesn't use a
--   pseudo random generator but steps through the curves in order.
--   This strategy turns out to be surprisingly fast, on average it doesn't
--   seem to be slower than the 'StdGen' based variant.
stepFactorisation :: Integer -> [(Integer,Int)]
stepFactorisation n
    = let (sfs,mb) = smallFactors 100000 n
      in sfs ++ case mb of
                  Nothing -> []
                  Just r  -> curveFactorisation (Just 10000000000) bailliePSW
                                                (\m k -> (if k < (m-1) then k else error "Curves exhausted",k+1)) 6 Nothing r

-- | @'defaultStdGenFactorisation'@ first strips off all small prime factors and then,
--   if the factorisation is not complete, proceeds to curve factorisation.
--   For negative numbers, a factor of @-1@ is included, the factorisation of @1@
--   is empty. Since @0@ has no prime factorisation, a zero argument causes
--   an error.
defaultStdGenFactorisation :: StdGen -> Integer -> [(Integer,Int)]
defaultStdGenFactorisation sg n
    | n == 0    = error "0 has no prime factorisation"
    | n < 0     = (-1,1) : defaultStdGenFactorisation sg (-n)
    | n == 1    = []
    | otherwise = defaultStdGenFactorisation' sg n

-- | Like 'defaultStdGenFactorisation', but without input checking, so
--   @n@ must be larger than @1@.
defaultStdGenFactorisation' :: StdGen -> Integer -> [(Integer,Int)]
defaultStdGenFactorisation' sg n
    = let (sfs,mb) = smallFactors 100000 n
      in sfs ++ case mb of
                  Nothing -> []
                  Just m  -> stdGenFactorisation (Just 10000000000) sg Nothing m

----------------------------------------------------------------------------------------------------
--                                    Factorisation wrappers                                      --
----------------------------------------------------------------------------------------------------

-- | A wrapper around 'curveFactorisation' providing a few default arguments.
--   The primality test is 'bailliePSW', the @prng@ function - naturally -
--   'randomR'. This function also requires small prime factors to have been
--   stripped before.
stdGenFactorisation :: Maybe Integer    -- ^ Lower bound for composite divisors
                    -> StdGen           -- ^ Standard PRNG
                    -> Maybe Int        -- ^ Estimated number of digits of smallest prime factor
                    -> Integer          -- ^ The number to factorise
                    -> [(Integer,Int)]  -- ^ List of prime factors and exponents
stdGenFactorisation primeBound sg digits n
    = curveFactorisation primeBound bailliePSW (\m -> randomR (6,m-2)) sg digits n

-- | @'curveFactorisation'@ is the driver for the factorisation. Its performance (and success)
--   can be influenced by passing appropriate arguments. If you know that @n@ has no prime divisors
--   below @b@, any divisor found less than @b*b@ must be prime, thus giving @Just (b*b)@ as the
--   first argument allows skipping the comparatively expensive primality test for those.
--   If @n@ is such that all prime divisors must have a specific easy to test for structure, a
--   custom primality test can improve the performance (normally, it will make very little
--   difference, since @n@ has not many divisors, and many curves have to be tried to find one).
--   More influence has the pseudo random generator (a function @prng@ with @6 <= fst (prng k s) <= k-2@
--   and an initial state for the PRNG) used to generate the curves to try. A lucky choice here can
--   make a huge difference. So, if the default takes too long, try another one; or you can improve your
--   chances for a quick result by running several instances in parallel.
--
--   @'curveFactorisation'@ requires that small prime factors have been stripped before. Also, it is
--   unlikely to succeed if @n@ has more than one (really) large prime factor.
curveFactorisation :: Maybe Integer                 -- ^ Lower bound for composite divisors
                   -> (Integer -> Bool)             -- ^ A primality test
                   -> (Integer -> g -> (Integer,g)) -- ^ A PRNG
                   -> g                             -- ^ Initial PRNG state
                   -> Maybe Int                     -- ^ Estimated number of digits of the smallest prime factor
                   -> Integer                       -- ^ The number to factorise
                   -> [(Integer,Int)]               -- ^ List of prime factors and exponents
curveFactorisation primeBound primeTest prng seed mbdigs n
    | ptest n   = [(n,1)]
    | otherwise = evalState (fact n digits) seed
      where
        digits = fromMaybe 8 mbdigs
        mult 1 xs = xs
        mult j xs = [(p,j*k) | (p,k) <- xs]
        dbl (u,v) = (mult 2 u, mult 2 v)
        ptest = case primeBound of
                  Just bd -> \k -> k <= bd || primeTest k
                  Nothing -> primeTest
        rndR k = state (\gen -> prng k gen)
        perfPw = case primeBound of
                   Nothing -> highestPower
                   Just bd -> largePFPower (integerSquareRoot' bd)
        fact m digs = do let (b1,b2,ct) = findParms digs
                         (pfs,cfs) <- repFact m b1 b2 ct
                         if null cfs
                           then return pfs
                           else do
                               nfs <- forM cfs $ \(k,j) ->
                                   mult j <$> fact k (if null pfs then digs+4 else digs)
                               return (mergeAll $ pfs:nfs)
        repFact m b1 b2 count = case perfPw m of
                                  (_,1) -> workFact m b1 b2 count
                                  (b,e)
                                    | ptest b -> return ([(b,e)],[])
                                    | otherwise -> do
                                      (as,bs) <- workFact b b1 b2 count
                                      return $ (mult e as, mult e bs)
        workFact m b1 b2 count
            | count < 0 = return ([],[(m,1)])
            | otherwise = do
                s <- rndR m
                case montgomeryFactorisation m b1 b2 s of
                  Nothing -> workFact m b1 b2 (count-1)
                  Just d  -> do
                      let !cof = m `quot` d
                      case gcd cof d of
                        1 -> do
                            (dp,dc) <- if ptest d
                                         then return ([(d,1)],[])
                                         else repFact d b1 b2 (count-1)
                            (cp,cc) <- if ptest cof
                                         then return ([(cof,1)],[])
                                         else repFact cof b1 b2 (count-1)
                            return (merge dp cp, dc ++ cc)
                        g -> do
                            let d' = d `quot` g
                                c' = cof `quot` g
                            (dp,dc) <- if ptest d'
                                         then return ([(d',1)],[])
                                         else repFact d' b1 b2 (count-1)
                            (cp,cc) <- if ptest c'
                                         then return ([(c',1)],[])
                                         else repFact c' b1 b2 (count-1)
                            (gp,gc) <- if ptest g
                                         then return ([(g,2)],[])
                                         else dbl <$> repFact g b1 b2 (count-1)
                            return  (mergeAll [dp,cp,gp], dc ++ cc ++ gc)

----------------------------------------------------------------------------------------------------
--                                         The workhorse                                          --
----------------------------------------------------------------------------------------------------

-- | @'montgomeryFactorisation' n b1 b2 s@ tries to find a factor of @n@ using the
--   curve and point determined by the seed @s@ (@6 <= s < n-1@), multiplying the
--   point by the least common multiple of all numbers @<= b1@ and all primes
--   between @b1@ and @b2@. The idea is that there's a good chance that the order
--   of the point in the curve over one prime factor divides the multiplier, but the
--   order over another factor doesn't, if @b1@ and @b2@ are appropriately chosen.
--   If they are too small, none of the orders will probably divide the multiplier,
--   if they are too large, all probably will, so they should be chosen to fit
--   the expected size of the smallest factor.
--
--   It is assumed that @n@ has no small prime factors.
--
--   The result is maybe a nontrivial divisor of @n@.
montgomeryFactorisation :: Integer -> Word -> Word -> Integer -> Maybe Integer
montgomeryFactorisation n b1 b2 s = go p5 (list primeStore)
  where
    l2 = wordLog2' b1
    b1i = toInteger b1
    (^~) :: Word -> Int -> Word
    w ^~ i = w ^ i
    (e, p0) = montgomeryData n s
    dbl pt = double n e pt
    dbln 0 !pt = pt
    dbln k pt = dbln (k-1) (dbl pt)
    p2 = dbln l2 p0
#if WORD_SIZE_IN_BITS == 64
    mul a b c = (a*b) `quot` c       -- can't overflow, work on Int
#else
    mul a b c = fromInteger ((toInteger a * b) `quot` c) -- might overflow if Int is used
#endif
    adjust bd ml w
      | w <= bd     = adjust bd ml (w*ml)
      | otherwise   = w
    l3 = mul l2 190537 301994
    w3 = 3 ^~ l3
    pw3 = adjust (b1 `quot` 3) 3 w3
    p3 = multiply n e pw3 p2
    l5 = mul l2 1936274 4495889
    w5 = 5 ^~ l5
    pw5 = adjust (b1 `quot` 5) 5 w5
    p5 = multiply n e pw5 p3
    go (P _ 0) _ = Nothing
    go !pt@(P _ z) (pr:prs)
      | pr <= b1    = let !lp = integerLogBase' (fromIntegral pr) b1i
                      in go (multiply n e (pr ^~ lp) pt) prs
      | otherwise   = case gcd n z of
                        1 -> lgo (multiply n e pr pt) prs
                        g -> Just g
    go (P _ z) _    = case gcd n z of
                        1 -> Nothing
                        g -> Just g
    lgo (P _ 0) _ = Nothing
    lgo !pt@(P _ z) (pr:prs)
      | pr <= b2    = lgo (multiply n e pr pt) prs
      | otherwise   = case gcd n z of
                        1 -> Nothing
                        g -> Just g
    lgo (P _ z) _   = case gcd n z of
                        1 -> Nothing
                        g -> Just g

----------------------------------------------------------------------------------------------------
--                            Helpers, Curves and elliptic arithmetics                            --
----------------------------------------------------------------------------------------------------

-- A Montgomery curve is given by y^2 = x^3 + (A_n / A_d - 2)*x^2 + x (mod n).
-- We store A_n and 4*A_d, since A_n occurs with the factor 4 in all formulae.
data Curve = C !Integer !Integer

-- Point in the projective plane, will be on the curve
-- A coordinate transformation eliminates the y-coordinate, hence
-- we store only x and z
data Point = P !Integer !Integer

-- Get curve and point to start
-- Input should satisfy 6 <= s < n-1
montgomeryData :: Integer -> Integer -> (Curve, Point)
montgomeryData n s = (C an ad4, P x z)
  where
    u = (s*s-5) `mod` n
    v = (4*s) `mod` n
    d = (v-u)
    x = (u*u*u) `mod` n
    z = (v*v*v) `mod` n
    an = ((d*d)*(d*(3*u+v))) `mod` n
    ad4 = (16*x*v) `mod` n

-- Addition on the curve, given the modulus n and three points,
-- p0, p1 and p2, with p0 = p2 - p1, calculate the point p1 + p2.
-- Note that the addition does not depend on the curve.
add :: Integer -> Point -> Point -> Point -> Point
add n (P x0 z0) (P x1 z1) (P x2 z2) = P x3 z3
  where
    a = (x1-z1)*(x2+z2)
    b = (x1+z1)*(x2-z2)
    c = a+b
    d = a-b
    x3 = (c*c*z0) `rem` n
    z3 = (d*d*x0) `rem` n

-- Double a point on the curve.
double :: Integer -> Curve -> Point -> Point
double n (C an ad4) (P x z) = P x' z'
  where
    r = x+z
    s = x-z
    u = r*r
    v = s*s
    t = u-v
    x' = (ad4*u*v) `rem` n
    z' = ((ad4*v+t*an)*t) `rem` n

-- Multiply a point on the curve by a Word.
-- Within Word range, we can use the faster variant going
-- from high-order bits to low-order.
multiply :: Integer -> Curve -> Word -> Point -> Point
multiply n cve (W# w##) p =
    case wordLog2# w## of
      l# -> go (l# -# 1#) p (double n cve p)
  where
    go 0# !p0 !p1 = case w## `and#` 1## of
                      0## -> double n cve p0
                      _   -> add n p p0 p1
    go i# p0 p1 = case (uncheckedShiftRL# w## i#) `and#` 1## of
                    0## -> go (i# -# 1#) (double n cve p0) (add n p p0 p1)
                    _   -> go (i# -# 1#) (add n p p0 p1) (double n cve p1)

{-  Not (yet) needed
-- Multiply a point on the curve by an Integer.
multIgr :: Integer -> Curve -> Integer -> Point -> Point
multIgr n cve k p = go k
  where
    go 1 = (p, double n cve p)
    go m = case m `quotRem` 2 of
             (q,r) -> let !(!s, l) = go q
                      in case r of
                           0 -> (double n cve s, add n p s l)
                           _ -> (add n p s l, double n cve l)
-}

-- primes, compactly stored as a bit sieve
primeStore :: [PrimeSieve]
primeStore = psieveFrom 7

-- generate list of primes from arrays
list :: [PrimeSieve] -> [Word]
list sieves = concat [[off + toPrim i | i <- [0 .. li], unsafeAt bs i]
                                | PS vO bs <- sieves, let { (_,li) = bounds bs; off = fromInteger vO; }]

-- | @'smallFactors' bound n@ finds all prime divisors of @n > 1@ up to @bound@ by trial division and returns the
--   list of these together with their multiplicities, and a possible remaining factor which may be composite.
smallFactors :: Integer -> Integer -> ([(Integer,Int)], Maybe Integer)
smallFactors bd n = case shiftToOddCount n of
                      (0,m) -> go m prms
                      (k,m) -> (2,k) <: if m == 1 then ([],Nothing) else go m prms
  where
    prms = tail (primeStore >>= primeList)
    x <: ~(l,b) = (x:l,b)
    go m (p:ps)
        | m < p*p   = ([(m,1)], Nothing)
        | bd < p    = ([], Just m)
        | otherwise = case splitOff p m of
                        (0,_) -> go m ps
                        (k,r) | r == 1 -> ([(p,k)], Nothing)
                              | otherwise -> (p,k) <: go r ps
    go m [] = ([(m,1)], Nothing)

-- helpers: merge sorted lists
merge :: [(Integer,Int)] -> [(Integer,Int)] -> [(Integer,Int)]
merge xxs@(x@(p,k):xs) yys@(y@(q,m):ys) = case compare p q of
                                            LT -> x : merge xs yys
                                            EQ -> (p,k+m) : merge xs ys
                                            GT -> y : merge xxs ys
merge xs [] = xs
merge _ ys = ys

mergeAll :: [[(Integer,Int)]] -> [(Integer,Int)]
mergeAll [] = []
mergeAll [xs] = xs
mergeAll (xs:ys:zss) = merge (merge xs ys) (mergeAll zss)

-- Parameters for the factorisation, the two b-parameters for montgomery and the number of tries
-- to use these, depending on the size of the factor we are looking for.
-- The numbers are roughly based on the parameters listed on Dario Alpern's ECM site.
testParms :: [(Int,Word,Word,Int)]
testParms = [ (12, 400, 10000, 10), (15, 2000, 50000, 25), (20, 11000, 150000, 90)
            , (25, 50000, 500000, 300), (30, 250000, 1500000, 700)
            , (35, 1000000, 4000000, 1800), (40, 3000000, 12000000, 5100)
            , (45, 11000000, 45000000, 10600), (50, 43000000, 200000000, 19300)
            , (55, 80000000, 400000000,30000), (60, 120000000, 700000000, 50000)
            ]

findParms :: Int -> (Word, Word, Int)
findParms digs = go (100, 1000, 7) testParms
  where
    go p ((d,b1,b2,ct):rest)
      | digs < d    = p
      | otherwise   = go (b1,b2,ct) rest
    go p [] = p
