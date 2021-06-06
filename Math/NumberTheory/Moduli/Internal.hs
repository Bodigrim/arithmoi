-- |
-- Module:      Math.NumberTheory.Moduli.Internal
-- Copyright:   (c) 2020 Bhavik Mehta
-- Licence:     MIT
-- Maintainer:  Bhavik Mehta <bhavikmehta8@gmail.com>
--
-- Multiplicative groups of integers modulo m.
--

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.Moduli.Internal
  ( isPrimitiveRoot'
  , discreteLogarithmPP
  ) where

import qualified Data.Map as M
import Data.Maybe
import Data.Mod
import Data.Proxy
import GHC.TypeNats (SomeNat(..), someNatVal)
import GHC.Integer.GMP.Internals
import Numeric.Natural

import Math.NumberTheory.Moduli.Chinese
import Math.NumberTheory.Moduli.Equations
import Math.NumberTheory.Moduli.Singleton
import Math.NumberTheory.Primes
import Math.NumberTheory.Roots
import Math.NumberTheory.Utils.FromIntegral

-- https://en.wikipedia.org/wiki/Primitive_root_modulo_n#Finding_primitive_roots
isPrimitiveRoot'
  :: (Integral a, UniqueFactorisation a)
  => CyclicGroup a m
  -> a
  -> Bool
isPrimitiveRoot' cg r =
  case cg of
    CG2                       -> r == 1
    CG4                       -> r == 3
    CGOddPrimePower p k       -> oddPrimePowerTest (unPrime p) k r
    CGDoubleOddPrimePower p k -> doubleOddPrimePowerTest (unPrime p) k r
  where
    oddPrimePowerTest p 1 g       = oddPrimeTest p (g `mod` p)
    oddPrimePowerTest p _ g       = oddPrimeTest p (g `mod` p) && case someNatVal (fromIntegral' (p * p)) of
      SomeNat (_ :: Proxy pp) -> fromIntegral g ^ (p - 1) /= (1 :: Mod pp)

    doubleOddPrimePowerTest p k g = odd g && oddPrimePowerTest p k g

    oddPrimeTest p g = g /= 0 && gcd g p == 1 && case someNatVal (fromIntegral' p) of
      SomeNat (_ :: Proxy p) -> all (\x -> fromIntegral g ^ x /= (1 :: Mod p)) pows
      where
        pows = map (\(q, _) -> (p - 1) `quot` unPrime q) (factorise (p - 1))

-- Implementation of Bach reduction (https://www2.eecs.berkeley.edu/Pubs/TechRpts/1984/CSD-84-186.pdf)
{-# INLINE discreteLogarithmPP #-}
discreteLogarithmPP :: Integer -> Word -> Integer -> Integer -> Natural
discreteLogarithmPP p 1 a b = discreteLogarithmPrime p a b
discreteLogarithmPP p k a b = fromInteger $ if result < 0 then result + pkMinusPk1 else result
  where
    baseSol    = toInteger $ discreteLogarithmPrime p (a `rem` p) (b `rem` p)
    thetaA     = theta p pkMinusOne a
    thetaB     = theta p pkMinusOne b
    pkMinusOne = p^(k-1)
    c          = (recipModInteger thetaA pkMinusOne * thetaB) `rem` pkMinusOne
    (result, pkMinusPk1) = fromJust $ chinese (baseSol, p-1) (c, pkMinusOne)

-- compute the homomorphism theta given in https://math.stackexchange.com/a/1864495/418148
{-# INLINE theta #-}
theta :: Integer -> Integer -> Integer -> Integer
theta p pkMinusOne a = (numerator `quot` pk) `rem` pkMinusOne
  where
    pk           = pkMinusOne * p
    p2kMinusOne  = pkMinusOne * pk
    numerator    = (powModInteger a (pk - pkMinusOne) p2kMinusOne - 1) `rem` p2kMinusOne

-- TODO: Use Pollig-Hellman to reduce the problem further into groups of prime order.
-- While Bach reduction simplifies the problem into groups of the form (Z/pZ)*, these
-- have non-prime order, and the Pollig-Hellman algorithm can reduce the problem into
-- smaller groups of prime order.
-- In addition, the gcd check before solveLinear is applied in Pollard below will be
-- made redundant, since n would be prime.
discreteLogarithmPrime :: Integer -> Integer -> Integer -> Natural
discreteLogarithmPrime p a b
  | p < 100000000 = intToNatural $ discreteLogarithmPrimeBSGS (fromInteger p) (fromInteger a) (fromInteger b)
  | otherwise     = discreteLogarithmPrimePollard p a b

discreteLogarithmPrimeBSGS :: Int -> Int -> Int -> Int
discreteLogarithmPrimeBSGS p a b = head [i*m + j | (v,i) <- zip giants [0..m-1], j <- maybeToList (M.lookup v table)]
  where
    m        = integerSquareRoot (p - 2) + 1 -- simple way of ceiling (sqrt (p-1))
    babies   = iterate (.* a) 1
    table    = M.fromList (zip babies [0..m-1])
    aInv     = recipModInteger (toInteger a) (toInteger p)
    bigGiant = fromInteger $ powModInteger aInv (toInteger m) (toInteger p)
    giants   = iterate (.* bigGiant) b
    x .* y   = x * y `rem` p

-- TODO: Use more advanced walks, in order to reduce divisions, cf
-- https://maths-people.anu.edu.au/~brent/pd/rpb231.pdf
-- This will slightly improve the expected time to collision, and can reduce the
-- number of divisions performed.
discreteLogarithmPrimePollard :: Integer -> Integer -> Integer -> Natural
discreteLogarithmPrimePollard p a b =
  case concatMap runPollard [(x,y) | x <- [0..n], y <- [0..n]] of
    (t:_)  -> fromInteger t
    []     -> error ("discreteLogarithm: pollard's rho failed, please report this as a bug. inputs " ++ show [p,a,b])
  where
    n                 = p-1 -- order of the cyclic group
    halfN             = n `quot` 2
    mul2 m            = if m < halfN then m * 2 else m * 2 - n
    sqrtN             = integerSquareRoot n
    step (xi,!ai,!bi) = case xi `rem` 3 of
                          0 -> (xi*xi `rem` p, mul2 ai, mul2 bi)
                          1 -> ( a*xi `rem` p,    ai+1,      bi)
                          _ -> ( b*xi `rem` p,      ai,    bi+1)
    initialise (x,y)  = (powModInteger a x n * powModInteger b y n `rem` n, x, y)
    begin t           = go (step t) (step (step t))
    check t           = powModInteger a t p == b
    go tort@(xi,ai,bi) hare@(x2i,a2i,b2i)
      | xi == x2i, gcd (bi - b2i) n < sqrtN = case someNatVal (fromInteger n) of
        SomeNat (Proxy :: Proxy n) -> map (toInteger . unMod) $ solveLinear (fromInteger (bi - b2i) :: Mod n) (fromInteger (ai - a2i))
      | xi == x2i                           = []
      | otherwise                           = go (step tort) (step (step hare))
    runPollard        = filter check . begin . initialise
