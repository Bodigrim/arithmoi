-- |
-- Module:      Math.NumberTheory.Moduli.Internal
-- Copyright:   (c) 2020 Bhavik Mehta
-- Licence:     MIT
-- Maintainer:  Bhavik Mehta <bhavikmehta8@gmail.com>
--
-- Multiplicative groups of integers modulo m.
--

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Math.NumberTheory.Moduli.Internal
  ( isPrimitiveRoot'
  , discreteLogarithmPP
  ) where

import Data.Constraint
import Data.Constraint.Nat hiding (Mod)
import qualified Data.Map as M
import Data.Maybe
import Data.Mod
import qualified Data.Mod.Word as Word
import Data.Proxy
import GHC.Integer.GMP.Internals
import GHC.TypeNats.Compat
import Numeric.Natural

import Math.NumberTheory.ArithmeticFunctions
import Math.NumberTheory.Moduli.Chinese
import Math.NumberTheory.Moduli.Equations
import Math.NumberTheory.Moduli.Singleton
import Math.NumberTheory.Primes
import Math.NumberTheory.Powers.Modular
import Math.NumberTheory.Roots

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
    oddPrimeTest p g              = let phi  = totient p
                                        pows = map (\pk -> phi `quot` unPrime (fst pk)) (factorise phi)
                                        exps = map (\x -> powMod g x p) pows
                                     in g /= 0 && gcd g p == 1 && notElem 1 exps
    oddPrimePowerTest p 1 g       = oddPrimeTest p (g `mod` p)
    oddPrimePowerTest p _ g       = oddPrimeTest p (g `mod` p) && powMod g (p-1) (p*p) /= 1
    doubleOddPrimePowerTest p k g = odd g && oddPrimePowerTest p k g

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
    pkMinusPk1 = pkMinusOne * (p - 1)
    c          = (recipModInteger thetaA pkMinusOne * thetaB) `rem` pkMinusOne
    result     = fromJust $ chineseCoprime (baseSol, p-1) (c, pkMinusOne)

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
  | p < 100000000 = case someNatVal (fromInteger p) of
    SomeNat (_ :: Proxy p) -> fromIntegral $ discreteLogarithmPrimeBSGS @p (fromInteger a) (fromInteger b)
  | otherwise     = case someNatVal (fromInteger p - 1) of
    SomeNat (_ :: Proxy p_1) -> case plusNat @p_1 @1 of
      Sub Dict -> unMod $ discreteLogarithmPrimePollard @p_1 (fromInteger a) (fromInteger b)

discreteLogarithmPrimeBSGS
  :: KnownNat p
  => Word.Mod p
  -> Word.Mod p
  -> Word
discreteLogarithmPrimeBSGS a b = head [i * m + j | (v, i) <- zip giants [0 .. m - 1], j <- maybeToList (M.lookup v table)]
  where
    p        = fromIntegral (natVal a)
    m        = integerSquareRoot (p - 2) + 1 -- simple way of ceiling (sqrt (p-1))
    babies   = iterate (* a) 1
    table    = M.fromList (zip babies [0 .. m - 1])
    bigGiant = recip a Word.^% m
    giants   = iterate (* bigGiant) b

type Triple p_1 = (Mod (p_1 + 1), Mod p_1, Mod p_1)

-- TODO: Use more advanced walks, in order to reduce divisions, cf
-- https://maths-people.anu.edu.au/~brent/pd/rpb231.pdf
-- This will slightly improve the expected time to collision, and can reduce the
-- number of divisions performed.
discreteLogarithmPrimePollard
  :: forall p_1.
     (KnownNat p_1, KnownNat (p_1 + 1))
  => Mod (p_1 + 1)
  -> Mod (p_1 + 1)
  -> Mod p_1
discreteLogarithmPrimePollard a b =
  case concatMap runPollard [(x,y) | x <- [minBound..maxBound], y <- [minBound..maxBound]] of
    (t:_)  -> t
    []     -> error ("discreteLogarithm: pollard's rho failed, please report this as a bug. inputs " ++ show (n, a, b))
  where
    n     = natVal (Proxy :: Proxy p_1) -- order of the cyclic group
    sqrtN = integerSquareRoot n

    step :: Triple p_1 -> Triple p_1
    step (xi, !ai, !bi) = case unMod xi `rem` 3 of
      0 -> (xi * xi, ai + ai, bi + bi)
      1 -> ( a * xi,  1 + ai,      bi)
      _ -> ( b * xi,      ai,  1 + bi)

    initialise (x, y) = (a ^% unMod x * b ^% unMod y, x, y)

    begin t = go (step t) (step (step t))

    go :: Triple p_1 -> Triple p_1 -> [Mod p_1]
    go tort@(xi, ai, bi) hare@(x2i, a2i, b2i)
      | xi == x2i, gcd (unMod (bi - b2i)) n < sqrtN
      = solveLinear (bi - b2i) (ai - a2i)
      | xi == x2i
      = []
      | otherwise
      = go (step tort) (step (step hare))

    runPollard = filter (\t -> a ^% unMod t == b) . begin . initialise
