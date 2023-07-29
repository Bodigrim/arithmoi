-- |
-- Module:      Math.NumberTheory.Moduli.Sqrt
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Modular square roots and
-- <https://en.wikipedia.org/wiki/Jacobi_symbol Jacobi symbol>.
--

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Math.NumberTheory.Moduli.Sqrt
  ( -- * Modular square roots
    sqrtsMod
  , sqrtsModFactorisation
  , sqrtsModPrimePower
  , sqrtsModPrime
    -- * Jacobi symbol
  , JacobiSymbol(..)
  , jacobi
  , symbolToNum
  ) where

import Control.Monad (liftM2)
import Data.Bits
import Data.Constraint
import Data.List.Infinite (Infinite(..), (...))
import qualified Data.List.Infinite as Inf
import Data.Maybe
import Data.Mod
import Data.Proxy
import GHC.TypeNats (KnownNat, SomeNat(..), natVal, someNatVal, Nat)
import Numeric.Natural (Natural)

import Math.NumberTheory.Moduli.Chinese
import Math.NumberTheory.Moduli.JacobiSymbol
import Math.NumberTheory.Moduli.Singleton
import Math.NumberTheory.Primes
import Math.NumberTheory.Utils (shiftToOddCount, splitOff)
import Math.NumberTheory.Utils.FromIntegral

-- | List all modular square roots.
--
-- >>> :set -XDataKinds
-- >>> sqrtsMod sfactors (1 :: Mod 60)
-- [(1 `modulo` 60),(49 `modulo` 60),(41 `modulo` 60),(29 `modulo` 60),(31 `modulo` 60),(19 `modulo` 60),(11 `modulo` 60),(59 `modulo` 60)]
sqrtsMod :: SFactors Integer m -> Mod m -> [Mod m]
sqrtsMod sm a = case proofFromSFactors sm of
  Sub Dict -> map fromInteger $ sqrtsModFactorisation (toInteger (unMod a)) (unSFactors sm)

-- | List all square roots modulo a number, the factorisation of which is
-- passed as a second argument.
--
-- >>> sqrtsModFactorisation 1 (factorise 60)
-- [1,49,41,29,31,19,11,59]
sqrtsModFactorisation :: Integer -> [(Prime Integer, Word)] -> [Integer]
sqrtsModFactorisation _ []  = [0]
sqrtsModFactorisation n pps = map fst $ foldl1 (liftM2 comb) cs
  where
    ms :: [Integer]
    ms = map (\(p, pow) -> unPrime p ^ pow) pps

    rs :: [[Integer]]
    rs = map (uncurry (sqrtsModPrimePower n)) pps

    cs :: [[(Integer, Integer)]]
    cs = zipWith (\l m -> map (, m) l) rs ms

    comb t1 t2 = (if ch < 0 then ch + m else ch, m)
      where
        (ch, m) = fromJust $ chinese t1 t2

-- | List all square roots modulo the power of a prime.
--
-- >>> import Data.Maybe
-- >>> import Math.NumberTheory.Primes
-- >>> sqrtsModPrimePower 7 (fromJust (isPrime 3)) 2
-- [4,5]
-- >>> sqrtsModPrimePower 9 (fromJust (isPrime 3)) 3
-- [3,12,21,24,6,15]
sqrtsModPrimePower :: Integer -> Prime Integer -> Word -> [Integer]
sqrtsModPrimePower nn p 1 = sqrtsModPrime nn p
sqrtsModPrimePower nn (unPrime -> prime) expo = let primeExpo = prime ^ expo in
  case splitOff prime (nn `mod` primeExpo) of
    (_, 0) -> [0, prime ^ ((expo + 1) `quot` 2) .. primeExpo - 1]
    (kk, n)
      | odd kk    -> []
      | otherwise -> case (if prime == 2 then sqM2P n expo' else sqrtModPP' n prime expo') of
        Nothing -> []
        Just r  -> let rr = r * prime ^ k in
          if prime == 2 && k + 1 == t
          then go rr os
          else go rr os ++ go (primeExpo - rr) os
      where
        k = kk `quot` 2
        t = (if prime == 2 then expo - k - 1 else expo - k) `max` ((expo + 1) `quot` 2)
        expo' = expo - 2 * k
        os = [0, prime ^ t .. primeExpo - 1]

        -- equivalent to map ((`mod` primeExpo) . (+ r)) rs,
        -- but avoids division
        go r rs = map (+ r) ps ++ map (+ (r - primeExpo)) qs
          where
            (ps, qs) = span (< primeExpo - r) rs

-- | List all square roots by prime modulo.
--
-- >>> import Data.Maybe
-- >>> import Math.NumberTheory.Primes
-- >>> sqrtsModPrime 1 (fromJust (isPrime 5))
-- [1,4]
-- >>> sqrtsModPrime 0 (fromJust (isPrime 5))
-- [0]
-- >>> sqrtsModPrime 2 (fromJust (isPrime 5))
-- []
sqrtsModPrime :: Integer -> Prime Integer -> [Integer]
sqrtsModPrime n (unPrime -> 2) = [n `mod` 2]
sqrtsModPrime n (unPrime -> prime) = case jacobi n prime of
  MinusOne -> []
  Zero     -> [0]
  One      -> case someNatVal (fromInteger prime) of
    SomeNat (_ :: Proxy p) -> let r = toInteger (unMod (sqrtModP' @p (fromInteger n))) in [r, prime - r]

-------------------------------------------------------------------------------
-- Internals

-- | @sqrtModP' square prime@ finds a square root of @square@ modulo
--   prime. @prime@ /must/ be a (positive) prime, and @square@ /must/ be a positive
--   quadratic residue modulo @prime@, i.e. @'jacobi square prime == 1@.
sqrtModP' :: KnownNat p => Mod p -> Mod p
sqrtModP' square
  | prime == 2         = square
  | rem4 prime == 3    = square ^ ((prime + 1) `quot` 4)
  | square == maxBound = sqrtOfMinusOne
  | otherwise          = tonelliShanks square
  where
    prime = natVal square

-- | @p@ must be of form @4k + 1@
sqrtOfMinusOne :: forall (p :: Nat). KnownNat p => Mod p
sqrtOfMinusOne = case results of
  [] -> error "sqrtOfMinusOne: internal invariant violated"
  hd : _ -> hd
  where
    p :: Natural
    p = natVal (Proxy :: Proxy p)

    k :: Natural
    k = (p - 1) `quot` 4

    results :: [Mod p]
    results = dropWhile (\n -> n == 1 || n == maxBound) $
      map (^ k) [2 .. maxBound - 1]

-- | @tonelliShanks square prime@ calculates a square root of @square@
--   modulo @prime@, where @prime@ is a prime of the form @4*k + 1@ and
--   @square@ is a positive quadratic residue modulo @prime@, using the
--   Tonelli-Shanks algorithm.
tonelliShanks :: forall p. KnownNat p => Mod p -> Mod p
tonelliShanks square = loop rc t1 generator log2
  where
    prime = natVal square
    (log2, q) = shiftToOddCount (prime - 1)
    generator = findNonSquare ^ q
    rc = square ^ ((q + 1) `quot` 2)
    t1 = square ^ q

    msquare 0 x = x
    msquare k x = msquare (k-1) (x * x)

    findPeriod per 1 = per
    findPeriod per x = findPeriod (per + 1) (x * x)

    loop :: Mod p -> Mod p -> Mod p -> Word -> Mod p
    loop !r t c m
        | t == 1    = r
        | otherwise = loop nextR nextT nextC nextM
          where
            nextM = findPeriod 0 t
            b     = msquare (m - 1 - nextM) c
            nextR = r * b
            nextC = b * b
            nextT = t * nextC

-- | prime must be odd, n must be coprime with prime
sqrtModPP' :: Integer -> Integer -> Word -> Maybe Integer
sqrtModPP' n prime expo = case jacobi n prime of
  MinusOne -> Nothing
  Zero     -> Nothing
  One      -> case someNatVal (fromInteger prime) of
    SomeNat (_ :: Proxy p) -> Just $ fixup $ sqrtModP' @p (fromInteger n)
  where
    fixup :: KnownNat p => Mod p -> Integer
    fixup r
      | diff' == 0 = r'
      | expo <= e  = r'
      | otherwise  = hoist (recip (2 * r)) r' (fromInteger q) (prime^e)
      where
        r' = toInteger (unMod r)
        diff' = r' * r' - n
        (e, q) = splitOff prime diff'

    hoist :: KnownNat p => Mod p -> Integer -> Mod p -> Integer -> Integer
    hoist inv root elim pp
      | diff' == 0    = root'
      | expo <= ex    = root'
      | otherwise     = hoist inv root' (fromInteger nelim) (prime ^ ex)
        where
          root' = root + toInteger (unMod (inv * negate elim)) * pp
          diff' = root' * root' - n
          (ex, nelim) = splitOff prime diff'

-- dirty, dirty
sqM2P :: Integer -> Word -> Maybe Integer
sqM2P n e
    | e < 2     = Just (n `mod` 2)
    | n' == 0   = Just 0
    | odd k     = Nothing
    | otherwise = (`mod` mdl) . (`shiftL` wordToInt k2) <$> solve s e2
      where
        mdl = 1 `shiftL` wordToInt e
        n' = n `mod` mdl
        (k, s) = shiftToOddCount n'
        k2 = k `quot` 2
        e2 = e - k
        solve _ 1 = Just 1
        solve 1 _ = Just 1
        solve r _
            | rem4 r == 3   = Nothing  -- otherwise r ≡ 1 (mod 4)
            | rem8 r == 5   = Nothing  -- otherwise r ≡ 1 (mod 8)
            | otherwise     = fixup r (fst $ shiftToOddCount (r-1))
              where
                fixup x pw
                    | pw >= e2  = Just x
                    | otherwise = fixup x' pw'
                      where
                        x' = x + (1 `shiftL` (wordToInt pw - 1))
                        d = x'*x' - r
                        pw' = if d == 0 then e2 else fst (shiftToOddCount d)

-------------------------------------------------------------------------------
-- Utilities

rem4 :: Integral a => a -> Int
rem4 n = fromIntegral n .&. 3

rem8 :: Integral a => a -> Int
rem8 n = fromIntegral n .&. 7

findNonSquare :: forall (n :: Nat). KnownNat n => Mod n
findNonSquare
  | rem8 n == 3 || rem8 n == 5 = 2
  | otherwise = fromIntegral $ Inf.head $
    Inf.dropWhile (\p -> jacobi p n /= MinusOne) candidates
  where
    n = natVal (Proxy :: Proxy n)

    -- It is enough to consider only prime candidates, but
    -- the probability that the smallest non-residue is > 67
    -- is small and 'jacobi' test is fast,
    -- so we use [71..n] instead of filter isPrime [71..n].
    candidates :: Infinite Natural
    candidates = 3 :< 5 :< 7 :< 11 :< 13 :< 17 :< 19 :< 23 :< 29 :< 31 :<
      37 :< 41 :< 43 :< 47 :< 53 :< 59 :< 61 :< 67 :< (71...)
