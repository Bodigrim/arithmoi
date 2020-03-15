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
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP          #-}

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
import Data.Maybe
import Data.Mod

import Math.NumberTheory.Moduli.Chinese
import Math.NumberTheory.Moduli.JacobiSymbol
import Math.NumberTheory.Moduli.Singleton
import Math.NumberTheory.Powers.Modular (powMod)
import Math.NumberTheory.Primes
import Math.NumberTheory.Utils (shiftToOddCount, splitOff, recipMod)
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
    rs = map (\(p, pow) -> sqrtsModPrimePower n p pow) pps

    cs :: [[(Integer, Integer)]]
    cs = zipWith (\l m -> map (\x -> (x, m)) l) rs ms

    comb t1@(_, m1) t2@(_, m2) = (if ch < 0 then ch + m else ch, m)
      where
        ch = fromJust $ chinese t1 t2
        m = m1 * m2

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
  One      -> let r = sqrtModP' (n `mod` prime) prime in [r, prime - r]

-------------------------------------------------------------------------------
-- Internals

-- | @sqrtModP' square prime@ finds a square root of @square@ modulo
--   prime. @prime@ /must/ be a (positive) prime, and @square@ /must/ be a positive
--   quadratic residue modulo @prime@, i.e. @'jacobi square prime == 1@.
sqrtModP' :: Integer -> Integer -> Integer
sqrtModP' square prime
    | prime == 2    = square
    | rem4 prime == 3 = powMod square ((prime + 1) `quot` 4) prime
    | square `mod` prime == prime - 1
                    = sqrtOfMinusOne prime
    | otherwise     = tonelliShanks square prime

-- | @p@ must be of form @4k + 1@
sqrtOfMinusOne :: Integer -> Integer
sqrtOfMinusOne p
  = head
  $ filter (\n -> n /= 1 && n /= p - 1)
  $ map (\n -> powMod n k p)
    [2..p-2]
  where
    k = (p - 1) `quot` 4

-- | @tonelliShanks square prime@ calculates a square root of @square@
--   modulo @prime@, where @prime@ is a prime of the form @4*k + 1@ and
--   @square@ is a positive quadratic residue modulo @prime@, using the
--   Tonelli-Shanks algorithm.
tonelliShanks :: Integer -> Integer -> Integer
tonelliShanks square prime = loop rc t1 generator log2
  where
    (wordToInt -> log2,q) = shiftToOddCount (prime-1)
    nonSquare = findNonSquare prime
    generator = powMod nonSquare q prime
    rc = powMod square ((q+1) `quot` 2) prime
    t1 = powMod square q prime
    msqr x = (x*x) `rem` prime
    msquare 0 x = x
    msquare k x = msquare (k-1) (msqr x)
    findPeriod per 1 = per
    findPeriod per x = findPeriod (per+1) (msqr x)

    loop :: Integer -> Integer -> Integer -> Int -> Integer
    loop !r t c m
        | t == 1    = r
        | otherwise = loop nextR nextT nextC nextM
          where
            nextM = findPeriod 0 t
            b     = msquare (m - 1 - nextM) c
            nextR = (r*b) `rem` prime
            nextC = msqr b
            nextT = (t*nextC) `rem` prime

-- | prime must be odd, n must be coprime with prime
sqrtModPP' :: Integer -> Integer -> Word -> Maybe Integer
sqrtModPP' n prime expo = case jacobi n prime of
  MinusOne -> Nothing
  Zero     -> Nothing
  One      -> fixup $ sqrtModP' (n `mod` prime) prime
  where
    fixup r = let diff' = r*r-n
              in if diff' == 0
                   then Just r
                   else case splitOff prime diff' of
                          (e,q) | expo <= e -> Just r
                                | otherwise -> fmap (\inv -> hoist inv r (q `mod` prime) (prime^e)) (recipMod (2*r) prime)

    hoist inv root elim pp
        | diff' == 0    = root'
        | expo <= ex    = root'
        | otherwise     = hoist inv root' (nelim `mod` prime) (prime^ex)
          where
            root' = (root + (inv*(prime-elim))*pp) `mod` (prime*pp)
            diff' = root'*root' - n
            (ex, nelim) = splitOff prime diff'

-- dirty, dirty
sqM2P :: Integer -> Word -> Maybe Integer
sqM2P n e
    | e < 2     = Just (n `mod` 2)
    | n' == 0   = Just 0
    | odd k     = Nothing
    | otherwise = fmap ((`mod` mdl) . (`shiftL` wordToInt k2)) $ solve s e2
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

findNonSquare :: Integer -> Integer
findNonSquare n
    | rem8 n == 5 || rem8 n == 3 = 2
    | otherwise = search candidates
      where
        -- It is enough to consider only prime candidates, but
        -- the probability that the smallest non-residue is > 67
        -- is small and 'jacobi' test is fast,
        -- so we use [71..n] instead of filter isPrime [71..n].
        candidates = 3:5:7:11:13:17:19:23:29:31:37:41:43:47:53:59:61:67:[71..n]
        search (p:ps) = case jacobi p n of
          MinusOne -> p
          _        -> search ps
        search _ = error "Should never have happened, prime list exhausted."
