-- |
-- Module:      Math.NumberTheory.Powers.General
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Calculating integer roots and determining perfect powers.
-- The algorithms are moderately efficient.
--
{-# LANGUAGE MagicHash, BangPatterns, CPP #-}
{-# OPTIONS_GHC -O2 -fspec-constr-count=8 #-}
module Math.NumberTheory.Powers.General
    ( integerRoot
    , exactRoot
    , isKthPower
    , isPerfectPower
    , highestPower
    , largePFPower
    ) where

#include "MachDeps.h"

import GHC.Base
import GHC.Integer
import GHC.Integer.GMP.Internals
import GHC.Integer.Logarithms (integerLog2#)

import Data.Bits
import Data.List (foldl')
import qualified Data.Set as Set

import Math.NumberTheory.Logarithms (integerLogBase')
import Math.NumberTheory.Utils  (shiftToOddCount
                                , splitOff
#if __GLASGOW_HASKELL__ < 707
                                , isTrue#
#endif
                                )
import qualified Math.NumberTheory.Powers.Squares as P2
import qualified Math.NumberTheory.Powers.Cubes as P3
import qualified Math.NumberTheory.Powers.Fourth as P4

-- | Calculate an integer root, @'integerRoot' k n@ computes the (floor of) the @k@-th
--   root of @n@, where @k@ must be positive.
--   @r = 'integerRoot' k n@ means @r^k <= n < (r+1)^k@ if that is possible at all.
--   It is impossible if @k@ is even and @n \< 0@, since then @r^k >= 0@ for all @r@,
--   then, and if @k <= 0@, @'integerRoot'@ raises an error. For @k < 5@, a specialised
--   version is called which should be more efficient than the general algorithm.
--   However, it is not guaranteed that the rewrite rules for those fire, so if @k@ is
--   known in advance, it is safer to directly call the specialised versions.
{-# SPECIALISE integerRoot :: Int -> Int -> Int,
                              Int -> Word -> Word,
                              Int -> Integer -> Integer,
                              Word -> Int -> Int,
                              Word -> Word -> Word,
                              Word -> Integer -> Integer,
                              Integer -> Integer -> Integer
  #-}
integerRoot :: (Integral a, Integral b) => b -> a -> a
integerRoot 1 n         = n
integerRoot 2 n         = P2.integerSquareRoot n
integerRoot 3 n         = P3.integerCubeRoot n
integerRoot 4 n         = P4.integerFourthRoot n
integerRoot k n
  | k < 1             = error "integerRoot: negative exponent or exponent 0"
  | n < 0 && even k   = error "integerRoot: negative radicand for even exponent"
  | n < 0             =
    let r = negate . fromInteger . integerRoot k . negate $ fromIntegral n
    in if r^k == n then r else (r-1)
  | n == 0            = 0
  | n < 31            = 1
  | kTooLarge         = 1
  | otherwise         = newtonK k' n a
    where
      k' = fromIntegral k
      a  = approxKthRoot (fromIntegral k) n
      kTooLarge = (toInteger k /= toInteger (fromIntegral k `asTypeOf` n))    -- k doesn't fit in n's type
                  || (toInteger k > toInteger (maxBound :: Int))  -- 2^k doesn't fit in Integer
                  || (I# (integerLog2# (toInteger n)) < fromIntegral k) -- n < 2^k

-- | @'exactRoot' k n@ returns @'Nothing'@ if @n@ is not a @k@-th power,
--   @'Just' r@ if @n == r^k@. If @k@ is divisible by @4, 3@ or @2@, a
--   residue test is performed to avoid the expensive calculation if it
--   can thus be determined that @n@ is not a @k@-th power.
exactRoot :: (Integral a, Integral b) => b -> a -> Maybe a
exactRoot 1 n = Just n
exactRoot 2 n = P2.exactSquareRoot n
exactRoot 3 n = P3.exactCubeRoot n
exactRoot 4 n = P4.exactFourthRoot n
exactRoot k n
  | n == 1          = Just 1
  | k < 1           = Nothing
  | n < 0 && even k = Nothing
  | n < 0           = fmap negate (exactRoot k (-n))
  | n < 2           = Just n
  | n < 31          = Nothing
  | kTooLarge       = Nothing
  | otherwise       = case k `rem` 12 of
                        0 | c4 && c3 && ok -> Just r
                          | otherwise -> Nothing
                        2 | c2 && ok -> Just r
                          | otherwise -> Nothing
                        3 | c3 && ok -> Just r
                          | otherwise -> Nothing
                        4 | c4 && ok -> Just r
                          | otherwise -> Nothing
                        6 | c3 && c2 && ok -> Just r
                          | otherwise -> Nothing
                        8 | c4 && ok -> Just r
                          | otherwise -> Nothing
                        9 | c3 && ok -> Just r
                          | otherwise -> Nothing
                        10 | c2 && ok -> Just r
                           | otherwise -> Nothing
                        _ | ok -> Just r
                          | otherwise -> Nothing

    where
      k' :: Int
      k' = fromIntegral k
      r  = integerRoot k' n
      c2 = P2.isPossibleSquare n
      c3 = P3.isPossibleCube n
      c4 = P4.isPossibleFourthPower n
      ok = r^k == n
      kTooLarge = (toInteger k /= toInteger (fromIntegral k `asTypeOf` n))    -- k doesn't fit in n's type
                  || (toInteger k > toInteger (maxBound :: Int))  -- 2^k doesn't fit in Integer
                  || (I# (integerLog2# (toInteger n)) < fromIntegral k) -- n < 2^k

-- | @'isKthPower' k n@ checks whether @n@ is a @k@-th power.
isKthPower :: (Integral a, Integral b) => b -> a -> Bool
isKthPower k n = case exactRoot k n of
                   Just _ -> True
                   Nothing -> False

-- | @'isPerfectPower' n@ checks whether @n == r^k@ for some @k > 1@.
isPerfectPower :: Integral a => a -> Bool
isPerfectPower n
  | n == 0 || n == 1    = True
  | otherwise           = k > 1
    where
      (_,k) = highestPower n

-- | @'highestPower' n@ produces the pair @(b,k)@ with the largest
--   exponent @k@ such that @n == b^k@, except for @'abs' n <= 1@,
--   in which case arbitrarily large exponents exist, and by an
--   arbitrary decision @(n,3)@ is returned.
--
--   First, by trial division with small primes, the range of possible
--   exponents is reduced (if @p^e@ exactly divides @n@, then @k@ must
--   be a divisor of @e@, if several small primes divide @n@, @k@ must
--   divide the greatest common divisor of their exponents, which mostly
--   will be @1@, generally small; if none of the small primes divides
--   @n@, the range of possible exponents is reduced since the base is
--   necessarily large), if that has not yet determined the result, the
--   remaining factor is examined by trying the divisors of the @gcd@
--   of the prime exponents if some have been found, otherwise by trying
--   prime exponents recursively.
highestPower :: Integral a => a -> (a, Int)
highestPower n'
  | abs n <= 1  = (n', 3)
  | n < 0       = case integerHighPower (negate n) of
                    (r,e) -> case shiftToOddCount e of
                               (k, o) -> (negate $ fromInteger (sqr k r), o)
  | otherwise   = case integerHighPower n of
                    (r,e) -> (fromInteger r, e)
    where
      n :: Integer
      n = toInteger n'

      sqr :: Int -> Integer -> Integer
      sqr 0 m = m
      sqr k m = sqr (k-1) (m*m)

-- | @'largePFPower' bd n@ produces the pair @(b,k)@ with the largest
--   exponent @k@ such that @n == b^k@, where @bd > 1@ (it is expected
--   that @bd@ is much larger, at least @1000@ or so), @n > bd^2@ and @n@
--   has no prime factors @p <= bd@, skipping the trial division phase
--   of @'highestPower'@ when that is a priori known to be superfluous.
--   It is only present to avoid duplication of work in factorisation
--   and primality testing, it is not expected to be generally useful.
--   The assumptions are not checked, if they are not satisfied, wrong
--   results and wasted work may be the consequence.
largePFPower :: Integer -> Integer -> (Integer, Int)
largePFPower bd n = rawPower ln n
  where
    ln = integerLogBase' (bd+1) n

------------------------------------------------------------------------------------------
--                                  Auxiliary functions                                 --
------------------------------------------------------------------------------------------

{-# SPECIALISE newtonK :: Int -> Int -> Int -> Int,
                          Integer -> Integer -> Integer -> Integer,
                          Word -> Word -> Word -> Word
  #-}
newtonK :: Integral a => a -> a -> a -> a
newtonK k n a = go (step a)
  where
    -- Beware integer overflow in m^(k-1)
    step m = ((k-1)*m + fromInteger (toInteger n `quot` (toInteger m^(k-1)))) `quot` k
    go m
      | l < m     = go l
      | otherwise = m
        where
          l = step m

{-# SPECIALISE approxKthRoot :: Int -> Integer -> Integer,
                                Int -> Int -> Int,
                                Int -> Word -> Word
  #-}
approxKthRoot :: Integral a => Int -> a -> a
approxKthRoot k = fromInteger . appKthRoot k . fromIntegral

-- find an approximation to the k-th root
-- here, k > 4 and n > 31
appKthRoot :: Int -> Integer -> Integer
appKthRoot (I# k#) (S# n#) = S# (double2Int# (int2Double# n# **## (1.0## /## int2Double# k#)))
appKthRoot k@(I# k#) n =
    case integerLog2# n of
      l# -> case l# `quotInt#` k# of
              0# -> 1
              1# -> 3
              2# -> 5
              3# -> 11
              h# | isTrue# (h# <# 500#) ->
                   floor (scaleFloat (I# (h# -# 1#))
                          (fromInteger (n `shiftRInteger` (h# *# k# -# k#)) ** (1/fromIntegral k) :: Double))
                 | otherwise ->
                   floor (scaleFloat 400 (fromInteger (n `shiftRInteger` (h# *# k# -# k#)) ** (1/fromIntegral k) :: Double))
                          `shiftLInteger` (h# -# 401#)

-- assumption: argument is > 1
integerHighPower :: Integer -> (Integer, Int)
integerHighPower n
  | n < 4       = (n,1)
  | otherwise   = case shiftToOddCount n of
                    (e2,m) | m == 1     -> (2,e2)
                           | otherwise  -> findHighPower e2 (if e2 == 0 then [] else [(2,e2)]) m r smallOddPrimes
                             where
                               r = P2.integerSquareRoot m

findHighPower :: Int -> [(Integer,Int)] -> Integer -> Integer -> [Integer] -> (Integer, Int)
findHighPower 1 pws m _ _ = (foldl' (*) m [p^e | (p,e) <- pws], 1)
findHighPower e pws 1 _ _ = (foldl' (*) 1 [p^(ex `quot` e) | (p,ex) <- pws], e)
findHighPower e pws m s (p:ps)
  | s < p       = findHighPower 1 pws m s []
  | otherwise   =
    case splitOff p m of
      (0,_) -> findHighPower e pws m s ps
      (k,r) -> findHighPower (gcd k e) ((p,k):pws) r (P2.integerSquareRoot r) ps
findHighPower e pws m _ [] = finishPower e pws m

spBEx :: Int
spBEx = 14

spBound :: Integer
spBound = 2^spBEx

smallOddPrimes :: [Integer]
smallOddPrimes = 3:5:primes'
  where
    primes' = 7:11:13:17:19:23:29:filter isPrime (takeWhile (< spBound) $ scanl (+) 31 (cycle [6,4,2,4,2,4,6,2]))
    isPrime n = go primes'
      where
        go (p:ps) = (p*p > n) || (n `rem` p /= 0 && go ps)
        go []     = True

-- n large, has no prime divisors < spBound
finishPower :: Int -> [(Integer, Int)] -> Integer -> (Integer, Int)
finishPower e pws n
  | n < (1 `shiftL` (2*spBEx))  = (foldl' (*) n [p^ex | (p,ex) <- pws], 1)    -- n is prime
  | e == 0  = rawPower maxExp n
  | otherwise = go divs
    where
      maxExp = (I# (integerLog2# n)) `quot` spBEx
      divs = divisorsTo maxExp e
      go [] = (foldl' (*) n [p^ex | (p,ex) <- pws], 1)
      go (d:ds) = case exactRoot d n of
                    Just r -> (foldl' (*) r [p^(ex `quot` d) | (p,ex) <- pws], d)
                    Nothing -> go ds

rawPower :: Int -> Integer -> (Integer, Int)
rawPower mx n
  | mx < 2      = (n,1)
  | mx == 2     = case P2.exactSquareRoot n of
                    Just r  -> (r,2)
                    Nothing -> (n,1)
rawPower mx n = case P4.exactFourthRoot n of
                  Just r -> case rawPower (mx `quot` 4) r of
                              (m,e) -> (m, 4*e)
                  Nothing -> case P2.exactSquareRoot n of
                               Just r -> case rawOddPower (mx `quot` 2) r of
                                           (m,e) -> (m, 2*e)
                               Nothing -> rawOddPower mx n

rawOddPower :: Int -> Integer -> (Integer, Int)
rawOddPower mx n
  | mx < 3       = (n,1)
rawOddPower mx n = case P3.exactCubeRoot n of
                     Just r -> case rawOddPower (mx `quot` 3) r of
                                 (m,e) -> (m, 3*e)
                     Nothing -> badPower mx n

badPower :: Int -> Integer -> (Integer, Int)
badPower mx n
  | mx < 5      = (n,1)
  | otherwise   = go 1 mx n (takeWhile (<= mx) $ scanl (+) 5 $ cycle [2,4])
    where
      go !e b m (k:ks)
        | b < k     = (m,e)
        | otherwise = case exactRoot k m of
                        Just r -> go (e*k) (b `quot` k) r (k:ks)
                        Nothing -> go e b m ks
      go e _ m []   = (m,e)

divisorsTo :: Int -> Int -> [Int]
divisorsTo mx n = case shiftToOddCount n of
                    (k,o) | k == 0 -> go (Set.singleton 1) n iops
                          | otherwise -> go (Set.fromDistinctAscList $ takeWhile (<= mx) $ take (k+1) (iterate (*2) 1)) o iops
  where
    mset k st = fst (Set.split (mx+1) (Set.mapMonotonic (*k) st))
    -- unP p m = (k, m / p ^ k), where k is as large as possible such that p ^ k still divides m
    unP :: Int -> Int -> (Int,Int)
    unP p m = goP 0 m
      where
        goP :: Int -> Int -> (Int,Int)
        goP !i j = case j `quotRem` p of
                     (q,r) | r == 0 -> goP (i+1) q
                           | otherwise -> (i,j)
    iops :: [Int]
    iops = 3:5:prs
    prs :: [Int]
    prs = 7:filter prm (scanl (+) 11 $ cycle [2,4,2,4,6,2,6,4])
    prm :: Int -> Bool
    prm k = td prs
      where
        td (p:ps) = (p*p > k) || (k `rem` p /= 0 && td ps)
        td []     = True
    go !st m (p:ps)
      | m == 1  = reverse $ Set.toAscList st
      | m < p*p = reverse . Set.toAscList $ Set.union st (mset m st)
      | otherwise =
        case unP p m of
          (0,_) -> go st m ps
          -- iterate f x = [x, f x, f (f x)...]
          (k,r) -> go (Set.unions (take (k + 1) (iterate (mset p) st))) r ps
    go st m [] = go st m [m+1]
