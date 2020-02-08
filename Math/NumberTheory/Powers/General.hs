-- |
-- Module:      Math.NumberTheory.Powers.General
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Description: Deprecated
--
-- Calculating integer roots and determining perfect powers.
-- The algorithms are moderately efficient.
--

{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Math.NumberTheory.Powers.General
  {-# DEPRECATED "Use Math.NumberTheory.Roots" #-}
    ( integerRoot
    , exactRoot
    , isKthPower
    , isPerfectPower
    , highestPower
    , largePFPower
    ) where

#include "MachDeps.h"

import Math.NumberTheory.Logarithms (integerLogBase')
import qualified Math.NumberTheory.Powers.Squares as P2
import qualified Math.NumberTheory.Powers.Cubes as P3
import qualified Math.NumberTheory.Powers.Fourth as P4
import Math.NumberTheory.Utils.FromIntegral (intToWord)

import Math.NumberTheory.Roots

-- | @'largePFPower' bd n@ produces the pair @(b,k)@ with the largest
--   exponent @k@ such that @n == b^k@, where @bd > 1@ (it is expected
--   that @bd@ is much larger, at least @1000@ or so), @n > bd^2@ and @n@
--   has no prime factors @p <= bd@, skipping the trial division phase
--   of @'highestPower'@ when that is a priori known to be superfluous.
--   It is only present to avoid duplication of work in factorisation
--   and primality testing, it is not expected to be generally useful.
--   The assumptions are not checked, if they are not satisfied, wrong
--   results and wasted work may be the consequence.
largePFPower :: Integer -> Integer -> (Integer, Word)
largePFPower bd n = rawPower ln n
  where
    ln = intToWord (integerLogBase' (bd+1) n)

------------------------------------------------------------------------------------------
--                                  Auxiliary functions                                 --
------------------------------------------------------------------------------------------

rawPower :: Word -> Integer -> (Integer, Word)
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

rawOddPower :: Word -> Integer -> (Integer, Word)
rawOddPower mx n
  | mx < 3       = (n,1)
rawOddPower mx n = case P3.exactCubeRoot n of
                     Just r -> case rawOddPower (mx `quot` 3) r of
                                 (m,e) -> (m, 3*e)
                     Nothing -> badPower mx n

badPower :: Word -> Integer -> (Integer, Word)
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
