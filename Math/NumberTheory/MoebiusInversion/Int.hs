-- |
-- Module:      Math.NumberTheory.MoebiusInversion
-- Copyright:   (c) 2012 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
--
-- Generalised Möbius inversion for 'Int' valued functions.
--
{-# LANGUAGE BangPatterns, FlexibleContexts #-}
{-# OPTIONS_GHC -fspec-constr-count=8 #-}

module Math.NumberTheory.MoebiusInversion.Int
  ( generalInversion
  , totientSum
  ) where

import Prelude hiding (replicate)

import Control.Monad
import Control.Monad.ST

import Math.NumberTheory.Powers.Squares
import Math.NumberTheory.Unsafe

-- | @totientSum n@ is, for @n > 0@, the sum of @[totient k | k <- [1 .. n]]@,
--   computed via generalised Möbius inversion.
--   See <http://mathworld.wolfram.com/TotientSummatoryFunction.html> for the
--   formula used for @totientSum@.
totientSum :: Int -> Int
totientSum n
  | n < 1 = 0
  | otherwise = generalInversion (triangle . fromIntegral) n
  where
    triangle k = (k * (k + 1)) `quot` 2

-- | @generalInversion g n@ evaluates the generalised Möbius inversion of @g@
--   at the argument @n@.
--
--   The generalised Möbius inversion implemented here allows an efficient
--   calculation of isolated values of the function @f : N -> Z@ if the function
--   @g@ defined by
--
-- >
-- > g n = sum [f (n `quot` k) | k <- [1 .. n]]
-- >
--
--   can be cheaply computed. By the generalised Möbius inversion formula, then
--
-- >
-- > f n = sum [moebius k * g (n `quot` k) | k <- [1 .. n]]
-- >
--
--   which allows the computation in /O/(n) steps, if the values of the
--   Möbius function are known. A slightly different formula, used here,
--   does not need the values of the Möbius function and allows the
--   computation in /O/(n^0.75) steps, using /O/(n^0.5) memory.
--
--   An example of a pair of such functions where the inversion allows a
--   more efficient computation than the direct approach is
--
-- >
-- > f n = number of reduced proper fractions with denominator <= n
-- > g n = number of proper fractions with denominator <= n
-- >
--
--   (a /proper fraction/ is a fraction @0 < n/d < 1@). Then @f n@ is the
--   cardinality of the Farey sequence of order @n@ (minus 1 or 2 if 0 and/or
--   1 are included in the Farey sequence), or the sum of the totients of
--   the numbers @2 <= k <= n@. @f n@ is not easily directly computable,
--   but then @g n = n*(n-1)/2@ is very easy to compute, and hence the inversion
--   gives an efficient method of computing @f n@.
--
--   For 'Int' valued functions, unboxed arrays can be used for greater efficiency.
--   That bears the risk of overflow, however, so be sure to use it only when it's
--   safe.
--
--   The value @f n@ is then computed by @generalInversion g n@. Note that when
--   many values of @f@ are needed, there are far more efficient methods, this
--   method is only appropriate to compute isolated values of @f@.
generalInversion :: (Int -> Int) -> Int -> Int
generalInversion fun n
  | n < 1 = error "Möbius inversion only defined on positive domain"
  | n == 1 = fun 1
  | n == 2 = fun 2 - fun 1
  | n == 3 = fun 3 - 2 * fun 1
  | otherwise = fastInvert fun n

fastInvert :: (Int -> Int) -> Int -> Int
fastInvert fun n = big `unsafeIndex` 0
  where
    !k0 = integerSquareRoot (n `quot` 2)
    !mk0 = n `quot` (2 * k0 + 1)
    kmax a m = (a `quot` m - 1) `quot` 2
    big =
      runST $ do
        small <- replicate (mk0 + 1) 0 :: ST s (STVector s Int)
        unsafeWrite small 0 0
        unsafeWrite small 1 (fun 1)
        when (mk0 >= 2) $ unsafeWrite small 2 (fun 2 - fun 1)
        let calcit switch change i
              | mk0 < i = return (switch, change)
              | i == change = calcit (switch + 1) (change + 4 * switch + 6) i
              | otherwise = do
                let mloop !acc k !m
                      | k < switch = kloop acc k
                      | otherwise = do
                        val <- unsafeRead small m
                        let nxtk = kmax i (m + 1)
                        mloop (acc - (k - nxtk) * val) nxtk (m + 1)
                    kloop !acc k
                      | k == 0 = do
                        unsafeWrite small i acc
                        calcit switch change (i + 1)
                      | otherwise = do
                        val <- unsafeRead small (i `quot` (2 * k + 1))
                        kloop (acc - val) (k - 1)
                mloop (fun i - fun (i `quot` 2)) ((i - 1) `quot` 2) 1
        (sw, ch) <- calcit 1 8 3
        large <- replicate k0 0 :: ST s (STVector s Int)
        let calcbig switch change j
              | j == 0 = return large
              | (2 * j - 1) * change <= n =
                calcbig (switch + 1) (change + 4 * switch + 6) j
              | otherwise = do
                let i = n `quot` (2 * j - 1)
                    mloop !acc k m
                      | k < switch = kloop acc k
                      | otherwise = do
                        val <- unsafeRead small m
                        let nxtk = kmax i (m + 1)
                        mloop (acc - (k - nxtk) * val) nxtk (m + 1)
                    kloop !acc k
                      | k == 0 = do
                        unsafeWrite large (j - 1) acc
                        calcbig switch change (j - 1)
                      | otherwise = do
                        let m = i `quot` (2 * k + 1)
                        val <-
                          if m <= mk0
                            then unsafeRead small m
                            else unsafeRead large (k * (2 * j - 1) + j - 1)
                        kloop (acc - val) (k - 1)
                mloop (fun i - fun (i `quot` 2)) ((i - 1) `quot` 2) 1
        calcbig sw ch k0 >>= unsafeFreeze
