-- |
-- Module:      Math.NumberTheory.MoebiusInversion
-- Copyright:   (c) 2012 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
--
-- Generalised Möbius inversion

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.MoebiusInversion
    ( generalInversion
    , totientSum
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Proxy
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG

import Math.NumberTheory.Roots
import Math.NumberTheory.Utils.FromIntegral

-- | @totientSum n@ is, for @n > 0@, the sum of @[totient k | k <- [1 .. n]]@,
--   computed via generalised Möbius inversion.
--   See <http://mathworld.wolfram.com/TotientSummatoryFunction.html> for the
--   formula used for @totientSum@.
--
-- >>> import Data.Proxy
-- >>> totientSum (Proxy :: Proxy Data.Vector.Unboxed.Vector) 100 :: Int
-- 3044
-- >>> totientSum (Proxy :: Proxy Data.Vector.Vector) 100 :: Integer
-- 3044
totientSum
    :: (Integral t, G.Vector v t)
    => Proxy v
    -> Word
    -> t
totientSum _ 0 = 0
totientSum proxy n = generalInversion proxy (triangle . fromIntegral) n
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
-- >
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
--   Since the function arguments are used as array indices, the domain of
--   @f@ is restricted to 'Int'.
--
--   The value @f n@ is then computed by @generalInversion g n@. Note that when
--   many values of @f@ are needed, there are far more efficient methods, this
--   method is only appropriate to compute isolated values of @f@.
generalInversion
    :: (Num t, G.Vector v t)
    => Proxy v
    -> (Word -> t)
    -> Word
    -> t
generalInversion proxy fun n = case n of
    0 ->error "Möbius inversion only defined on positive domain"
    1 -> fun 1
    2 -> fun 2 - fun 1
    3 -> fun 3 - 2*fun 1
    _ -> runST (fastInvertST proxy (fun . intToWord) (wordToInt n))

fastInvertST
    :: forall s t v.
       (Num t, G.Vector v t)
    => Proxy v
    -> (Int -> t)
    -> Int
    -> ST s t
fastInvertST _ fun n = do
    let !k0 = integerSquareRoot (n `quot` 2)
        !mk0 = n `quot` (2*k0+1)
        kmax a m = (a `quot` m - 1) `quot` 2

    small <- MG.unsafeNew (mk0 + 1) :: ST s (G.Mutable v s t)
    MG.unsafeWrite small 0 0
    MG.unsafeWrite small 1 $! (fun 1)
    when (mk0 >= 2) $
        MG.unsafeWrite small 2 $! (fun 2 - fun 1)

    let calcit :: Int -> Int -> Int -> ST s (Int, Int)
        calcit switch change i
            | mk0 < i   = return (switch,change)
            | i == change = calcit (switch+1) (change + 4*switch+6) i
            | otherwise = do
                let mloop !acc k !m
                        | k < switch    = kloop acc k
                        | otherwise     = do
                            val <- MG.unsafeRead small m
                            let nxtk = kmax i (m+1)
                            mloop (acc - fromIntegral (k-nxtk)*val) nxtk (m+1)
                    kloop !acc k
                        | k == 0    = do
                            MG.unsafeWrite small i $! acc
                            calcit switch change (i+1)
                        | otherwise = do
                            val <- MG.unsafeRead small (i `quot` (2*k+1))
                            kloop (acc-val) (k-1)
                mloop (fun i - fun (i `quot` 2)) ((i-1) `quot` 2) 1

    (sw, ch) <- calcit 1 8 3
    large <- MG.unsafeNew k0 :: ST s (G.Mutable v s t)

    let calcbig :: Int -> Int -> Int -> ST s (G.Mutable v s t)
        calcbig switch change j
            | j == 0    = return large
            | (2*j-1)*change <= n   = calcbig (switch+1) (change + 4*switch+6) j
            | otherwise = do
                let i = n `quot` (2*j-1)
                    mloop !acc k m
                        | k < switch    = kloop acc k
                        | otherwise     = do
                            val <- MG.unsafeRead small m
                            let nxtk = kmax i (m+1)
                            mloop (acc - fromIntegral (k-nxtk)*val) nxtk (m+1)
                    kloop !acc k
                        | k == 0    = do
                            MG.unsafeWrite large (j-1) $! acc
                            calcbig switch change (j-1)
                        | otherwise = do
                            let m = i `quot` (2*k+1)
                            val <- if m <= mk0
                                     then MG.unsafeRead small m
                                     else MG.unsafeRead large (k*(2*j-1)+j-1)
                            kloop (acc-val) (k-1)
                mloop (fun i - fun (i `quot` 2)) ((i-1) `quot` 2) 1

    mvec <- calcbig sw ch k0
    MG.unsafeRead mvec 0

