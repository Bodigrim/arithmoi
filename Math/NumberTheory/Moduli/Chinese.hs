-- |
-- Module:      Math.NumberTheory.Moduli.Chinese
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Chinese remainder theorem
--
{-# LANGUAGE BangPatterns #-}

module Math.NumberTheory.Moduli.Chinese
  ( chineseRemainders
  , chineseRemainders2
  , chineseRemainder
  , chineseRemainder2
  )
where

import Data.Ratio (numerator, denominator)
import Control.Monad (foldM)

import GHC.Integer.GMP.Internals

import Math.NumberTheory.GCD (extendedGCD)
import Math.NumberTheory.Moduli.Class
import Math.NumberTheory.Primes.Factorisation (factorise)

-- [Unsure where to put these.  Does arithmoi keep QuickCheck tests in separate documents?
--
-- The two tests are slightly different.  The test for the binary function compares to the
-- result of an exhaustive search.  The test for the list function merely tests that the results
-- are consistent with the requirements as exhaustive search over the LCM of the moduli of long
-- lists tends to take an unreasonable amount of time.
--
-- import Test.QuickCheck
-- import Data.List (foldl')

-- prop_chineseRemainders :: [(Integer, Positive Integer)] -> Bool
-- prop_chineseRemainders rms
--   | Just (SomeMod d) <- c = getMod d == l && all (\ (r, Positive m) -> (getVal d-r) `mod` m==0) rms
--   | otherwise             = or [(r1-r2) `mod` g /= 0|((r1, Positive m1):rms2)<-tails rms, (r2, Positive m2)<-rms2, let g = gcd m1 m2]
--   where
--     l = foldl' lcm 1 $ map (getPositive.snd) rms
--     c = chineseRemainders $ [r `modulo` fromIntegral m|(r,Positive m)<-rms]

-- prop_chineseRemainders2 :: Integer -> Positive Integer -> Integer -> Positive Integer -> Bool
-- prop_chineseRemainders2 xv (Positive xm) yv (Positive ym)
--   | Just (SomeMod d) <- c = getMod d == l && [getVal d] == sols
--   | otherwise             = null sols
--   where
--     l = lcm xm ym
--     c = chineseRemainders2 (xv `modulo` fromIntegral xm) (yv `modulo` fromIntegral ym)
--     sols = [i|i<-[0..l-1],(i-xv) `mod` xm==0,(i-yv) `mod` ym==0]




{-# DEPRECATED chineseRemainder, chineseRemainder2 "Consider switching to the more general and correct *remainders* (note the terminal S) functions" #-}

-- | Given a list @[r_1 `modulo` m_1, ..., r_n `modulo` m_n)]@ of @SomeMod@
--   pairs, @chineseRemainders@ calculates the intersection between all the
--   congruence classes represented by the @SomeMod@.
--
--   This result may be another congruence class @Just (r `modulo` n)@ if all
--   congruence classes have a non-empty intersection, @Nothing@ if they do not
--   (such as may, or may not, happen if the moduli are not pairwise co-prime),
--   or a specific rational number @Just (InfMod r)@, if it was one of the parameters
--   and is also a member of all of the other congruence classes.
--
--   n.b. The result will always be @Nothing@ if there are two distinct @InfMod@
--   parameters (they do not intersect) or if a parameter is a non-integral @InfMod@
--   and at least one other is a @SomeMod@ (they too do not intersect).
--
--   On an empty parameter list, @chineseRemainders@ returns @Just (0 `modulo` 1)@, the
--   congruence class of all integers.

chineseRemainders :: [SomeMod] -> Maybe SomeMod
chineseRemainders (x:xs) = foldM chineseRemainders2 x xs
chineseRemainders _ = Just (0 `modulo` 1)

-- | Given a pair of @SomeMod@, @chineseRemainders2@ determines their intersection.
--   if any.  This function is the underlying worker for @chineseRemainders@.

chineseRemainders2 :: SomeMod -> SomeMod -> Maybe SomeMod
chineseRemainders2 xsm@(SomeMod x) ysm@(SomeMod y)
  | xm == 1               = Just ysm
  | ym == 1               = Just xsm
  | Just (SomeMod j) <- i = Just $ (xv+(yv-xv)*xm*getVal j) `modulo` (xn*yn)
  | (xv-yv) `mod` gm == 0 = chineseRemainders2 (xv `modulo` xq) (yv `modulo` yq)
  | otherwise             = Nothing
  where
    xv = getVal x
    xm = getMod x
    xn = getNatMod x

    yv = getVal y
    ym = getMod y
    yn = getNatMod y

    i = invertSomeMod (xm `modulo` yn)

    gn = gcd xn yn
    gm = fromIntegral gn

    (xq, yq) = foldr distribute (xn `div` gn, yn `div` gn) $ factorise gm
    distribute (p',a) (xo, yo)
      | xo `mod` p == 0 = (xo*p^a, yo)
      | otherwise       = (xo, yo*p^a)
      where
        p = fromIntegral p'

chineseRemainders2 x@(SomeMod x') y@(InfMod y')
  | denominator y' /= 1                       = Nothing
  | numerator y' `mod` getMod x' == getVal x' = Just y
  | otherwise                                 = Nothing

chineseRemainders2 x@(InfMod _) y@(SomeMod _) = chineseRemainders2 y x

chineseRemainders2 x y
  | x == y    = Just x
  | otherwise = Nothing


-- | Given a list @[(r_1,m_1), ..., (r_n,m_n)]@ of @(residue,modulus)@
--   pairs, @chineseRemainder@ calculates the solution to the simultaneous
--   congruences
--
-- >
-- > r ≡ r_k (mod m_k)
-- >
--
--   if all moduli are positive and pairwise coprime. Otherwise
--   the result is @Nothing@ regardless of whether
--   a solution exists.
--
--   n.b. the @chineseRemainders@ and @chineseRemainders2@ (note the terminal S) functions
--   will find a solution in this case, if one exist.
chineseRemainder :: [(Integer,Integer)] -> Maybe Integer
chineseRemainder remainders = foldM addRem 0 remainders
  where
    !modulus = product (map snd remainders)
    addRem acc (_,1) = Just acc
    addRem acc (r,m) = do
        let cf = modulus `quot` m
        inv <- recipMod cf m
        Just $! (acc + inv*cf*r) `mod` modulus

-- | @chineseRemainder2 (r_1,m_1) (r_2,m_2)@ calculates the solution of
--
-- >
-- > r ≡ r_k (mod m_k)
--
--   if @m_1@ and @m_2@ are coprime.
chineseRemainder2 :: (Integer,Integer) -> (Integer,Integer) -> Integer
chineseRemainder2 (r1, md1) (r2,md2)
    = case extendedGCD md1 md2 of
        (_,u,v) -> ((1 - u*md1)*r1 + (1 - v*md2)*r2) `mod` (md1*md2)

recipMod :: Integer -> Integer -> Maybe Integer
recipMod x m = case recipModInteger x m of
  0 -> Nothing
  y -> Just y
