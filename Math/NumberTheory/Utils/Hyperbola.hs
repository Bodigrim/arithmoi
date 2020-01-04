-- |
-- Module:      Math.NumberTheory.Utils.Hyperbola
-- Copyright:   (c) 2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Highest points under hyperbola.
--

{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

module Math.NumberTheory.Utils.Hyperbola
  ( pointsUnderHyperbola
  ) where

import Data.Bits

import Math.NumberTheory.Roots.Cubes

-- | Straightforward computation of
-- [ n `quot` x | x <- [hi, hi - 1 .. lo] ].
-- Unfortunately, such list generator performs poor,
-- so we fall back to manual recursion.
pointsUnderHyperbola0 :: Int -> Int -> Int -> [Int]
pointsUnderHyperbola0 n lo hi
  | n < 0     = error "pointsUnderHyperbola0: first argument must be non-negative"
  | lo <= 0   = error "pointsUnderHyperbola0: second argument must be positive"
  | otherwise = go hi
    where
      go x
        | x < lo    = []
        | otherwise = n `quot` x : go (x - 1)

data Bresenham = Bresenham
  {  bresX       :: !Int
  ,  bresBeta    :: !Int
  , _bresGamma   :: !Int
  , _bresDelta1  :: !Int
  , _bresEpsilon :: !Int
  }

initBresenham :: Int -> Int -> Bresenham
initBresenham n x = Bresenham x beta gamma delta1 epsilon
  where
    beta    = n `quot` x
    epsilon = n `rem` x
    delta1  = n `quot` (x - 1) - beta
    gamma   = beta - (x - 1) * delta1

-- | bresenham(x+1) -> bresenham(x) for x >= (2n)^1/3
stepBack :: Bresenham -> Bresenham
stepBack (Bresenham x' beta' gamma' delta1' epsilon') =
  if eps >= x
    then (if eps >= x `shiftL` 1
      then {- delta2 = 2 -}
        let delta1 = delta1' + 2 in (Bresenham x (beta' + delta1) (gamma' + delta1 `shiftL` 1 - x `shiftL` 1) delta1 (eps - x `shiftL` 1))
      else {- delta1 = 1 -}
        let delta1 = delta1' + 1 in (Bresenham x (beta' + delta1) (gamma' + delta1 `shiftL` 1 - x) delta1 (eps - x))
      )
    else (if eps >= 0
      then {- delta2 =  0 -}
        (Bresenham x (beta' + delta1') (gamma' + delta1' `shiftL` 1) delta1' eps)
      else {- delta2 = -1 -}
        let delta1 = delta1' - 1 in (Bresenham x (beta' + delta1) (gamma' + delta1 `shiftL` 1 + x) delta1 (eps + x)))
  where
    x       = x' - 1
    eps     = epsilon' + gamma'
{-# INLINE stepBack #-}

-- | Division-free computation of
-- [ n `quot` x | x <- [hi, hi - 1 .. lo] ].
-- In other words, we compute y-coordinates of highest integral points
-- under hyperbola @x * y = n@ between @x = lo@ and @x = hi@ in reverse order.
--
-- The implementation follows section 5 of <https://arxiv.org/pdf/1206.3369.pdf A successive approximation algorithm for computing the divisor summatory function>
-- by R. Sladkey.
-- It is 2x faster than a trivial implementation for 'Int'.
pointsUnderHyperbola :: Int -> Int -> Int -> [Int]
pointsUnderHyperbola n lo hi
  | n < 0        = error "pointsUnderHyperbola: first argument must be non-negative"
  | lo <= 0      = error "pointsUnderHyperbola: second argument must be positive"
  | hi <  lo     = []
  | hi == lo     = [n `quot` lo]
  | otherwise    = go (initBresenham n hi)
  where
    mid = (integerCubeRoot (2 * n) + 1) `max` lo
    go h
      | bresX h < mid = pointsUnderHyperbola0 n lo ((mid - 1) `min` hi)
      | otherwise = bresBeta h : go (stepBack h)
