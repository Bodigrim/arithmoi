module Math.NumberTheory.Utils
  ( stripHighestPower
  , stripHighestPowerOf2
  ) where

import Data.Euclidean (GcdDomain, divide)
import Math.NumberTheory.Utils.Internal ( splitOff, shiftToOddCount )
import Data.Semiring (one, zero)
import Data.Maybe (isJust)

-- | Find out which highest power of the first argument
-- divides the second argument and return the result of division.
-- The first argument must be non-zero and non-unit.
--
-- >>> stripHighestPower 2 60
-- (2,15)
-- >>> stripHighestPower 3 (-108)
-- (3,-4)
-- >>> stripHighestPower 10 1000
-- (3,1)
-- >>> stripHighestPower (-10) 1000
-- (3,-1)
--
stripHighestPower :: (Eq a, GcdDomain a) => a -> a -> (Word, a)
stripHighestPower d n
  | d == zero = (0, n)
  | isJust (one `divide` d) = (0, n)
  | otherwise = splitOff d n
{-# INLINABLE stripHighestPower #-}

-- | Find out which highest power of 2 divides the argument
-- and return the result of division.
--
-- This is effectively 'stripHighestPower' specialised to 2,
-- but also with a different constraint on the type of arguments,
-- 'Integral' instead of 'GcdDomain'.
--
-- >>> stripHighestPowerOf2 60
-- (2,15)
-- >>> stripHighestPowerOf2 (-32)
-- (5,-1)
stripHighestPowerOf2 :: Integral a => a -> (Word, a)
stripHighestPowerOf2 = shiftToOddCount
{-# INLINABLE stripHighestPowerOf2 #-}
