-- |
-- Module:      Math.NumberTheory.Primes.Types
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- This is an internal module, defining types for primes.
-- Should not be exposed to users.
--

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Math.NumberTheory.Primes.Types
  ( Prime(..)
  , toPrimeIntegral
  ) where

import Data.Bits
import GHC.Generics
import Control.DeepSeq
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as U

-- | Wrapper for prime elements of @a@. It is supposed to be constructed
-- by 'Math.NumberTheory.Primes.nextPrime' / 'Math.NumberTheory.Primes.precPrime'.
-- and eliminated by 'unPrime'.
--
-- One can leverage 'Enum' instance to generate lists of primes.
-- Here are some examples.
--
-- *  Generate primes from the given interval:
--
--    >>> [nextPrime 101 .. precPrime 130]
--    [Prime 101,Prime 103,Prime 107,Prime 109,Prime 113,Prime 127]
--
-- *  Generate an infinite list of primes:
--
--    >>> [nextPrime 101 ..]
--    [Prime 101,Prime 103,Prime 107,Prime 109,Prime 113,Prime 127...
--
-- *  Generate primes from the given interval of form p = 6k+5:
--
--    >>> [nextPrime 101, nextPrime 107 .. precPrime 150]
--    [Prime 101,Prime 107,Prime 113,Prime 131,Prime 137,Prime 149]
--
-- *  Get next prime:
--
--    >>> succ (nextPrime 101)
--    Prime 103
--
-- *  Get previous prime:
--
--    >>> prec (nextPrime 101)
--    Prime 97
--
-- *  Count primes less than a given number (cf. 'Math.NumberTheory.Primes.Counting.approxPrimeCount'):
--
--    >>> fromEnum (precPrime 100)
--    25
--
-- *  Get 25-th prime number (cf. 'Math.NumberTheory.Primes.Counting.nthPrimeApprox'):
--
--    >>> toEnum 25 :: Prime Int
--    Prime 97
--
newtype Prime a = Prime
  { unPrime :: a -- ^ Unwrap prime element.
  }
  deriving (Eq, Ord, Generic)

instance NFData a => NFData (Prime a)

instance Show a => Show (Prime a) where
  showsPrec d (Prime p) r = (if d > 10 then "(" ++ s ++ ")" else s) ++ r
    where
      s = "Prime " ++ show p

newtype instance U.MVector s (Prime a) = MV_Prime (U.MVector s a)
newtype instance U.Vector    (Prime a) = V_Prime  (U.Vector    a)

instance U.Unbox a => U.Unbox (Prime a)

instance M.MVector U.MVector a => M.MVector U.MVector (Prime a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Prime v) = M.basicLength v
  basicUnsafeSlice i n (MV_Prime v) = MV_Prime $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Prime v1) (MV_Prime v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Prime <$> M.basicUnsafeNew n
  basicInitialize (MV_Prime v) = M.basicInitialize v
  basicUnsafeReplicate n x = MV_Prime <$> M.basicUnsafeReplicate n (unPrime x)
  basicUnsafeRead (MV_Prime v) i = Prime <$> M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Prime v) i x = M.basicUnsafeWrite v i (unPrime x)
  basicClear (MV_Prime v) = M.basicClear v
  basicSet (MV_Prime v) x = M.basicSet v (unPrime x)
  basicUnsafeCopy (MV_Prime v1) (MV_Prime v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Prime v1) (MV_Prime v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Prime v) n = MV_Prime <$> M.basicUnsafeGrow v n

instance G.Vector U.Vector a => G.Vector U.Vector (Prime a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Prime v) = V_Prime <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Prime v) = MV_Prime <$> G.basicUnsafeThaw v
  basicLength (V_Prime v) = G.basicLength v
  basicUnsafeSlice i n (V_Prime v) = V_Prime $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Prime v) i = Prime <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Prime mv) (V_Prime v) = G.basicUnsafeCopy mv v
  elemseq _ = seq

-- | Convert between primes of different types, similar in spirit to 'toIntegralSized'.
--
-- A simpler version of this function is:
--
-- > toPrimeIntegral :: (Integral a, Integral b) => a -> Maybe b
-- > toPrimeIntegral (Prime a)
-- >   | toInteger a == b = Just (Prime (fromInteger b))
-- >   | otherwise        = Nothing
-- >   where
-- >     b = toInteger a
--
-- The point of 'toPrimeIntegral' is to avoid redundant conversions and conditions,
-- when it is safe to do so, determining type sizes statically with 'bitSizeMaybe'.
-- For example, 'toPrimeIntegral' from 'Prime' 'Int' to 'Prime' 'Word' boils down to
-- 'Just' . 'fromIntegral'.
--
toPrimeIntegral :: (Integral a, Integral b, Bits a, Bits b) => Prime a -> Maybe (Prime b)
toPrimeIntegral (Prime a) = case unsignedWidth b of
  Nothing -> res
  Just bW -> case unsignedWidth a of
    Just aW
      | aW <= bW -> res
    _
      | a <= bit bW - 1 -> res
      | otherwise       -> Nothing
  where
    b = fromIntegral a
    res = Just (Prime b)
{-# INLINE toPrimeIntegral #-}

unsignedWidth :: Bits a => a -> Maybe Int
unsignedWidth t
  | isSigned t = subtract 1 <$> bitSizeMaybe t
  | otherwise  =                bitSizeMaybe t
{-# INLINE unsignedWidth #-}
