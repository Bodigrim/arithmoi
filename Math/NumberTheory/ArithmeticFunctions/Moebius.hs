-- |
-- Module:      Math.NumberTheory.ArithmeticFunctions.Moebius
-- Copyright:   (c) 2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Values of <https://en.wikipedia.org/wiki/Möbius_function Möbius function>.
--

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Math.NumberTheory.ArithmeticFunctions.Moebius
  ( Moebius(..)
  , runMoebius
  ) where

import Control.Monad (liftM)
import Data.Int
import Data.Semigroup
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed         as U
import GHC.Exts
import GHC.Integer.GMP.Internals

-- | Represents three possible values of <https://en.wikipedia.org/wiki/Möbius_function Möbius function>.
--
-- 'U.Unbox' instance is designed to be used with 'Math.NumberTheory.ArithmeticFunctions.SieveBlock.sieveBlockUnboxed'.
data Moebius
  = MoebiusN -- ^ −1
  | MoebiusZ -- ^  0
  | MoebiusP -- ^  1
  deriving (Eq, Ord, Show)

-- | Convert to any numeric type.
runMoebius :: Num a => Moebius -> a
runMoebius m = fromInteger (S# (dataToTag# m -# 1#))

fromMoebius :: Moebius -> Int8
fromMoebius m = fromIntegral $ I# (dataToTag# m)
{-# INLINE fromMoebius #-}

toMoebius :: Int8 -> Moebius
toMoebius i = let !(I# i#) = fromIntegral i in tagToEnum# i#
{-# INLINE toMoebius #-}

newtype instance U.MVector s Moebius = MV_Moebius (P.MVector s Int8)
newtype instance U.Vector    Moebius = V_Moebius  (P.Vector    Int8)

instance U.Unbox Moebius

instance M.MVector U.MVector Moebius where
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
  basicLength (MV_Moebius v) = M.basicLength v
  basicUnsafeSlice i n (MV_Moebius v) = MV_Moebius $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Moebius v1) (MV_Moebius v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Moebius `liftM` M.basicUnsafeNew n
  basicInitialize (MV_Moebius v) = M.basicInitialize v
  basicUnsafeReplicate n x = MV_Moebius `liftM` M.basicUnsafeReplicate n (fromMoebius x)
  basicUnsafeRead (MV_Moebius v) i = toMoebius `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Moebius v) i x = M.basicUnsafeWrite v i (fromMoebius x)
  basicClear (MV_Moebius v) = M.basicClear v
  basicSet (MV_Moebius v) x = M.basicSet v (fromMoebius x)
  basicUnsafeCopy (MV_Moebius v1) (MV_Moebius v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Moebius v1) (MV_Moebius v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Moebius v) n = MV_Moebius `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector Moebius where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Moebius v) = V_Moebius `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Moebius v) = MV_Moebius `liftM` G.basicUnsafeThaw v
  basicLength (V_Moebius v) = G.basicLength v
  basicUnsafeSlice i n (V_Moebius v) = V_Moebius $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Moebius v) i = toMoebius `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Moebius mv) (V_Moebius v) = G.basicUnsafeCopy mv v
  elemseq _ = seq

instance Semigroup Moebius where
  MoebiusZ <> _ = MoebiusZ
  _ <> MoebiusZ = MoebiusZ
  MoebiusP <> a = a
  a <> MoebiusP = a
  _ <> _ = MoebiusP

instance Monoid Moebius where
  mempty  = MoebiusP
  mappend = (<>)
