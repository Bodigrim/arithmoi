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
  , sieveBlockMoebius
  ) where

import Control.Monad (forM_, liftM)
import Control.Monad.ST (runST)
import Data.Int
import Data.Semigroup
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as MU
import GHC.Exts
import GHC.Integer.GMP.Internals

import Math.NumberTheory.Primes (primes)
import Math.NumberTheory.Powers.Squares (integerSquareRoot)
import Math.NumberTheory.Utils.FromIntegral (wordToInt)

-- | Represents three possible values of <https://en.wikipedia.org/wiki/Möbius_function Möbius function>.
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

-- | Evaluate the Möbius function over a block.
-- Value of @f@ at 0, if zero falls into block, is undefined.
--
-- Based on the sieving algorithm from p. 2 of <https://arxiv.org/pdf/1610.08551.pdf Computations of the Mertens function and improved bounds on the Mertens conjecture> by G. Hurst. It is approximately 5x faster than 'Math.NumberTheory.ArithmeticFunctions.SieveBlock.sieveBlockUnboxed'.
--
-- > > sieveBlockMoebius 1 10
-- > [MoebiusP, MoebiusN, MoebiusN, MoebiusZ, MoebiusN, MoebiusP, MoebiusN, MoebiusZ, MoebiusZ, MoebiusP]
sieveBlockMoebius
  :: Word
  -> Word
  -> U.Vector Moebius
-- It is tempting to implement the sieving algorithm from p. 3 of the same paper,
-- but it is correct only for n < 10^16.
sieveBlockMoebius lowIndex' len' = U.imap mapper $ runST $ do
    as <- MU.replicate len 1
    forM_ ps $ \p -> do
      let offset = negate lowIndex `mod` p
          offset2 = negate lowIndex `mod` (p * p)
      forM_ [offset, offset + p .. len - 1] $ \ix -> do
        MU.unsafeModify as (\y -> negate y * p) ix
      forM_ [offset2, offset2 + p * p .. len - 1] $ \ix -> do
        MU.unsafeWrite as ix 0
    U.unsafeFreeze as

  where
    lowIndex :: Int
    lowIndex = wordToInt lowIndex'

    len :: Int
    len = wordToInt len'

    highIndex :: Int
    highIndex = lowIndex + len - 1

    ps :: [Int]
    ps = takeWhile (<= integerSquareRoot highIndex) $ map fromInteger primes

    mapper :: Int -> Int -> Moebius
    mapper _ 0 = MoebiusZ
    mapper ix val
      | val > 0, val == ix + lowIndex = MoebiusP
      | val > 0                       = MoebiusN
      | negate val == ix + lowIndex   = MoebiusN
      | otherwise                     = MoebiusP
