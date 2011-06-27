-- |
-- Module:          Math.NumberTheory.Primes.Sieve.Eratosthenes
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Sieve
--
{-# LANGUAGE CPP, BangPatterns #-}
module Math.NumberTheory.Primes.Sieve.Eratosthenes
    ( primes
    ) where

#include "MachDeps.h"

import Control.Monad.ST
import Data.Array.Base (unsafeRead, unsafeWrite, unsafeAt, unsafeNewArray_)
import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad (when)
import Data.Bits
import Data.Word

import Math.NumberTheory.Powers.Squares (integerSquareRoot)
import Math.NumberTheory.Utils

-- Sieve in 128K chunks.
-- Large enough to get something done per chunk
-- and hopefully small enough to fit in the cache.
sieveBytes :: Int
sieveBytes = 128*1024

-- Number of bits per chunk.
sieveBits :: Int
sieveBits = 8*sieveBytes

-- Last index of chunk.
lastIndex :: Int
lastIndex = sieveBits - 1

-- Range of a chunk.
sieveRange :: Int
sieveRange = 30*sieveBytes

sieveWords :: Int
sieveWords = sieveBytes `quot` SIZEOF_HSWORD

#if SIZEOF_HSWORD == 8
type CacheWord = Word
#define RMASK 63
#define WSHFT 6
#else
type CacheWord = Word64
#define RMASK 31
#define WSHFT 5
#endif

data PrimeSieve = PS !Integer {-# UNPACK #-} !(UArray Int Bool)

primes :: [Integer]
primes = 2:3:5:concat [[vO + fromIntegral (toPrim i) | i <- [0 .. li], unsafeAt bs i]
                                | PS vO bs <- psieveList, let (_,li) = bounds bs]

psieveList :: [PrimeSieve]
psieveList = makeSieves plim sqlim 0 0 cache
  where
    plim = 4801     -- prime #647
    sqlim = plim*plim
    cache = runSTUArray $ do
        sieve <- sieveTo 4801
        new <- unsafeNewArray_ (0,1287) :: ST s (STUArray s Int CacheWord)
        let fill j indx
              | 1279 < indx = return new
              | otherwise = do
                p <- unsafeRead sieve indx
                if p
                  then do
                    let !i = indx .&. 7
                        k :: Integer
                        k = fromIntegral (indx `shiftR` 3)
                        strt1 = (k*(30*k + fromIntegral (2*rho i))
                                    + fromIntegral (byte i)) `shiftL` 3
                                    + fromIntegral (idx i)
                        !strt = fromIntegral strt1 .&. 0xFFFFF
                        !skip = fromIntegral (strt1 `shiftR` 20)
                        !ixes = fromIntegral indx `shiftL` 23 + strt `shiftL` 3 + fromIntegral i
                    unsafeWrite new j skip
                    unsafeWrite new (j+1) ixes
                    fill (j+2) (indx+1)
                  else fill j (indx+1)
        fill 0 0

makeSieves :: Integer -> Integer -> Integer -> Integer -> UArray Int Word -> [PrimeSieve]
makeSieves plim sqlim bitOff valOff cache
  | valOff' < sqlim =
      let (nc, bs) = runST $ do
            cch <- {-# SCC "Thaw" #-} unsafeThaw cache :: ST s (STUArray s Int CacheWord)
            bs0 <- slice cch
            fcch <- {-# SCC "FreezeCache" #-} unsafeFreeze cch
            fbs0 <- {-# SCC "FreezeSieve" #-} unsafeFreeze bs0
            return (fcch, fbs0)
      in PS valOff bs : makeSieves plim sqlim bitOff' valOff' nc
  | otherwise       =
      let plim' = plim + 4800
          sqlim' = plim' * plim'
          (nc,bs) = runST $ do
            cch <- growCache bitOff plim cache
            bs0 <- slice cch
            fcch <- unsafeFreeze cch
            fbs0 <- unsafeFreeze bs0
            return (fcch, fbs0)
      in PS valOff bs : makeSieves plim' sqlim' bitOff' valOff' nc
    where
      valOff' = valOff + fromIntegral sieveRange
      bitOff' = bitOff + fromIntegral sieveBits

slice :: STUArray s Int Word -> ST s (STUArray s Int Bool)
slice cache = do
    (0,hi) <- getBounds cache
    sieve <- newArray (0,lastIndex) True
    let treat pr
          | hi < pr     = return sieve
          | otherwise   = do
            w <- unsafeRead cache pr
            if w /= 0
              then unsafeWrite cache pr (w-1)
              else do
                ixes <- unsafeRead cache (pr+1)
                let !stj = ixes .&. 0x7FFFFF
                    !ixw = ixes `shiftR` 23
                    !i = fromIntegral (ixw .&. 7)
                    !k = fromIntegral ixw - i
                    !o = i `shiftL` 3
                    !j = fromIntegral (stj .&. 7)
                    !s = fromIntegral (stj `shiftR` 3)
                (n, u) <- tick k o j s
                let !skip = fromIntegral n `shiftR` 20
                    !strt = fromIntegral n .&. 0xFFFFF
                unsafeWrite cache pr skip
                unsafeWrite cache (pr+1) (ixes - stj + strt `shiftL` 3 + fromIntegral u)
            treat (pr+2)
        tick stp off j ix
          | lastIndex < ix  = return (ix - sieveBits, j)
          | otherwise       = do
            p <- unsafeRead sieve ix
            when p (unsafeWrite sieve ix False)
            tick stp off ((j+1) .&. 7) (ix + stp*delta j + tau (off+j))
    treat 0

-- | Sieve up to bound in one go.
sieveTo :: Integer -> ST s (STUArray s Int Bool)
sieveTo bound = arr
  where
    (!bytes,!lidx) = idxPr bound
    !mxidx = 8*bytes+lidx
    mxval = 30*fromIntegral bytes + fromIntegral (rho lidx)
    !mxsve = integerSquareRoot mxval
    (kr,r) = idxPr mxsve
    !svbd = 8*kr+r
    arr = do
        ar <- newArray (0,mxidx) True
        let start k i = 8*(k*(30*k+2*rho i) + byte i) + idx i
            tick stp off j ix
              | mxidx < ix = return ()
              | otherwise  = do
                p <- unsafeRead ar ix
                when p (unsafeWrite ar ix False)
                tick stp off ((j+1) .&. 7) (ix + stp*delta j + tau (off+j))
            sift ix
              | svbd < ix = return ar
              | otherwise = do
                p <- unsafeRead ar ix
                when p  (do let i = ix .&. 7
                                k = ix `shiftR` 3
                                !off = i `shiftL` 3
                                !stp = ix - i
                            tick stp off i (start k i))
                sift (ix+1)
        sift 0

growCache :: Integer -> Integer -> UArray Int CacheWord -> ST s (STUArray s Int CacheWord)
growCache offset plim old = do
    let (0,num) = bounds old
        (bt,ix) = idxPr plim
        !start  = 8*bt+ix+1
        !nlim   = plim+4800
    sieve <- sieveTo nlim
    (_,hi) <- getBounds sieve
    more <- countFromTo start hi sieve
    new <- unsafeNewArray_ (0,num+2*more) :: ST s (STUArray s Int CacheWord)
    let copy i
          | num < i   = return ()
          | otherwise = do
            unsafeWrite new i (old `unsafeAt` i)
            copy (i+1)
    copy 0
    let fill j indx
          | hi < indx = return new
          | otherwise = do
            p <- unsafeRead sieve indx
            if p
              then do
                let !i = indx .&. 7
                    k :: Integer
                    k = fromIntegral (indx `shiftR` 3)
                    strt0 = ((k*(30*k + fromIntegral (2*rho i))
                                + fromIntegral (byte i)) `shiftL` 3)
                                    + fromIntegral (idx i)
                    strt1 = strt0 - offset
                    !strt = fromIntegral strt1 .&. 0xFFFFF
                    !skip = fromIntegral (strt1 `shiftR` 20)
                    !ixes = fromIntegral indx `shiftL` 23 + strt `shiftL` 3 + fromIntegral i
                unsafeWrite new j skip
                unsafeWrite new (j+1) ixes
                fill (j+2) (indx+1)
              else fill j (indx+1)
    fill (num+1) start

-- Danger: relies on start and end being the first resp. last
-- index in a Word
-- Do not use except in growCache
{-# INLINE countFromTo #-}
countFromTo :: Int -> Int -> STUArray s Int Bool -> ST s Int
countFromTo start end ba = do
    wa <- (castSTUArray :: STUArray s Int Bool -> ST s (STUArray s Int Word)) ba
    let !sb = start `shiftR` WSHFT
        !eb = end `shiftR` WSHFT
        count !acc i
          | eb < i    = return acc
          | otherwise = do
            w <- unsafeRead wa i
            count (acc + bitCountWord w) (i+1)
    count 0 sb

-- Auxiliary stuff, conversion between number and index,
-- remainders modulo 30 and related things.

{-# INLINE idxPr #-}
idxPr :: Integer -> (Int,Int)
idxPr n0 = (fromInteger bytes0, rm3)
  where
    n = if (fromInteger n0 .&. 1 == (1 :: Int))
            then n0 else (n0-1)
    (bytes0,rm0) = (n-7) `quotRem` 30
    rm1 = fromInteger rm0
    rm2 = rm1 `quot` 3
    rm3 = min 7 (if rm2 > 5 then rm2-1 else rm2)

{-# INLINE toPrim #-}
toPrim :: Int -> Integer
toPrim ix = 30*fromIntegral k + fromIntegral (rho i)
  where
    i = ix .&. 7
    k = ix `shiftR` 3

{-# INLINE rho #-}
rho :: Int -> Int
rho i = unsafeAt residues i

residues :: UArray Int Int
residues = listArray (0,7) [7,11,13,17,19,23,29,31]

{-# INLINE delta #-}
delta :: Int -> Int
delta i = unsafeAt deltas i

deltas :: UArray Int Int
deltas = listArray (0,7) [4,2,4,2,4,6,2,6]

{-# INLINE tau #-}
tau :: Int -> Int
tau i = unsafeAt taus i

taus :: UArray Int Int
taus = listArray (0,63)
        [  7,  4,  7,  4,  7, 12,  3, 12
        , 12,  6, 11,  6, 12, 18,  5, 18
        , 14,  7, 13,  7, 14, 21,  7, 21
        , 18,  9, 19,  9, 18, 27,  9, 27
        , 20, 10, 21, 10, 20, 30, 11, 30
        , 25, 12, 25, 12, 25, 36, 13, 36
        , 31, 15, 31, 15, 31, 47, 15, 47
        , 33, 17, 33, 17, 33, 49, 17, 49
        ]

{-# INLINE byte #-}
byte :: Int -> Int
byte i = unsafeAt startByte i

startByte :: UArray Int Int
startByte = listArray (0,7) [1,3,5,9,11,17,27,31]

{-# INLINE idx #-}
idx :: Int -> Int
idx i = unsafeAt startIdx i

startIdx :: UArray Int Int
startIdx = listArray (0,7) [4,7,4,4,7,4,7,7]
