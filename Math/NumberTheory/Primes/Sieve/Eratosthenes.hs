-- |
-- Module:      Math.NumberTheory.Primes.Sieve.Eratosthenes
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Sieve
--
{-# LANGUAGE CPP, BangPatterns, FlexibleContexts #-}
{-# OPTIONS_GHC -fspec-constr-count=8 #-}
{-# OPTIONS_HADDOCK hide #-}
module Math.NumberTheory.Primes.Sieve.Eratosthenes
    ( primes
    , sieveFrom
    , psieveFrom
    , PrimeSieve(..)
    , psieveList
    , primeList
    , primeSieve
    , nthPrimeCt
    , countFromTo
    , countAll
    , countToNth
    , sieveBytes
    , sieveBits
    , sieveWords
    , sieveRange
    , sieveTo
    ) where

#include "MachDeps.h"

import Control.Monad.ST
import Data.Array.ST
import Control.Monad (when)
import Data.Bits
#if __GLASGOW_HASKELL__ < 709 || WORD_SIZE_IN_BITS == 32
import Data.Word
#endif

import Math.NumberTheory.Powers.Squares (integerSquareRoot)
import Math.NumberTheory.Unsafe
import Math.NumberTheory.Utils
import Math.NumberTheory.Primes.Counting.Approximate
import Math.NumberTheory.Primes.Sieve.Indexing

#define IX_MASK     0xFFFFF
#define IX_BITS     20
#define IX_J_MASK   0x7FFFFF
#define IX_J_BITS   23
#define J_MASK      7
#define J_BITS      3
#define SIEVE_KB    128

-- Sieve in 128K chunks.
-- Large enough to get something done per chunk
-- and hopefully small enough to fit in the cache.
sieveBytes :: Int
sieveBytes = SIEVE_KB*1024

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
#define TOPB 32
#define TOPM 0xFFFFFFFF
#else
type CacheWord = Word64
#define RMASK 31
#define WSHFT 5
#define TOPB 16
#define TOPM 0xFFFF
#endif

-- | Compact store of primality flags.
data PrimeSieve = PS !Integer {-# UNPACK #-} !(UArray Int Bool)

-- | Sieve primes up to (and including) a bound (or 7, if bound is smaller).
--   For small enough bounds, this is more efficient than
--   using the segmented sieve.
--
--   Since arrays are 'Int'-indexed, overflow occurs when the sieve
--   size comes near @'maxBound' :: 'Int'@, that corresponds to an
--   upper bound near @15/8*'maxBound'@. On @32@-bit systems, that
--   is often within memory limits, so don't give bounds larger than
--   @8*10^9@ there.
primeSieve :: Integer -> PrimeSieve
primeSieve bound = PS 0 (runSTUArray $ sieveTo bound)

-- | Generate a list of primes for consumption from a
--   'PrimeSieve'.
primeList :: PrimeSieve -> [Integer]
primeList (PS 0 bs) = 2:3:5:[toPrim i | let (lo,hi) = bounds bs
                                      , i <- [lo .. hi]
                                      , unsafeAt bs i
                                      ]
primeList (PS vO bs) = [vO + toPrim i
                            | let (lo,hi) = bounds bs
                            , i <- [lo .. hi]
                            , unsafeAt bs i
                            ]

-- | List of primes.
--   Since the sieve uses unboxed arrays, overflow occurs at some point.
--   On 64-bit systems, that point is beyond the memory limits, on
--   32-bit systems, it is at about @1.7*10^18@.
primes :: [Integer]
primes = 2:3:5:concat [[vO + toPrim i | i <- [0 .. li], unsafeAt bs i]
                                | PS vO bs <- psieveList, let (_,li) = bounds bs]

-- | List of primes in the form of a list of 'PrimeSieve's, more compact than
--   'primes', thus it may be better to use @'psieveList' >>= 'primeList'@
--   than keeping the list of primes alive during the entire run.
psieveList :: [PrimeSieve]
psieveList = makeSieves plim sqlim 0 0 cache
  where
    plim = 4801     -- prime #647, 644 of them to use
    sqlim = plim*plim
    cache = runSTUArray $ do
        sieve <- sieveTo 4801
        new <- unsafeNewArray_ (0,1287) :: ST s (STUArray s Int CacheWord)
        let fill j indx
              | 1279 < indx = return new    -- index of 4801 = 159*30 + 31 ~> 159*8+7
              | otherwise = do
                p <- unsafeRead sieve indx
                if p
                  then do
                    let !i = indx .&. J_MASK
                        k = indx `shiftR` J_BITS
                        strt1 = (k*(30*k + 2*rho i) + byte i) `shiftL` J_BITS + fromIntegral (idx i)
                        !strt = fromIntegral (strt1 .&. IX_MASK)
                        !skip = fromIntegral (strt1 `shiftR` IX_BITS)
                        !ixes = fromIntegral indx `shiftL` IX_J_BITS + strt `shiftL` J_BITS + fromIntegral i
                    unsafeWrite new j skip
                    unsafeWrite new (j+1) ixes
                    fill (j+2) (indx+1)
                  else fill j (indx+1)
        fill 0 0

makeSieves :: Integer -> Integer -> Integer -> Integer -> UArray Int CacheWord -> [PrimeSieve]
makeSieves plim sqlim bitOff valOff cache
  | valOff' < sqlim =
      let (nc, bs) = runST $ do
            cch <- unsafeThaw cache :: ST s (STUArray s Int CacheWord)
            bs0 <- slice cch
            fcch <- unsafeFreeze cch
            fbs0 <- unsafeFreeze bs0
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

slice :: STUArray s Int CacheWord -> ST s (STUArray s Int Bool)
slice cache = do
    hi <- snd `fmap` getBounds cache
    sieve <- newArray (0,lastIndex) True
    let treat pr
          | hi < pr     = return sieve
          | otherwise   = do
            w <- unsafeRead cache pr
            if w /= 0
              then unsafeWrite cache pr (w-1)
              else do
                ixes <- unsafeRead cache (pr+1)
                let !stj = fromIntegral ixes .&. IX_J_MASK   -- position of multiple and index of cofactor
                    !ixw = fromIntegral (ixes `shiftR` IX_J_BITS)  -- prime data, up to 41 bits
                    !i = ixw .&. J_MASK
                    !k = ixw - i        -- On 32-bits, k > 44717396 means overflow is possible in tick
                    !o = i `shiftL` J_BITS
                    !j = stj .&. J_MASK          -- index of cofactor
                    !s = stj `shiftR` J_BITS     -- index of first multiple to tick off
                (n, u) <- tick k o j s
                let !skip = fromIntegral (n `shiftR` IX_BITS)
                    !strt = fromIntegral (n .&. IX_MASK)
                unsafeWrite cache pr skip
                unsafeWrite cache (pr+1) ((ixes .&. complement IX_J_MASK) .|. strt `shiftL` J_BITS .|. fromIntegral u)
            treat (pr+2)
        tick stp off j ix
          | lastIndex < ix  = return (ix - sieveBits, j)
          | otherwise       = do
            p <- unsafeRead sieve ix
            when p (unsafeWrite sieve ix False)
            tick stp off ((j+1) .&. J_MASK) (ix + stp*delta j + tau (off+j))
    treat 0

-- | Sieve up to bound in one go.
sieveTo :: Integer -> ST s (STUArray s Int Bool)
sieveTo bound = arr
  where
    (bytes,lidx) = idxPr bound
    !mxidx = 8*bytes+lidx
    mxval :: Integer
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
                tick stp off ((j+1) .&. J_MASK) (ix + stp*delta j + tau (off+j))
            sift ix
              | svbd < ix = return ar
              | otherwise = do
                p <- unsafeRead ar ix
                when p  (do let i = ix .&. J_MASK
                                k = ix `shiftR` J_BITS
                                !off = i `shiftL` J_BITS
                                !stp = ix - i
                            tick stp off i (start k i))
                sift (ix+1)
        sift 0

growCache :: Integer -> Integer -> UArray Int CacheWord -> ST s (STUArray s Int CacheWord)
growCache offset plim old = do
    let (_,num) = bounds old
        (bt,ix) = idxPr plim
        !start  = 8*bt+ix+1
        !nlim   = plim+4800
    sieve <- sieveTo nlim       -- Implement SieveFromTo for this, it's pretty wasteful when nlim isn't
    (_,hi) <- getBounds sieve   -- very small anymore
    more <- countFromToWd start hi sieve
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
                let !i = indx .&. J_MASK
                    k :: Integer
                    k = fromIntegral (indx `shiftR` J_BITS)
                    strt0 = ((k*(30*k + fromIntegral (2*rho i))
                                + fromIntegral (byte i)) `shiftL` J_BITS)
                                    + fromIntegral (idx i)
                    strt1 = strt0 - offset
                    !strt = fromIntegral strt1 .&. IX_MASK
                    !skip = fromIntegral (strt1 `shiftR` IX_BITS)
                    !ixes = fromIntegral indx `shiftL` IX_J_BITS .|. strt `shiftL` J_BITS .|. fromIntegral i
                unsafeWrite new j skip
                unsafeWrite new (j+1) ixes
                fill (j+2) (indx+1)
              else fill j (indx+1)
    fill (num+1) start

-- Danger: relies on start and end being the first resp. last
-- index in a Word
-- Do not use except in growCache and psieveFrom
{-# INLINE countFromToWd #-}
countFromToWd :: Int -> Int -> STUArray s Int Bool -> ST s Int
countFromToWd start end ba = do
    wa <- (castSTUArray :: STUArray s Int Bool -> ST s (STUArray s Int Word)) ba
    let !sb = start `shiftR` WSHFT
        !eb = end `shiftR` WSHFT
        count !acc i
          | eb < i    = return acc
          | otherwise = do
            w <- unsafeRead wa i
            count (acc + bitCountWord w) (i+1)
    count 0 sb

-- count set bits between two indices (inclusive)
-- start and end must both be valid indices and start <= end
countFromTo :: Int -> Int -> STUArray s Int Bool -> ST s Int
countFromTo start end ba = do
    wa <- (castSTUArray :: STUArray s Int Bool -> ST s (STUArray s Int Word)) ba
    let !sb = start `shiftR` WSHFT
        !si = start .&. RMASK
        !eb = end `shiftR` WSHFT
        !ei = end .&. RMASK
        count !acc i
            | i == eb = do
                w <- unsafeRead wa i
                return (acc + bitCountWord (w `shiftL` (RMASK - ei)))
            | otherwise = do
                w <- unsafeRead wa i
                count (acc + bitCountWord w) (i+1)
    if sb < eb
      then do
          w <- unsafeRead wa sb
          count (bitCountWord (w `shiftR` si)) (sb+1)
      else do
          w <- unsafeRead wa sb
          let !w1 = w `shiftR` si
          return (bitCountWord (w1 `shiftL` (RMASK - ei + si)))

-- | @'sieveFrom' n@ creates the list of primes not less than @n@.
sieveFrom :: Integer -> [Integer]
sieveFrom n = case psieveFrom n of
                        ps -> dropWhile (< n) (ps >>= primeList)

-- | @'psieveFrom' n@ creates the list of 'PrimeSieve's starting roughly
--   at @n@. Due to the organisation of the sieve, the list may contain
--   a few primes less than @n@.
--   This form uses less memory than @['Integer']@, hence it may be preferable
--   to use this if it is to be reused.
psieveFrom :: Integer -> [PrimeSieve]
psieveFrom n = makeSieves plim sqlim bitOff valOff cache
    where
      k0 = max 0 (n-7) `quot` 30
      valOff = 30*k0
      bitOff = 8*k0
      start = valOff+7
      ssr = integerSquareRoot (start-1) + 1
      end1 = start - 6 + fromIntegral sieveRange
      plim0 = integerSquareRoot end1
      plim = plim0 + 4801 - (plim0 `rem` 4800)
      sqlim = plim*plim
      cache = runSTUArray $ do
          sieve <- sieveTo plim
          (lo,hi) <- getBounds sieve
          pct <- countFromToWd lo hi sieve
          new <- unsafeNewArray_ (0,2*pct-1) ::  ST s (STUArray s Int CacheWord)
          let fill j indx
                | hi < indx = return new
                | otherwise = do
                  isPr <- unsafeRead sieve indx
                  if isPr
                    then do
                      let !i = indx .&. J_MASK
                          !moff = i `shiftL` J_BITS
                          k :: Integer
                          k = fromIntegral (indx `shiftR` J_BITS)
                          p = 30*k+fromIntegral (rho i)
                          q0 = (start-1) `quot` p
                          (skp0,q1) = q0 `quotRem` fromIntegral sieveRange
                          (b0,r0)
                              | q1 == 0   = (-1,6)
                              | q1 < 7    = (-1,7)
                              | otherwise = idxPr (fromIntegral q1 :: Int)
                          (b1,r1) | r0 == 7 = (b0+1,0)
                                  | otherwise = (b0,r0+1)
                          b2 = skp0*fromIntegral sieveBytes + fromIntegral b1
                          strt0 = ((k*(30*b2 + fromIntegral (rho r1))
                                        + b2 * fromIntegral (rho i)
                                        + fromIntegral (mu (moff + r1))) `shiftL` J_BITS)
                                            + fromIntegral (nu (moff + r1))
                          strt1 = ((k*(30*k + fromIntegral (2*rho i))
                                      + fromIntegral (byte i)) `shiftL` J_BITS)
                                          + fromIntegral (idx i)
                          (strt2,r2)
                              | p < ssr   = (strt0 - bitOff,r1)
                              | otherwise = (strt1 - bitOff, i)
                          !strt = fromIntegral strt2 .&. IX_MASK
                          !skip = fromIntegral (strt2 `shiftR` IX_BITS)
                          !ixes = fromIntegral indx `shiftL` IX_J_BITS .|. strt `shiftL` J_BITS .|. fromIntegral r2
                      unsafeWrite new j skip
                      unsafeWrite new (j+1) ixes
                      fill (j+2) (indx+1)
                    else fill j (indx+1)
          fill 0 0

-- prime counting

nthPrimeCt :: Integer -> Integer
nthPrimeCt 1      = 2
nthPrimeCt 2      = 3
nthPrimeCt 3      = 5
nthPrimeCt 4      = 7
nthPrimeCt 5      = 11
nthPrimeCt 6      = 13
nthPrimeCt n
  | n < 1       = error "nthPrimeCt: negative argument"
  | n < 200000  = let bd0 = nthPrimeApprox n
                      bnd = bd0 + bd0 `quot` 32 + 37
                      !sv = primeSieve bnd
                  in countToNth (n-3) [sv]
  | otherwise   = countToNth (n-3) (psieveFrom (fromIntegral $ fromInteger n .&. (7 :: Int)))

-- find the n-th set bit in a list of PrimeSieves,
-- aka find the (n+3)-rd prime
countToNth :: Integer -> [PrimeSieve] -> Integer
countToNth !n ps = runST (countDown n ps)

countDown :: Integer -> [PrimeSieve] -> ST s Integer
countDown !n (ps@(PS v0 bs) : more)
  | n > 278734 || (v0 /= 0 && n > 253000) = do
    ct <- countAll ps
    countDown (n - fromIntegral ct) more
  | otherwise = do
    stu <- unsafeThaw bs
    wa <- (castSTUArray :: STUArray s Int Bool -> ST s (STUArray s Int Word)) stu
    let go !k i
          | i == sieveWords  = countDown k more
          | otherwise   = do
            w <- unsafeRead wa i
            let !bc = fromIntegral $ bitCountWord w
            if bc < k
                then go (k-bc) (i+1)
                else let !j = fromIntegral (bc - k)
                         !px = top w j (fromIntegral bc)
                     in return (v0 + toPrim (px+(i `shiftL` WSHFT)))
    go n 0
countDown _ [] = error "Prime stream ended prematurely"

-- count all set bits in a chunk, do it wordwise for speed.
countAll :: PrimeSieve -> ST s Int
countAll (PS _ bs) = do
    stu <- unsafeThaw bs
    wa <- (castSTUArray :: STUArray s Int Bool -> ST s (STUArray s Int Word)) stu
    let go !ct i
            | i == sieveWords = return ct
            | otherwise = do
                w <- unsafeRead wa i
                go (ct + bitCountWord w) (i+1)
    go 0 0

-- Find the j-th highest of bc set bits in the Word w.
top :: Word -> Int -> Int -> Int
top w j bc = go 0 TOPB TOPM bn w
    where
      !bn = bc-j
      go !_ _ !_ !_ 0 = error "Too few bits set"
      go bs 0 _ _ wd = if wd .&. 1 == 0 then error "Too few bits, shift 0" else bs
      go bs a msk ix wd =
        case bitCountWord (wd .&. msk) of
          lc | lc < ix  -> go (bs+a) a msk (ix-lc) (wd `uncheckedShiftR` a)
             | otherwise ->
               let !na = a `shiftR` 1
               in go bs na (msk `uncheckedShiftR` na) ix wd
