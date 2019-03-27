-- |
-- Module:      Math.NumberTheory.Primes.Sieve.Eratosthenes
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
--
-- Sieve
--
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    , sieveBits
    , sieveRange
    , sieveTo
    ) where

#include "MachDeps.h"

import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Coerce
import Data.Proxy
import Control.Monad (when)
import Data.Bits
#if WORD_SIZE_IN_BITS == 32
import Data.Word
#endif

import Math.NumberTheory.Powers.Squares (integerSquareRoot)
import Math.NumberTheory.Unsafe
import Math.NumberTheory.Utils
import Math.NumberTheory.Utils.FromIntegral
import Math.NumberTheory.Primes.Counting.Approximate
import Math.NumberTheory.Primes.Sieve.Indexing
import Math.NumberTheory.Primes.Types

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
primeList :: forall a. Integral a => PrimeSieve -> [Prime a]
primeList ps@(PS v _)
  | doesNotFit (Proxy :: Proxy a) v
              = [] -- has an overflow already happened?
  | v == 0    = (coerce :: [a] -> [Prime a])
              $ takeWhileIncreasing $ 2 : 3 : 5 : primeListInternal ps
  | otherwise = (coerce :: [a] -> [Prime a])
              $ takeWhileIncreasing $ primeListInternal ps

primeListInternal :: Num a => PrimeSieve -> [a]
primeListInternal (PS v0 bs)
  = map ((+ fromInteger v0) . toPrim)
  $ filter (unsafeAt bs) [lo..hi]
  where
    (lo, hi) = bounds bs

-- | Returns true if integer is beyond representation range of type a.
doesNotFit :: forall a. Integral a => Proxy a -> Integer -> Bool
doesNotFit _ v = toInteger (fromInteger v :: a) /= v

-- | Extracts the longest strictly increasing prefix of the list
-- (possibly infinite).
takeWhileIncreasing :: Ord a => [a] -> [a]
takeWhileIncreasing = \case
  []     -> []
  x : xs -> x : foldr go (const []) xs x
    where
      go :: Ord a => a -> (a -> [a]) -> a -> [a]
      go y f z = if z < y then y : f y else []

-- | Ascending list of primes.
--
-- >>> take 10 primes
-- [Prime 2,Prime 3,Prime 5,Prime 7,Prime 11,Prime 13,Prime 17,Prime 19,Prime 23,Prime 29]
--
-- 'primes' is a polymorphic list, so the results of computations are not retained in memory.
-- Make it monomorphic to take advantages of memoization. Compare
--
-- >>> :set +s
-- >>> primes !! 1000000 :: Prime Int
-- Prime 15485867
-- (5.32 secs, 6,945,267,496 bytes)
-- >>> primes !! 1000000 :: Prime Int
-- Prime 15485867
-- (5.19 secs, 6,945,267,496 bytes)
--
-- against
--
-- >>> let primes' = primes :: [Prime Int]
-- >>> primes' !! 1000000 :: Prime Int
-- Prime 15485867
-- (5.29 secs, 6,945,269,856 bytes)
-- >>> primes' !! 1000000 :: Prime Int
-- Prime 15485867
-- (0.02 secs, 336,232 bytes)
primes :: Integral a => [Prime a]
primes
  = (coerce :: [a] -> [Prime a])
  $ takeWhileIncreasing $ 2 : 3 : 5 : concatMap primeListInternal psieveList

-- | List of primes in the form of a list of 'PrimeSieve's, more compact than
--   'primes', thus it may be better to use @'psieveList' >>= 'primeList'@
--   than keeping the list of primes alive during the entire run.
psieveList :: [PrimeSieve]
psieveList = makeSieves plim sqlim 0 0 cache
  where
    plim = 4801     -- prime #647, 644 of them to use
    sqlim = plim*plim
    cache = runSTUArray $ do
        sieve <- sieveTo (4801 :: Integer)
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
sieveFrom :: Integer -> [Prime Integer]
sieveFrom n = case psieveFrom n of
                        ps -> dropWhile ((< n) . unPrime) (ps >>= primeList)

-- | @'psieveFrom' n@ creates the list of 'PrimeSieve's starting roughly
--   at @n@. Due to the organisation of the sieve, the list may contain
--   a few primes less than @n@.
--   This form uses less memory than @['Integer']@, hence it may be preferable
--   to use this if it is to be reused.
psieveFrom :: Integer -> [PrimeSieve]
psieveFrom n = makeSieves plim sqlim bitOff valOff cache
    where
      k0 = ((n `max` 7) - 7) `quot` 30 -- beware arithmetic underflow
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
  | otherwise   = countToNth (n-3) (psieveFrom (intToInteger $ fromInteger n .&. (7 :: Int)))

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

{-# INLINE mu #-}
mu :: Int -> Int
mu i = unsafeAt mArr i

{-# INLINE nu #-}
nu :: Int -> Int
nu i = unsafeAt nArr i

mArr :: UArray Int Int
mArr = listArray (0,63)
        [ 1,  2,  2,  3,  4,  5,  6,  7
        , 2,  3,  4,  6,  6,  8, 10, 11
        , 2,  4,  5,  7,  8,  9, 12, 13
        , 3,  6,  7,  9, 10, 12, 16, 17
        , 4,  6,  8, 10, 11, 14, 18, 19
        , 5,  8,  9, 12, 14, 17, 22, 23
        , 6, 10, 12, 16, 18, 22, 27, 29
        , 7, 11, 13, 17, 19, 23, 29, 31
        ]

nArr :: UArray Int Int
nArr = listArray (0,63)
        [ 4, 3, 7, 6, 2, 1, 5, 0
        , 3, 7, 5, 0, 6, 2, 4, 1
        , 7, 5, 4, 1, 0, 6, 3, 2
        , 6, 0, 1, 4, 5, 7, 2, 3
        , 2, 6, 0, 5, 7, 3, 1, 4
        , 1, 2, 6, 7, 3, 4, 0, 5
        , 5, 4, 3, 2, 1, 0, 7, 6
        , 0, 1, 2, 3, 4, 5, 6, 7
        ]
