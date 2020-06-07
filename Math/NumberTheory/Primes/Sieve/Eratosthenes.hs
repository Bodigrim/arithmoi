-- |
-- Module:      Math.NumberTheory.Primes.Sieve.Eratosthenes
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
--
-- Sieve
--
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fspec-constr-count=8 #-}
module Math.NumberTheory.Primes.Sieve.Eratosthenes
    ( primes
    , psieveFrom
    , PrimeSieve(..)
    , psieveList
    , primeList
    , primeSieve
    , sieveBits
    , sieveRange
    , sieveTo
    ) where

import Control.Monad (when)
import Control.Monad.ST
import Data.Array.Base
import Data.Array.ST
import Data.Bits
import Data.Coerce
import Data.Proxy
import Data.Word

import Math.NumberTheory.Primes.Sieve.Indexing
import Math.NumberTheory.Primes.Types
import Math.NumberTheory.Roots

iXMASK :: Num a => a
iXMASK   = 0xFFFFF

iXBITS :: Int
iXBITS   = 20

iXJMASK :: Num a => a
iXJMASK = 0x7FFFFF

iXJBITS :: Int
iXJBITS = 23

jMASK :: Int
jMASK    = 7

jBITS :: Int
jBITS    = 3

-- Sieve in 128K chunks.
-- Large enough to get something done per chunk
-- and hopefully small enough to fit in the cache.
sieveBytes :: Int
sieveBytes = 128 * 1024

-- Number of bits per chunk.
sieveBits :: Int
sieveBits = 8 * sieveBytes

-- Last index of chunk.
lastIndex :: Int
lastIndex = sieveBits - 1

-- Range of a chunk.
sieveRange :: Int
sieveRange = 30 * sieveBytes

wSHFT :: (Bits a, Num a) => a
wSHFT = if finiteBitSize (0 :: Word) == 64 then 6 else 5

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
        new <- unsafeNewArray_ (0,1287) :: ST s (STUArray s Int Word64)
        let fill j indx
              | 1279 < indx = return new    -- index of 4801 = 159*30 + 31 ~> 159*8+7
              | otherwise = do
                p <- unsafeRead sieve indx
                if p
                  then do
                    let !i = indx .&. jMASK
                        k = indx `shiftR` jBITS
                        strt1 = (k*(30*k + 2*rho i) + byte i) `shiftL` jBITS + idx i
                        !strt = fromIntegral (strt1 .&. iXMASK)
                        !skip = fromIntegral (strt1 `shiftR` iXBITS)
                        !ixes = fromIntegral indx `shiftL` iXJBITS + strt `shiftL` jBITS + fromIntegral i
                    unsafeWrite new j skip
                    unsafeWrite new (j+1) ixes
                    fill (j+2) (indx+1)
                  else fill j (indx+1)
        fill 0 0

makeSieves :: Integer -> Integer -> Integer -> Integer -> UArray Int Word64 -> [PrimeSieve]
makeSieves plim sqlim bitOff valOff cache
  | valOff' < sqlim =
      let (nc, bs) = runST $ do
            cch <- unsafeThaw cache :: ST s (STUArray s Int Word64)
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

slice :: STUArray s Int Word64 -> ST s (STUArray s Int Bool)
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
                let !stj = fromIntegral ixes .&. iXJMASK   -- position of multiple and index of cofactor
                    !ixw = fromIntegral (ixes `shiftR` iXJBITS)  -- prime data, up to 41 bits
                    !i = ixw .&. jMASK
                    !k = ixw - i        -- On 32-bits, k > 44717396 means overflow is possible in tick
                    !o = i `shiftL` jBITS
                    !j = stj .&. jMASK          -- index of cofactor
                    !s = stj `shiftR` jBITS     -- index of first multiple to tick off
                (n, u) <- tick k o j s
                let !skip = fromIntegral (n `shiftR` iXBITS)
                    !strt = fromIntegral (n .&. iXMASK)
                unsafeWrite cache pr skip
                unsafeWrite cache (pr+1) ((ixes .&. complement iXJMASK) .|. strt `shiftL` jBITS .|. fromIntegral u)
            treat (pr+2)
        tick stp off j ix
          | lastIndex < ix  = return (ix - sieveBits, j)
          | otherwise       = do
            p <- unsafeRead sieve ix
            when p (unsafeWrite sieve ix False)
            tick stp off ((j+1) .&. jMASK) (ix + stp*delta j + tau (off+j))
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
                tick stp off ((j+1) .&. jMASK) (ix + stp*delta j + tau (off+j))
            sift ix
              | svbd < ix = return ar
              | otherwise = do
                p <- unsafeRead ar ix
                when p  (do let i = ix .&. jMASK
                                k = ix `shiftR` jBITS
                                !off = i `shiftL` jBITS
                                !stp = ix - i
                            tick stp off i (start k i))
                sift (ix+1)
        sift 0

growCache :: Integer -> Integer -> UArray Int Word64 -> ST s (STUArray s Int Word64)
growCache offset plim old = do
    let (_,num) = bounds old
        (bt,ix) = idxPr plim
        !start  = 8*bt+ix+1
        !nlim   = plim+4800
    sieve <- sieveTo nlim       -- Implement SieveFromTo for this, it's pretty wasteful when nlim isn't
    (_,hi) <- getBounds sieve   -- very small anymore
    more <- countFromToWd start hi sieve
    new <- unsafeNewArray_ (0,num+2*more) :: ST s (STUArray s Int Word64)
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
                let !i = indx .&. jMASK
                    k :: Integer
                    k = fromIntegral (indx `shiftR` jBITS)
                    strt0 = ((k*(30*k + fromIntegral (2*rho i))
                                + fromIntegral (byte i)) `shiftL` jBITS)
                                    + fromIntegral (idx i)
                    strt1 = strt0 - offset
                    !strt = fromIntegral strt1 .&. iXMASK
                    !skip = fromIntegral (strt1 `shiftR` iXBITS)
                    !ixes = fromIntegral indx `shiftL` iXJBITS .|. strt `shiftL` jBITS .|. fromIntegral i
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
    let !sb = start `shiftR` wSHFT
        !eb = end `shiftR` wSHFT
        count !acc i
          | eb < i    = return acc
          | otherwise = do
            w <- unsafeRead wa i
            count (acc + popCount w) (i+1)
    count 0 sb

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
          new <- unsafeNewArray_ (0,2*pct-1) ::  ST s (STUArray s Int Word64)
          let fill j indx
                | hi < indx = return new
                | otherwise = do
                  isPr <- unsafeRead sieve indx
                  if isPr
                    then do
                      let !i = indx .&. jMASK
                          !moff = i `shiftL` jBITS
                          k :: Integer
                          k = fromIntegral (indx `shiftR` jBITS)
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
                                        + fromIntegral (mu (moff + r1))) `shiftL` jBITS)
                                            + fromIntegral (nu (moff + r1))
                          strt1 = ((k*(30*k + fromIntegral (2*rho i))
                                      + fromIntegral (byte i)) `shiftL` jBITS)
                                          + fromIntegral (idx i)
                          (strt2,r2)
                              | p < ssr   = (strt0 - bitOff,r1)
                              | otherwise = (strt1 - bitOff, i)
                          !strt = fromIntegral strt2 .&. iXMASK
                          !skip = fromIntegral (strt2 `shiftR` iXBITS)
                          !ixes = fromIntegral indx `shiftL` iXJBITS .|. strt `shiftL` jBITS .|. fromIntegral r2
                      unsafeWrite new j skip
                      unsafeWrite new (j+1) ixes
                      fill (j+2) (indx+1)
                    else fill j (indx+1)
          fill 0 0


{-# INLINE delta #-}
delta :: Int -> Int
delta = unsafeAt deltas

deltas :: UArray Int Int
deltas = listArray (0,7) [4,2,4,2,4,6,2,6]

{-# INLINE tau #-}
tau :: Int -> Int
tau = unsafeAt taus

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
byte = unsafeAt startByte

startByte :: UArray Int Int
startByte = listArray (0,7) [1,3,5,9,11,17,27,31]

{-# INLINE idx #-}
idx :: Int -> Int
idx = unsafeAt startIdx

startIdx :: UArray Int Int
startIdx = listArray (0,7) [4,7,4,4,7,4,7,7]

{-# INLINE mu #-}
mu :: Int -> Int
mu = unsafeAt mArr

{-# INLINE nu #-}
nu :: Int -> Int
nu = unsafeAt nArr

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
