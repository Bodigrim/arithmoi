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
import Data.Bit
import Data.Bits
import Data.Coerce
import Data.Proxy
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word

import Math.NumberTheory.Primes.Sieve.Indexing
import Math.NumberTheory.Primes.Types
import Math.NumberTheory.Roots
import Math.NumberTheory.Utils.FromIntegral

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

-- | Compact store of primality flags.
data PrimeSieve = PS !Integer {-# UNPACK #-} !(U.Vector Bit)

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
primeSieve bound = PS 0 (runST $ sieveTo bound >>= U.unsafeFreeze)

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
  $ filter (unBit . U.unsafeIndex bs) [lo..hi]
  where
    (lo, hi) = (0, U.length bs - 1)

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
-- >>> primes !! 1000000 :: Prime Int -- (5.32 secs, 6,945,267,496 bytes)
-- Prime 15485867
-- >>> primes !! 1000000 :: Prime Int -- (5.19 secs, 6,945,267,496 bytes)
-- Prime 15485867
--
-- against
--
-- >>> let primes' = primes :: [Prime Int]
-- >>> primes' !! 1000000 :: Prime Int -- (5.29 secs, 6,945,269,856 bytes)
-- Prime 15485867
-- >>> primes' !! 1000000 :: Prime Int -- (0.02 secs, 336,232 bytes)
-- Prime 15485867
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
    cache = runST $ do
        sieve <- sieveTo (4801 :: Integer)
        new <- MU.unsafeNew 1288 :: ST s (MU.MVector s Word64)
        let fill j indx
              | 1279 < indx = return new    -- index of 4801 = 159*30 + 31 ~> 159*8+7
              | otherwise = do
                Bit p <- MU.unsafeRead sieve indx
                if p
                  then do
                    let !i = indx .&. jMASK
                        k = indx `shiftR` jBITS
                        strt1 = (k*(30*k + 2*rho i) + byte i) `shiftL` jBITS + idx i
                        !strt = intToWord64 (strt1 .&. iXMASK)
                        !skip = intToWord64 (strt1 `shiftR` iXBITS)
                        !ixes = intToWord64 indx `shiftL` iXJBITS + strt `shiftL` jBITS + intToWord64 i
                    MU.unsafeWrite new j skip
                    MU.unsafeWrite new (j+1) ixes
                    fill (j+2) (indx+1)
                  else fill j (indx+1)
        vec <- fill 0 0
        U.unsafeFreeze vec

makeSieves :: Integer -> Integer -> Integer -> Integer -> U.Vector Word64 -> [PrimeSieve]
makeSieves plim sqlim bitOff valOff cache
  | valOff' < sqlim =
      let (nc, bs) = runST $ do
            cch <- U.unsafeThaw cache :: ST s (MU.MVector s Word64)
            bs0 <- slice cch
            fcch <- U.unsafeFreeze cch
            fbs0 <- U.unsafeFreeze bs0
            return (fcch, fbs0)
      in PS valOff bs : makeSieves plim sqlim bitOff' valOff' nc
  | otherwise       =
      let plim' = plim + 4800
          sqlim' = plim' * plim'
          (nc,bs) = runST $ do
            cch <- growCache bitOff plim cache
            bs0 <- slice cch
            fcch <- U.unsafeFreeze cch
            fbs0 <- U.unsafeFreeze bs0
            return (fcch, fbs0)
      in PS valOff bs : makeSieves plim' sqlim' bitOff' valOff' nc
    where
      valOff' = valOff + intToInteger sieveRange
      bitOff' = bitOff + intToInteger sieveBits

slice :: MU.MVector s Word64 -> ST s (MU.MVector s Bit)
slice cache = do
    let hi = MU.length cache - 1
    sieve <- MU.replicate (lastIndex + 1) (Bit True)
    let treat pr
          | hi < pr     = return sieve
          | otherwise   = do
            w <- MU.unsafeRead cache pr
            if w /= 0
              then MU.unsafeWrite cache pr (w-1)
              else do
                ixes <- MU.unsafeRead cache (pr+1)
                let !stj = word64ToInt ixes .&. iXJMASK   -- position of multiple and index of cofactor
                    !ixw = word64ToInt (ixes `shiftR` iXJBITS)  -- prime data, up to 41 bits
                    !i = ixw .&. jMASK
                    !k = ixw - i        -- On 32-bits, k > 44717396 means overflow is possible in tick
                    !o = i `shiftL` jBITS
                    !j = stj .&. jMASK          -- index of cofactor
                    !s = stj `shiftR` jBITS     -- index of first multiple to tick off
                (n, u) <- tick k o j s
                let !skip = intToWord64 (n `shiftR` iXBITS)
                    !strt = intToWord64 (n .&. iXMASK)
                MU.unsafeWrite cache pr skip
                MU.unsafeWrite cache (pr+1) ((ixes .&. complement iXJMASK) .|. strt `shiftL` jBITS .|. intToWord64 u)
            treat (pr+2)
        tick stp off j ix
          | lastIndex < ix  = return (ix - sieveBits, j)
          | otherwise       = do
            Bit p <- MU.unsafeRead sieve ix
            when p (MU.unsafeWrite sieve ix (Bit False))
            tick stp off ((j+1) .&. jMASK) (ix + stp*delta j + tau (off+j))
    treat 0

-- | Sieve up to bound in one go.
sieveTo :: Integer -> ST s (MU.MVector s Bit)
sieveTo bound = arr
  where
    (bytes,lidx) = idxPr bound
    !mxidx = 8*bytes+lidx
    mxval :: Integer
    mxval = 30*intToInteger bytes + intToInteger (rho lidx)
    !mxsve = integerSquareRoot mxval
    (kr,r) = idxPr mxsve
    !svbd = 8*kr+r
    arr = do
        ar <- MU.replicate (mxidx + 1) (Bit True)
        let start k i = 8*(k*(30*k+2*rho i) + byte i) + idx i
            tick stp off j ix
              | mxidx < ix = return ()
              | otherwise  = do
                Bit p <- MU.unsafeRead ar ix
                when p (MU.unsafeWrite ar ix (Bit False))
                tick stp off ((j+1) .&. jMASK) (ix + stp*delta j + tau (off+j))
            sift ix
              | svbd < ix = return ar
              | otherwise = do
                Bit p <- MU.unsafeRead ar ix
                when p  (do let i = ix .&. jMASK
                                k = ix `shiftR` jBITS
                                !off = i `shiftL` jBITS
                                !stp = ix - i
                            tick stp off i (start k i))
                sift (ix+1)
        sift 0

growCache :: Integer -> Integer -> U.Vector Word64 -> ST s (MU.MVector s Word64)
growCache offset plim old = do
    let num = U.length old - 1
        (bt,ix) = idxPr plim
        !start  = 8*bt+ix+1
        !nlim   = plim+4800
    sieve <- sieveTo nlim       -- Implement SieveFromTo for this, it's pretty wasteful when nlim isn't
    let hi = MU.length sieve - 1
    more <- countFromToWd start hi sieve
    new <- MU.unsafeNew (1 + num + 2 * more) :: ST s (MU.MVector s Word64)
    let copy i
          | num < i   = return ()
          | otherwise = do
            MU.unsafeWrite new i (old `U.unsafeIndex` i)
            copy (i+1)
    copy 0
    let fill j indx
          | hi < indx = return new
          | otherwise = do
            Bit p <- MU.unsafeRead sieve indx
            if p
              then do
                let !i = indx .&. jMASK
                    k :: Integer
                    k = intToInteger (indx `shiftR` jBITS)
                    strt0 = ((k*(30*k + intToInteger (2*rho i))
                                + intToInteger (byte i)) `shiftL` jBITS)
                                    + intToInteger (idx i)
                    strt1 = strt0 - offset
                    !strt = integerToWord64 strt1 .&. iXMASK
                    !skip = integerToWord64 (strt1 `shiftR` iXBITS)
                    !ixes = intToWord64 indx `shiftL` iXJBITS .|. strt `shiftL` jBITS .|. intToWord64 i
                MU.unsafeWrite new j skip
                MU.unsafeWrite new (j+1) ixes
                fill (j+2) (indx+1)
              else fill j (indx+1)
    fill (num+1) start

{-# INLINE countFromToWd #-}
countFromToWd :: Int -> Int -> MU.MVector s Bit -> ST s Int
countFromToWd start end =
  fmap countBits . U.unsafeFreeze . MU.slice start (end - start + 1)

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
      end1 = start - 6 + intToInteger sieveRange
      plim0 = integerSquareRoot end1
      plim = plim0 + 4801 - (plim0 `rem` 4800)
      sqlim = plim*plim
      cache = runST $ do
          sieve <- sieveTo plim
          let (lo,hi) = (0, MU.length sieve - 1)
          pct <- countFromToWd lo hi sieve
          new <- MU.unsafeNew (2 * pct) ::  ST s (MU.MVector s Word64)
          let fill j indx
                | hi < indx = return new
                | otherwise = do
                  Bit isPr <- MU.unsafeRead sieve indx
                  if isPr
                    then do
                      let !i = indx .&. jMASK
                          !moff = i `shiftL` jBITS
                          k :: Integer
                          k = intToInteger (indx `shiftR` jBITS)
                          p = 30*k+intToInteger (rho i)
                          q0 = (start-1) `quot` p
                          (skp0,q1) = q0 `quotRem` intToInteger sieveRange
                          (b0,r0)
                              | q1 == 0   = (-1,6)
                              | q1 < 7    = (-1,7)
                              | otherwise = idxPr (integerToInt q1 :: Int)
                          (b1,r1) | r0 == 7 = (b0+1,0)
                                  | otherwise = (b0,r0+1)
                          b2 = skp0*intToInteger sieveBytes + intToInteger b1
                          strt0 = ((k*(30*b2 + intToInteger (rho r1))
                                        + b2 * intToInteger (rho i)
                                        + intToInteger (mu (moff + r1))) `shiftL` jBITS)
                                            + intToInteger (nu (moff + r1))
                          strt1 = ((k*(30*k + intToInteger (2*rho i))
                                      + intToInteger (byte i)) `shiftL` jBITS)
                                          + intToInteger (idx i)
                          (strt2,r2)
                              | p < ssr   = (strt0 - bitOff,r1)
                              | otherwise = (strt1 - bitOff, i)
                          !strt = integerToWord64 strt2 .&. iXMASK
                          !skip = integerToWord64 (strt2 `shiftR` iXBITS)
                          !ixes = intToWord64 indx `shiftL` iXJBITS .|. strt `shiftL` jBITS .|. intToWord64 r2
                      MU.unsafeWrite new j skip
                      MU.unsafeWrite new (j+1) ixes
                      fill (j+2) (indx+1)
                    else fill j (indx+1)
          vec <- fill 0 0
          U.unsafeFreeze vec

{-# INLINE delta #-}
delta :: Int -> Int
delta = U.unsafeIndex deltas

deltas :: U.Vector Int
deltas = U.fromList [4,2,4,2,4,6,2,6]

{-# INLINE tau #-}
tau :: Int -> Int
tau = U.unsafeIndex taus

taus :: U.Vector Int
taus = U.fromList
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
byte = U.unsafeIndex startByte

startByte :: U.Vector Int
startByte = U.fromList [1,3,5,9,11,17,27,31]

{-# INLINE idx #-}
idx :: Int -> Int
idx = U.unsafeIndex startIdx

startIdx :: U.Vector Int
startIdx = U.fromList [4,7,4,4,7,4,7,7]

{-# INLINE mu #-}
mu :: Int -> Int
mu = U.unsafeIndex mArr

{-# INLINE nu #-}
nu :: Int -> Int
nu = U.unsafeIndex nArr

mArr :: U.Vector Int
mArr = U.fromList
        [ 1,  2,  2,  3,  4,  5,  6,  7
        , 2,  3,  4,  6,  6,  8, 10, 11
        , 2,  4,  5,  7,  8,  9, 12, 13
        , 3,  6,  7,  9, 10, 12, 16, 17
        , 4,  6,  8, 10, 11, 14, 18, 19
        , 5,  8,  9, 12, 14, 17, 22, 23
        , 6, 10, 12, 16, 18, 22, 27, 29
        , 7, 11, 13, 17, 19, 23, 29, 31
        ]

nArr :: U.Vector Int
nArr = U.fromList
        [ 4, 3, 7, 6, 2, 1, 5, 0
        , 3, 7, 5, 0, 6, 2, 4, 1
        , 7, 5, 4, 1, 0, 6, 3, 2
        , 6, 0, 1, 4, 5, 7, 2, 3
        , 2, 6, 0, 5, 7, 3, 1, 4
        , 1, 2, 6, 7, 3, 4, 0, 5
        , 5, 4, 3, 2, 1, 0, 7, 6
        , 0, 1, 2, 3, 4, 5, 6, 7
        ]
