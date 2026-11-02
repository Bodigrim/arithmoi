-- |
-- Module:      Math.NumberTheory.Primes.Counting.CountingImpl
-- Copyright:   (c) 2026 W. Gordon Goodsman (AKA GordonBGood)
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Number of primes not exceeding @limit@, @&#960;(limit)@.
--
{-# LANGUAGE CPP, BangPatterns, MagicHash, UnboxedTuples, UnliftedFFITypes #-}

-- Gourdon prime counting with multi-threading, page-segmentation, and wheel-factorization.
--
--   See the "HowPrimeCountingWorks.md" file in this directory for
--   a more complete explanation of how this implementation works.
--
--   Input : Word64      Output: Int64
--   pi(x) = A - B + C + D + Phi0 + Sigma   (Gourdon)
--
-- Conventions
--   * wheel-30 sieve, ONE BYTE per 30 values, bit index i = 8*(v div 30) + position;
--     SET bit = composite (cull = OR), clear bit = survivor, as in the Meissel engine.
--   * value-like quantities that can reach 2^64 are Word64; everything used to index memory
--     or to count is native Int (64-bit); the final sum is wrapped into Int64.
--   * the sieve is processed in pages of 512 KB.
--   * D phase (partial sieving): one Word16 count of survivors per block of 2^BLKSHIFT 64-bit
--     words (default 8 = 512 bits).  A counted cull decrements the block count of every bit
--     it actually sets, so syncing the prefix counts is a cumulative sum over the blocks
--     (no popcount) and a query popcounts at most one block.
--   * C, A and B queries always use a final per-word popcount sync.
--   * B-term primes (y, sqrt x] come from a reverse page-segmented sieve run lazily as the
--     page loop advances (one generator state in a small Ptr Int block).
--   * the query loops divide in two stages: x div p1 once per outer prime (Word64), then
--     (x div p1) div p2 per query.
--
-- Uses Template Haskell to cull the base primes by TH-generated unrolled kernels (one
-- pass per prime, 8 slots per stride; plain and counted variants).  HLS, which defines
-- __GHCIDE__, selects the scalar eight-plane culler instead; results are identical; this
-- avoids HLS crashing the IDE with huge memory use due to tracking the unrolled loops.
--
-- Modules:
--   WheelIndexing    page size / block size constants, the 30-wheel tables and conversions, integer roots, fi, unIO
--   Kernels          the Template Haskell culling kernels (empty with -DNO_TH)
--   Sieve            cullPage, fillPage, block counts and the D-phase operations
--   this file        query loops, bigCount (single thread), bigCountT, the pure API, alpha, main
--
-- Pure API: primeCount :: Integer -> Integer (n <= 0 gives 0, n > 2^64-1 is an error).
--
-- Alpha (y = alpha * cbrt x): comes from the fit in defaultAlpha.
--
-- This module is threaded, so must be compiled with the -threaded compiler linker flag.
--
-- Buffer pages are independent tasks computed with the carried values (basephis, running pi) set to ZERO;
-- each returns a partial sum plus the coefficients that multiply the carried values,
-- and the main thread merges the tasks in page order.
-- The expensive low pages (all the A queries) are cut into chunks of work units (a range of D base primes.
-- A dynamic work queue feeds forkIO'd workers that own their buffers.  The
-- single-thread code path (bigCount) is used with one capability or below a limit of 10^11.
-- Eight pages of run-ahead per thread; only small result arrays are held per page,
-- the workers allocate almost nothing and GC is rare.
--
-- Suggested RTS options: -N<n> -qg -A64m (default RTS options also scale).
--

module Math.NumberTheory.Primes.Counting.CountingImpl
    ( getNumThreads, setNumThreads
    , primeCountMaxArg
    , primeCount
    ) where

import Math.NumberTheory.Primes.Sieve.Sieve
import Math.NumberTheory.Primes.Sieve.WheelIndexing

import Data.Int ( Int64 )
import Data.Word ( Word8, Word16, Word32, Word64 )
import Data.Bits
    ( Bits((.&.), unsafeShiftL, xor, unsafeShiftR, complement),
      FiniteBits(countTrailingZeros) )
import qualified Data.Array as A
import Foreign.Ptr ( castPtr, plusPtr, nullPtr )
import Foreign.Storable ( peekElemOff, pokeElemOff, peekByteOff )
import Foreign.Marshal.Alloc ( mallocBytes, callocBytes, free )
import Foreign.Marshal.Utils ( fillBytes )
import GHC.IO ( IO(..) )
import System.IO.Unsafe ( unsafeDupablePerformIO, unsafePerformIO )
import GHC.Exts
import Control.Concurrent
  ( forkIO, getNumCapabilities, setNumCapabilities, rtsSupportsBoundThreads )
import Control.Concurrent.MVar ( MVar, newEmptyMVar, putMVar, takeMVar )
import Control.Exception ( SomeException, try, finally, throwIO )
import Control.Monad ( when, unless )
import Data.IORef ( newIORef, readIORef, writeIORef, atomicModifyIORef' )
import GHC.Conc
  ( getNumProcessors, atomically, newTVarIO, readTVarIO
  , readTVar, writeTVar, retry )

primeCountMaxArg :: Integer
primeCountMaxArg = 2^(64 :: Int) - 1

primeCount :: Integer -> Integer
primeCount limit
  | limit < 2 = 0
  | limit > primeCountMaxArg =
      error $ "primeCount: can't handle bound " ++ show limit
  | otherwise =
      fromIntegral (unsafePerformIO (countPrimes (fromIntegral limit)))
{-# NOINLINE primeCount #-}

------------------------------------------------------------------------
-- Query loops shared by several terms
------------------------------------------------------------------------

compactSize :: Int
compactSize = 256

-- sum over odd factor indices i = hiI down to loI+1 with fctrs[i] > bp (lpf above bp):
--   mu * (count(limit div (bp*(2i+1))) + add)       (D1 and C1)
-- The three query loops below carry their state as Int# and return (# State#, Int# #).  An IO loop
-- written with boxed Int arguments and a final `return acc` gets its accumulator BOXED by GHC 9.6
-- (boxity analysis keeps an argument boxed when it is returned boxed), which showed up in the
-- assembly as a 16-byte heap allocation, a heap check and stack stores on every query.
-- This version with a branch-free compaction pass (see the header), after primecount's D1: the qualifying fctrs INDICES
-- are copied to pbuf (4 entries per iteration, one store each, the output position advances by the 0/1 test), and
-- the query loop re-reads fctrs[i] for the sign.
muLoopC :: Word64 -> Ptr Word16 -> (Int -> IO Int) -> Ptr Int
        -> Int -> Int -> Int -> Int -> Int -> IO Int
muLoopC limW fctrs qry pbuf bp hiI loI add acc0 =
  IO (\sa -> case unIO (return ()) sa of
    (# s0, _ #) -> case chunk (loI# +# 1#) acc0# s0 of
                     (# s1, r #) -> (# s1, I# r #))
  where
    nume       = limW `quot` fi bp :: Word64
    !(I# bp#)   = bp
    !(I# hiI#)  = hiI
    !(I# loI#)  = loI
    !(I# add#)  = add
    !(I# acc0#) = acc0
    !(I# ch#)   = compactSize
    !(Ptr fc#)  = fctrs
    !(Ptr pb#)  = pbuf
    chunk :: Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
    chunk i0 acc s
      | isTrue# (i0 ># hiI#) = (# s, acc #)
      | otherwise =
          let !left = (hiI# -# i0) +# 1#
              !cnt  = if isTrue# (left ># ch#) then ch# else left
          in case compact4 i0 cnt 0# 0# s of
               (# s1, m #) ->
                 case sumPk nume 0# m acc s1 of
                   (# s2, accN #) -> chunk (i0 +# cnt) accN s2
    -- store index a at output position m, advance m when fctrs[a] > bp
    step :: Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
    step a m s = case readWord16OffAddr# fc# a s of
      (# s1, w #) -> case writeIntOffAddr# pb# m a s1 of
        s2 -> (# s2, m +# (word2Int# (word16ToWord# w) ># bp#) #)
    {-# INLINE step #-}
    compact4 :: Int# -> Int# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
    compact4 i0 cnt t m s
      | isTrue# ((t +# 4#) ># cnt) = compact1 i0 cnt t m s
      | otherwise = case step (i0 +# t) m s of
          (# s1, m1 #) -> case step (i0 +# t +# 1#) m1 s1 of
            (# s2, m2 #) -> case step (i0 +# t +# 2#) m2 s2 of
              (# s3, m3 #) -> case step (i0 +# t +# 3#) m3 s3 of
                (# s4, m4 #) -> compact4 i0 cnt (t +# 4#) m4 s4
    compact1 :: Int# -> Int# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
    compact1 i0 cnt t m s
      | isTrue# (t >=# cnt) = (# s, m #)
      | otherwise = case step (i0 +# t) m s of
          (# s1, m1 #) -> compact1 i0 cnt (t +# 1#) m1 s1
    -- queries with the division inside the loop
    sumPk :: Word64 -> Int# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
    sumPk !n j m acc s
      | isTrue# (j >=# m) = (# s, acc #)
      | otherwise = case readIntOffAddr# pb# j s of
          (# s1, i #) -> case readWord16OffAddr# fc# i s1 of
            (# s2, fw #) -> case unIO (return ()) s2 of
              (# s3, _ #) ->
                let !d = (i *# 2#) +# 1#
                    !f = word2Int# (word16ToWord# fw)
                in case unIO (qry (fi (n `quot` fi (I# d)))) s3 of
                     (# s4, I# c #) ->
                       let !sg = ((f `andI#` 1#) *# 2#) -# 1#
                       in sumPk n (j +# 1#) m (acc +# sg *# (c +# add#)) s4
{-# INLINE muLoopC #-}

-- sum over odd primes p2 = yprms[hiI down to loI] of  count(limit div (p1*p2)) + add
-- (D2 and C2)
pairLoop :: Word64 -> Ptr Word32 -> (Int -> IO Int)
         -> Int -> Int -> Int -> Int -> Int -> IO Int
pairLoop limW yprms qry p1 hiI loI add acc0 =
  IO (\s0 -> case goW nume hiI# acc0# s0 of
               (# s1, r #) -> (# s1, I# r #))
  where
    nume       = limW `quot` fi p1 :: Word64
    !(I# hiI#)  = hiI
    !(I# loI#)  = loI
    !(I# add#)  = add
    !(I# acc0#) = acc0
    goW :: Word64 -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
    goW !n i acc s
      | isTrue# (i <# loI#) = (# s, acc #)
      | otherwise = case unIO (peekElemOff yprms (I# i)) s of
          (# s1, p2 #) -> case unIO (return ()) s1 of
            (# s2, _ #) -> case unIO (qry (fi (n `quot` fi p2))) s2 of
              (# s3, I# c #) -> goW n (i -# 1#) (acc +# c +# add#) s3
{-# INLINE pairLoop #-}

-- A term loop: weight * (totprvpi + count(lmtdivp1 div yprms[p2i])), p2i = hiI down to loI
aLoop :: Ptr Word32 -> Ptr Word32 -> Ptr Word64
      -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO Int
aLoop yprms blk b64 lw lmtdivp1 weight totprvpi hiI loI acc0 =
  IO (\s0 -> case goI hiI# acc0# s0 of
               (# s1, r #) -> (# s1, I# r #))
  where
    !(I# hiI#)  = hiI
    !(I# loI#)  = loI
    !(I# acc0#) = acc0
    !(I# w#)    = weight
    !(I# tp#)   = totprvpi
    goI :: Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
    goI i acc s
      | isTrue# (i <# loI#) = (# s, acc #)
      | otherwise = case unIO (peekElemOff yprms (I# i)) s of
          (# s1, p2 #) -> case unIO (return ()) s1 of
            (# s2, _ #) -> case unIO (pntCnt lw blk b64 (lmtdivp1 `quot` fi p2)) s2 of
              (# s3, I# c #) -> goI (i -# 1#) (acc +# w# *# (tp# +# c)) s3

strideZero16 :: Ptr Word16 -> Int -> Int -> Int -> IO ()
strideZero16 p i0 step n = go i0
  where
    go !i | i >= n    = return ()
          | otherwise = pokeElemOff p i 0 >> go (i + step)

strideXor16 :: Ptr Word16 -> Int -> Int -> Int -> IO ()
strideXor16 p i0 step n = go i0
  where
    go !i | i >= n    = return ()
          | otherwise = do v <- peekElemOff p i
                           pokeElemOff p i (v `xor` 1)
                           go (i + step)

-----------------------------------------------------------------------
-- THREADED build (link with -threaded): top-level pieces
------------------------------------------------------------------------

-- Number of threads = GHC capabilities.  Plain IO actions (not CAFs), so they always see the current value.
-- setNumThreads clips the request to 1 .. getNumProcessors (logical processors, SMT included) and returns the
-- value in force.  With one capability the single-thread code (bigCount) runs.
getNumThreads :: IO Int
getNumThreads = getNumCapabilities

setNumThreads :: Int -> IO Int
setNumThreads n = do
  np <- getNumProcessors
  when rtsSupportsBoundThreads $ setNumCapabilities (max 1 (min np n))
  getNumCapabilities

-- Threading is used from this counting limit upward (and only with at least two capabilities).
threadMinX :: Word64
threadMinX = 100000000000

-- per-page constants (what the page loop of bigCount computes at the top of every iteration)
data PG = PG { pgFrst :: !Int, pgNxt :: !Int, pgGrddNxt :: !Int, pgMps :: !Word64
             , pgLda :: !Int                 -- limit div acsplit
             , pgBpisz :: !Int               -- number of D base primes of the page
             , pgMinp1C2 :: !Int, pgNC2 :: !Int
             , pgMinp1A :: Int, pgNA :: !Int }

-- the buffers one worker thread owns for its whole life
data WBuf = WBuf { wbPage :: !(Ptr Word8), wbBlk :: !(Ptr Word32), wbPbuf :: !(Ptr Int)
                 , wbChnk :: !(Ptr Word16), wbGbuf :: !(Ptr Word8), wbSg :: !(Ptr Int) }

-- muLoopC for a ZERO-CARRIED page: the same loop, but it also stores the SUM OF THE SIGNS of the queried entries
-- at sgp[0].  The caller's `add` then leaves out the carried part, which the ordered merge adds back as
-- (sum of signs) * carried.
muLoopCZ :: Word64 -> Ptr Word16 -> (Int -> IO Int) -> Ptr Int -> Ptr Int
         -> Int -> Int -> Int -> Int -> Int -> IO Int
muLoopCZ limW fctrs qry pbuf sgp bp hiI loI add acc0 =
  IO (\sa -> case unIO (return ()) sa of
    (# s0, _ #) -> case chunk (loI# +# 1#) acc0# 0# s0 of
                     (# s1, r, g #) -> case writeIntOffAddr# sp# 0# g s1 of
                                         s2 -> (# s2, I# r #))
  where
    nume       = limW `quot` fi bp :: Word64
    !(I# bp#)   = bp
    !(I# hiI#)  = hiI
    !(I# loI#)  = loI
    !(I# add#)  = add
    !(I# acc0#) = acc0
    !(I# ch#)   = compactSize
    !(Ptr fc#)  = fctrs
    !(Ptr pb#)  = pbuf
    !(Ptr sp#)  = sgp
    chunk :: Int# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int#, Int# #)
    chunk i0 acc g s
      | isTrue# (i0 ># hiI#) = (# s, acc, g #)
      | otherwise =
          let !left = (hiI# -# i0) +# 1#
              !cnt  = if isTrue# (left ># ch#) then ch# else left
          in case compact4 i0 cnt 0# 0# s of
               (# s1, m #) ->
                 case sumPk nume 0# m acc g s1 of
                   (# s2, accN, gN #) -> chunk (i0 +# cnt) accN gN s2
    step :: Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
    step a m s = case readWord16OffAddr# fc# a s of
      (# s1, w #) -> case writeIntOffAddr# pb# m a s1 of
        s2 -> (# s2, m +# (word2Int# (word16ToWord# w) ># bp#) #)
    {-# INLINE step #-}
    compact4 :: Int# -> Int# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
    compact4 i0 cnt t m s
      | isTrue# ((t +# 4#) ># cnt) = compact1 i0 cnt t m s
      | otherwise = case step (i0 +# t) m s of
          (# s1, m1 #) -> case step (i0 +# t +# 1#) m1 s1 of
            (# s2, m2 #) -> case step (i0 +# t +# 2#) m2 s2 of
              (# s3, m3 #) -> case step (i0 +# t +# 3#) m3 s3 of
                (# s4, m4 #) -> compact4 i0 cnt (t +# 4#) m4 s4
    compact1 :: Int# -> Int# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
    compact1 i0 cnt t m s
      | isTrue# (t >=# cnt) = (# s, m #)
      | otherwise = case step (i0 +# t) m s of
          (# s1, m1 #) -> compact1 i0 cnt (t +# 1#) m1 s1
    sumPk :: Word64 -> Int# -> Int# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int#, Int# #)
    sumPk !n j m acc g s
      | isTrue# (j >=# m) = (# s, acc, g #)
      | otherwise = case readIntOffAddr# pb# j s of
          (# s1, i #) -> case readWord16OffAddr# fc# i s1 of
            (# s2, fw #) -> case unIO (return ()) s2 of
              (# s3, _ #) ->
                let !d = (i *# 2#) +# 1#
                    !f = word2Int# (word16ToWord# fw)
                in case unIO (qry (fi (n `quot` fi (I# d)))) s3 of
                     (# s4, I# c #) ->
                       let !sg = ((f `andI#` 1#) *# 2#) -# 1#
                       in sumPk n (j +# 1#) m (acc +# sg *# (c +# add#)) (g +# sg) s4
{-# INLINE muLoopCZ #-}

-- sum of a[i] * b[i], i < n
dotInts :: Ptr Int -> Ptr Int -> Int -> IO Int
dotInts a b n = go 0 0
  where go !i !acc | i >= n    = return acc
                   | otherwise = do { x <- peekElemOff a i; z <- peekElemOff b i; go (i + 1) (acc + x * z) }

-- carried[i] += pend[i]; pend[i] = 0
foldInts :: Ptr Int -> Ptr Int -> Int -> IO ()
foldInts carried pend n = go 0
  where go !i | i >= n    = return ()
              | otherwise = do { x <- peekElemOff carried i; z <- peekElemOff pend i
                               ; pokeElemOff carried i (x + z); pokeElemOff pend i 0; go (i + 1) }


------------------------------------------------------------------------
-- The counting function
------------------------------------------------------------------------

smallCount :: Int -> Int
smallCount l
  | l < 2     = 0
  | l < 3     = 1
  | l < 9     = (l + 1) `unsafeShiftR` 1
  | l < 25    = ((l - 1) `unsafeShiftR` 1) - ((l `quot` 3 - 1) `unsafeShiftR` 1) + 1
  | otherwise = ((l + 1) `unsafeShiftR` 1) - ((l `quot` 3 + 1) `unsafeShiftR` 1)
                  - ((l `quot` 5 + 1) `unsafeShiftR` 1)
                  + ((l `quot` 15 + 1) `unsafeShiftR` 1) + 2

-- Default alpha (y = alpha * cbrt x): cubic in ln x, fitted to the tuned points (this implementation, NCG, threaded)
--   1e12 5.0   1e14 8.0   1e16 14.0   1e18 21.0   2^64-1 33.0
-- (the optimum is flat at the low end, so the high end matters most; the curve is increasing over 1e12 .. 2^64).
-- Constant, equal to the curve at 1e12 (4.82536), for x <= 1e12.  -DALPHA=<a> fixes it; callers may pass their own alpha.
defaultAlpha :: Word64 -> Double
defaultAlpha x
  | x <= 1000000000000 = 4.82536
  | otherwise = let ln = log (fromIntegral x :: Double)
                in -285.25017727 + ln * (26.13202015 + ln * (-0.79414795 + ln * 0.00826392))

countPrimes :: Word64 -> IO Int64
countPrimes x = countPrimesAlpha x (defaultAlpha x)

countPrimesAlpha :: Word64 -> Double -> IO Int64
countPrimesAlpha limW alpha
  | limW < 49 = return (fi (smallCount (fi limW)))
  | limW >= threadMinX =
      if rtsSupportsBoundThreads then do
        nt <- getNumCapabilities
        if nt < 2 then do -- if can thread but only one set, set maximum...
          mxnt <- getNumProcessors
          setNumCapabilities mxnt; bigCountT limW alpha mxnt
        else bigCountT limW alpha nt
      else bigCount limW alpha
  | otherwise = bigCount limW alpha 

bigCount :: Word64 -> Double -> IO Int64
bigCount limW alpha = do
  let -- ---- derived constants (all fit Int; limit itself stays Word64) ----
      !sqrtlmtW    = isqrtW limW
      !sqrtlmt     = fi sqrtlmtW :: Int
      !cbrtlmt     = fi (icbrtW limW) :: Int
      !yraw        = truncate (alpha * fi cbrtlmt) :: Int
      !y           = min sqrtlmt (max yraw (max (cbrtlmt + 2) 64))
      !ysz         = toOddNdx y + 1
      !sqrty       = isqrtI y
      !sqrtysz     = toOddNdx sqrty + 1
      !lmtdivyW    = limW `quot` fi y
      !lmtdivy     = fi lmtdivyW :: Int
      !sqrtlmtdivy = fi (isqrtW lmtdivyW) :: Int
      !sqrtsqrtlmt = fi (isqrtW sqrtlmtW) :: Int
      !lmtstar     = max sqrtsqrtlmt (fi (lmtdivyW `quot` fi y)) :: Int
      !blkbits     = 64 * blkWords
      !maxsieve    = toWhlNdx lmtdivy + blkbits
      !sievebitsz  = maxsieve - (maxsieve `rem` blkbits)
      !fullsievesz = sievebitsz `quot` 8
      !sievesz     = min pageBytes fullsievesz
      !nwords      = sievesz `quot` 8
      !nblk        = nwords `unsafeShiftR` blkShift
      pcAll        = takeWhile (\p -> (fi p :: Word64) * fi p <= limW) [7, 11, 13, 17, 19 :: Int]
      preCull      = map fst (takeWhile (\(_, pr) -> pr <= sievesz) (zip pcAll (scanl1 (*) pcAll)))
      !prodpc      = product preCull :: Int
      !k           = length preCull + 3

  tm <- callocBytes (32 * 8) :: IO (Ptr Int)
  -- ---- factor (Moebius / least prime factor) table over odd numbers up to y ----
  fctrs <- mallocBytes (ysz * 2) :: IO (Ptr Word16)
  fillBytes fctrs 0xFF (ysz * 2)
  let zeroFor bp = do { pokeElemOff fctrs (toOddNdx bp) 0
                      ; strideZero16 fctrs (toOddNdx (bp * bp)) bp ysz }
  mapM_ zeroFor ([3, 5] ++ preCull)
  let lowPrimes !bpi
        | bpi >= sqrtysz = return ()
        | otherwise = do
            v <- peekElemOff fctrs bpi
            if v >= 0xFFFF
              then do
                let !bp  = 2 * bpi + 1
                    !sqr = bp * bp
                    mark !ci
                      | ci >= ysz = return ()
                      | otherwise = do
                          c <- peekElemOff fctrs ci
                          pokeElemOff fctrs ci (if c >= 0xFFFF then fi bp else c `xor` 1)
                          mark (ci + bp)
                mark (toOddNdx bp + bp)
                strideZero16 fctrs (toOddNdx sqr) sqr ysz
                lowPrimes (bpi + 1)
              else lowPrimes (bpi + 1)
  lowPrimes 1
  let highPrimes !bpi
        | bpi >= ysz = return ()
        | otherwise = do
            v <- peekElemOff fctrs bpi
            if v >= 0xFFFF
              then do
                let !bp = 2 * bpi + 1
                strideXor16 fctrs (toOddNdx bp + bp) bp ysz
                highPrimes (bpi + 1)
              else highPrimes (bpi + 1)
  highPrimes sqrtysz
  pokeElemOff fctrs 0 0xFFFE   -- the "one" is special: not a prime

  -- ---- pi table over odd numbers (piys) and the prime array up to y (yprms) ----
  piys <- mallocBytes (ysz * 4) :: IO (Ptr Word32)
  let mkPi !i !cnt
        | i >= ysz = return cnt
        | otherwise = do
            v <- peekElemOff fctrs i
            let !cnt' = if v >= 0xFFFF then cnt + 1 else cnt
            pokeElemOff piys i (fi cnt')
            mkPi (i + 1) cnt'
  piyTotal <- mkPi 0 k
  yprms <- mallocBytes (max 4 ((piyTotal - k) * 4)) :: IO (Ptr Word32)
  let mkPrms !i
        | i >= ysz = return ()
        | otherwise = do
            v <- peekElemOff fctrs i
            if v >= 0xFFFF
              then do
                pc <- peekElemOff piys i
                pokeElemOff yprms (fi pc - k - 1) (fi (2 * i + 1))
                mkPrms (i + 1)
              else mkPrms (i + 1)
  mkPrms 0

  let -- read-only table lookups: the table is complete before any of these is forced
      piAtIdx :: Int -> Int
      piAtIdx i = fi (unsafeDupablePerformIO (peekElemOff piys i))
      piOf :: Int -> Int
      piOf n = piAtIdx (toOddNdx n)
      !piy            = piyTotal
      !picbrtlmt      = piOf cbrtlmt
      !pisqrtlmtdivy  = piOf sqrtlmtdivy
      !pilmtstar      = piOf lmtstar
      !pisqrty        = piOf sqrty
      !picbrtlmtdivy  = max k (piOf (icbrtI lmtdivy))
      lmtDiv :: Int -> Int          -- limit div d  (d >= 7 here, so the result fits Int)
      lmtDiv d = fi (limW `quot` fi d)

  -- ---- the pre-cull pattern, page buffers ----
  let !patLen = prodpc + 16384 + 64
  ptrn <- callocBytes patLen :: IO (Ptr Word8)
  mapM_ (\p -> cullPage 0 ptrn nullPtr 0 p patLen p 0) preCull
  page <- mallocBytes sievesz :: IO (Ptr Word8)
  let !page64 = castPtr page :: Ptr Word64
  blk <- mallocBytes ((nwords + 1) * 4) :: IO (Ptr Word32)   -- block prefix counts (D) / word prefix counts (C, A, B)
  pbuf <- mallocBytes (compactSize * 8) :: IO (Ptr Int)        -- compacted muLoop indices
  chnk <- mallocBytes (nblk * 2) :: IO (Ptr Word16)           -- per-block survivor counts (delta counts)

  -- ---- Phi0 (pattern holds all pre-cull primes, so tinyphi is one lookup) ----
  fillPage page ptrn prodpc sievesz 0
  syncBlk 0 blk page64 nwords (sievesz * 30)
  let !kprmscirc = prodpc * 15
      !kprmshits = product (map (subtract 1) preCull) * 8 :: Int
      tinyPhi :: Word64 -> IO Int
      tinyPhi v = do
        let !oxndv = fi ((v - 1) `unsafeShiftR` 1) :: Int
            !cd    = oxndv `quot` kprmscirc
            !cm    = oxndv - cd * kprmscirc
        c <- pntCnt 0 blk page64 (2 * cm + 1)
        return (cd * kprmshits + c)
      phi0Loop !i !acc
        | i >= ysz = return acc
        | otherwise = do
            fc <- peekElemOff fctrs i
            let !f = fi fc :: Int
            if f <= 1
              then phi0Loop (i + 1) acc
              else do
                t <- tinyPhi (limW `quot` fi (2 * i + 1))
                phi0Loop (i + 1) (acc - ((f .&. 1) * 2 - 1) * t)
  phi0 <- phi0Loop 0 (0 :: Int)

  -- ---- reverse B-prime generator: primes in (y, sqrt x], largest first, yielding limit div p ----
  let !hindx     = toWhlNdx sqrtlmt
      !bufhisz   = ((hindx + 512) .&. (-512)) `unsafeShiftR` 3
      !buflowhli = if sievesz >= fullsievesz then 0
                   else (toWhlNdx y .&. (-512)) `unsafeShiftR` 3
      !svrng     = bufhisz - buflowhli
      !bgsz      = min pageBytes svrng
      !numbufpgs = (svrng + bgsz - 1) `quot` bgsz
      !strtwhli  = buflowhli + (numbufpgs - 1) * bgsz
      !histrtbpi = hindx - (strtwhli `unsafeShiftL` 3)
  gbuf <- mallocBytes bgsz :: IO (Ptr Word8)
  gst  <- callocBytes (4 * 8) :: IO (Ptr Int)   -- 0: page low index, 1: next bit index, 2: done, 3: count
  let prepB :: Int -> IO ()
      prepB lwG = do
        let !frstvalG = lwG * 30
            !nxtvalG  = if lwG >= strtwhli then bufhisz * 30 else frstvalG + bgsz * 30
            !bpiszG   = piOf (min (isqrtI (nxtvalG - 1)) y) - k
            !cullsz   = if lwG >= strtwhli then bufhisz - lwG else bgsz
        fillPage gbuf ptrn prodpc bgsz lwG
        let cl !bpi
              | bpi >= bpiszG = return ()
              | otherwise = do
                  bpW <- peekElemOff yprms bpi
                  let !bp = fi bpW :: Int
                  _ <- cullPage 0 gbuf nullPtr lwG bp cullsz (bp * bp) (toWhlNdx bp .&. 7)
                  cl (bpi + 1)
        cl 0
      finishB = pokeElemOff gst 2 1 >> return 0
      nextB :: IO Int
      nextB = do
        dn <- peekElemOff gst 2
        if dn /= 0 then return 0 else scanB
      scanB :: IO Int
      scanB = do
        bpi <- peekElemOff gst 1
        if bpi < 0
          then do
            lwG <- peekElemOff gst 0
            let !lw' = lwG - bgsz
            if lw' < buflowhli
              then finishB
              else do
                pokeElemOff gst 0 lw'
                prepB lw'
                pokeElemOff gst 1 (bgsz * 8 - 1)
                scanB
          else do
            b <- peekByteOff gbuf (bpi `unsafeShiftR` 3) :: IO Word8
            pokeElemOff gst 1 (bpi - 1)
            if (b `unsafeShiftR` (bpi .&. 7)) .&. 1 == 0
              then do
                lwG <- peekElemOff gst 0
                let !bp = lwG * 30 + ndxToPrm bpi
                if bp <= y
                  then finishB
                  else do
                    n <- peekElemOff gst 3
                    pokeElemOff gst 3 (n + 1)
                    return (lmtDiv bp)
              else scanB
  pokeElemOff gst 0 strtwhli
  pokeElemOff gst 1 histrtbpi
  prepB strtwhli
  bn0 <- nextB

  -- ---- running phi per base prime (last element = running pi) ----
  let !lstbasephisi = pilmtstar - k
  basephis <- callocBytes ((lstbasephisi + 1) * 8) :: IO (Ptr Int)
  pokeElemOff basephis lstbasephisi (k - 1)

  -- ---- the page loop ----
  let pageLoop :: Int -> Int -> Int -> Int -> Int -> Int -> IO (Int, Int, Int, Int)
      pageLoop !lw !bn !aacc !bacc !cacc !dacc
        | lw >= fullsievesz = return (aacc, bacc, cacc, dacc)
        | otherwise = do
            let !frstval        = lw * 30
                !nxtval         = frstval + sievesz * 30
                !grddfrstval    = max frstval 1
                !grddnxtval     = min nxtval lmtdivy
                !mps            = limW `quot` fi grddfrstval :: Word64
                !maxprm         = fi (isqrtW mps) :: Int
                !acsplit        = min nxtval sqrtlmt
                !lmtdivacsplit  = fi (limW `quot` fi acsplit) :: Int
                !pisqrtfrstval  = piOf (isqrtI grddfrstval)
                !pimaxc2        = piOf (min (lmtdivacsplit `quot` y) lmtstar)
                mpsq :: Int -> Int -> Int          -- min (mps div d) cap
                mpsq d cap = fi (min (mps `quot` fi d) (fi cap :: Word64))
            fillPage page ptrn prodpc sievesz lw
            curphi0 <- dInit lw blk chnk page64 nwords nxtval
            let !bpisz = piOf (isqrtI (min (nxtval - 1) lmtdivy)) - k
                dLoop !bpi !curphi !da
                  | bpi >= bpisz = return (curphi, da)
                  | otherwise = do
                      bpW <- peekElemOff yprms bpi
                      let !bp           = fi bpW :: Int
                          !lmtdivbplow  = mpsq bp y
                          !lmtdivbphigh = min (lmtDiv bp `quot` grddnxtval) y
                          !maxm         = min (lmtDiv bp `quot` bp `quot` bp) lmtdivbplow
                          !swpi         = toWhlNdx bp .&. 7
                      -- basephis has entries only for base primes <= lmtstar: read it only there
                      if bp <= sqrty
                        then do
                          bph <- peekElemOff basephis bpi
                          let !maxfctri   = toOddNdx maxm
                              !minfctri   = toOddNdx (max lmtdivbphigh (y `quot` bp))
                              !totprvphi  = bph - bpi
                          da' <- muLoopC limW fctrs (dQuery lw blk page64) pbuf bp maxfctri minfctri totprvphi da
                          pokeElemOff basephis bpi (bph + curphi)
                          eff <- dCull curphi lw page page64 chnk nblk bp sievesz swpi
                          dSync lw blk chnk page64 nwords nxtval
                          dLoop (bpi + 1) (curphi - eff) da'
                        else if bp <= lmtstar
                          then do
                            bph <- peekElemOff basephis bpi
                            let !maxp2i    = piOf maxm - k - 1
                                !minp2i    = piOf (min (max lmtdivbphigh bp) maxm) - k
                                !totprvphi = bph - bpi
                            da' <- pairLoop limW yprms (dQuery lw blk page64) bp maxp2i minp2i totprvphi da
                            pokeElemOff basephis bpi (bph + curphi)
                            eff <- dCull curphi lw page page64 chnk nblk bp sievesz swpi
                            dSync lw blk chnk page64 nwords (lmtDiv bp `quot` bp)
                            dLoop (bpi + 1) (curphi - eff) da'
                          else do
                            _ <- cullPage 0 page nullPtr lw bp sievesz (bp * bp) swpi
                            dLoop (bpi + 1) curphi da
            (curphi1, dacc1) <- dLoop 0 curphi0 dacc
            let adj !bpi
                  | bpi >= pilmtstar - k = return ()
                  | otherwise = do
                      v <- peekElemOff basephis bpi
                      pokeElemOff basephis bpi (v + curphi1)
                      adj (bpi + 1)
            adj bpisz
            syncBlk lw blk page64 nwords nxtval
            totprvpi <- peekElemOff basephis lstbasephisi

            -- C1: base primes <= sqrt y
            let c1Loop !bpi !ca
                  | bpi >= pisqrty - k = return ca
                  | otherwise = do
                      bpW <- peekElemOff yprms bpi
                      let !bp   = fi bpW :: Int
                          !maxm = min (mpsq bp y) (lmtDiv bp `quot` bp)
                          -- the third bound keeps the queried points inside THIS page (needed
                          -- once y exceeds one page's value range, i.e. x above about 2e17)
                          !minm = max (max (lmtDiv bp `quot` bp `quot` bp) (y `quot` bp))
                                      (lmtDiv bp `quot` grddnxtval)
                      ca' <- muLoopC limW fctrs (pntCnt lw blk page64) pbuf bp (toOddNdx maxm) (toOddNdx minm)
                                     (totprvpi - bpi - k + 1) ca
                      c1Loop (bpi + 1) ca'
            cacc1 <- c1Loop (picbrtlmtdivy - k) cacc

            -- C2: prime pairs
            let !maxp1isz = piOf (min maxprm lmtstar) - k
                !minp1i0  = max (max picbrtlmtdivy pisqrty) (max pisqrtfrstval pimaxc2) - k
                c2Loop !p1i !ca
                  | p1i >= maxp1isz = return ca
                  | otherwise = do
                      p1W <- peekElemOff yprms p1i
                      let !p1     = fi p1W :: Int
                          !maxp2  = min (mpsq p1 y) (lmtDiv p1 `quot` p1)
                          !maxp2i = piOf maxp2 - k - 1
                          !minp2  = min (max (max (lmtdivacsplit `quot` p1)
                                                  (lmtDiv p1 `quot` p1 `quot` p1)) p1) maxp2
                          !minp2i = piAtIdx (min (ysz - 1) (toOddNdx minp2)) - k
                      ca' <- pairLoop limW yprms (pntCnt lw blk page64) p1 maxp2i minp2i
                                      (totprvpi - p1i - k + 1) ca
                      c2Loop (p1i + 1) ca'
            cacc2 <- c2Loop minp1i0 cacc1

            -- A: complete once the pages reach sqrt x
            aacc1 <-
              if frstval <= sqrtlmt
                then do
                  let !minp1iA = piOf (max lmtstar (min (lmtdivacsplit `quot` acsplit) cbrtlmt)) - k
                      !p1isz   = piOf (min cbrtlmt maxprm) - k
                      aOuter !p1i !aa
                        | p1i >= p1isz = return aa
                        | otherwise = do
                            p1W <- peekElemOff yprms p1i
                            let !p1            = fi p1W :: Int
                                !lmtdivp1      = lmtDiv p1
                                !sqrtlmtdivp1  = isqrtI lmtdivp1
                                !maxp2         = min sqrtlmtdivp1 (mpsq p1 sqrtlmtdivp1)
                                !minp2         = min (lmtdivacsplit `quot` p1) sqrtlmtdivp1
                                !maxp2i        = piOf maxp2 - k - 1
                                !minp2i        = piOf (max minp2 p1) - k
                                !splitsz       = piOf (max (max (lmtdivp1 `quot` y) minp2) p1) - k
                                !aftspliti     = min (splitsz - 1) maxp2i
                            aa1 <- aLoop yprms blk page64 lw lmtdivp1 2 totprvpi maxp2i splitsz aa
                            aa2 <- aLoop yprms blk page64 lw lmtdivp1 1 totprvpi aftspliti minp2i aa1
                            aOuter (p1i + 1) aa2
                  aOuter minp1iA aacc
                else return aacc

            -- B: reverse primes whose point lies in this page
            let bLoop !b !ba
                  | b /= 0 && b < nxtval = do
                      c <- pntCnt lw blk page64 b
                      b' <- nextB
                      bLoop b' (ba + totprvpi + c)
                  | otherwise = return (b, ba)
            (bn', bacc1) <- bLoop bn bacc

            tot <- peekElemOff blk nwords
            pokeElemOff basephis lstbasephisi (totprvpi + fi tot)
            pageLoop (lw + sievesz) bn' aacc1 bacc1 cacc2 dacc1

  (aacc, bacc, cacc, dacc) <- pageLoop 0 bn0 0 0 0 0

  -- ---- Sigma ----
  numrevb <- peekElemOff gst 3
  let !pisqrtlmt = piy + numrevb
      !a = piy; !b = picbrtlmt; !c = pisqrtlmtdivy; !d = pilmtstar; !e = pisqrtlmt
      !s0 = a - 1 + (e * (e - 1)) `quot` 2 - (a * (a - 1)) `quot` 2
      !s1 = ((a - b) * (a - b - 1)) `quot` 2
      !s2 = a * (b - c - (c * (c - 3)) `quot` 2 + (d * (d - 3)) `quot` 2)
      !s3 = (b * (b - 1) * (b + b - 1)) `quot` 6 - b - (d * (d - 1) * (d + d - 1)) `quot` 6 + d
      yp i = fi (unsafeDupablePerformIO (peekElemOff yprms i)) :: Int
      sum4 !i !acc | i >= c - k = acc
                   | otherwise  = sum4 (i + 1) (acc + piOf (lmtDiv (yp i) `quot` y))
      sum5 !i !acc | i >= b - k = acc
                   | otherwise  = sum5 (i + 1) (acc + piOf (lmtDiv (yp i) `quot` yp i))
      sum6 !i !acc | i >= b - k = acc
                   | otherwise  = let !q = piOf (isqrtI (lmtDiv (yp i))) in sum6 (i + 1) (acc - q * q)
      !s4 = a * sum4 (d - k) 0
      !s5 = sum5 (c - k) 0
      !s6 = sum6 (d - k) 0
      !sigma = s0 + s1 + s2 + s3 + s4 + s5 + s6
      !result = aacc - bacc + cacc + dacc + phi0 + sigma

  free tm
  free fctrs; free piys; free yprms; free ptrn; free page; free blk; free chnk
  free pbuf
  free gbuf; free gst; free basephis
  return (fi result)

bigCountT :: Word64 -> Double -> Int -> IO Int64
bigCountT limW alpha nthr = do
  let -- ---- derived constants (all fit Int; limit itself stays Word64) ----
      !sqrtlmtW    = isqrtW limW
      !sqrtlmt     = fi sqrtlmtW :: Int
      !cbrtlmt     = fi (icbrtW limW) :: Int
      !yraw        = truncate (alpha * fi cbrtlmt) :: Int
      !y           = min sqrtlmt (max yraw (max (cbrtlmt + 2) 64))
      !ysz         = toOddNdx y + 1
      !sqrty       = isqrtI y
      !sqrtysz     = toOddNdx sqrty + 1
      !lmtdivyW    = limW `quot` fi y
      !lmtdivy     = fi lmtdivyW :: Int
      !sqrtlmtdivy = fi (isqrtW lmtdivyW) :: Int
      !sqrtsqrtlmt = fi (isqrtW sqrtlmtW) :: Int
      !lmtstar     = max sqrtsqrtlmt (fi (lmtdivyW `quot` fi y)) :: Int
      !blkbits     = 64 * blkWords
      !maxsieve    = toWhlNdx lmtdivy + blkbits
      !sievebitsz  = maxsieve - (maxsieve `rem` blkbits)
      !fullsievesz = sievebitsz `quot` 8
      !sievesz     = min pageBytes fullsievesz
      !nwords      = sievesz `quot` 8
      !nblk        = nwords `unsafeShiftR` blkShift
      pcAll        = takeWhile (\p -> (fi p :: Word64) * fi p <= limW) [7, 11, 13, 17, 19 :: Int]
      preCull      = map fst (takeWhile (\(_, pr) -> pr <= sievesz) (zip pcAll (scanl1 (*) pcAll)))
      !prodpc      = product preCull :: Int
      !k           = length preCull + 3

  tm <- callocBytes (32 * 8) :: IO (Ptr Int)
  -- ---- factor (Moebius / least prime factor) table over odd numbers up to y ----
  fctrs <- mallocBytes (ysz * 2) :: IO (Ptr Word16)
  fillBytes fctrs 0xFF (ysz * 2)
  let zeroFor bp = do { pokeElemOff fctrs (toOddNdx bp) 0
                      ; strideZero16 fctrs (toOddNdx (bp * bp)) bp ysz }
  mapM_ zeroFor ([3, 5] ++ preCull)
  let lowPrimes !bpi
        | bpi >= sqrtysz = return ()
        | otherwise = do
            v <- peekElemOff fctrs bpi
            if v >= 0xFFFF
              then do
                let !bp  = 2 * bpi + 1
                    !sqr = bp * bp
                    mark !ci
                      | ci >= ysz = return ()
                      | otherwise = do
                          c <- peekElemOff fctrs ci
                          pokeElemOff fctrs ci (if c >= 0xFFFF then fi bp else c `xor` 1)
                          mark (ci + bp)
                mark (toOddNdx bp + bp)
                strideZero16 fctrs (toOddNdx sqr) sqr ysz
                lowPrimes (bpi + 1)
              else lowPrimes (bpi + 1)
  lowPrimes 1
  let highPrimes !bpi
        | bpi >= ysz = return ()
        | otherwise = do
            v <- peekElemOff fctrs bpi
            if v >= 0xFFFF
              then do
                let !bp = 2 * bpi + 1
                strideXor16 fctrs (toOddNdx bp + bp) bp ysz
                highPrimes (bpi + 1)
              else highPrimes (bpi + 1)
  highPrimes sqrtysz
  pokeElemOff fctrs 0 0xFFFE   -- the "one" is special: not a prime

  -- ---- pi table over odd numbers (piys) and the prime array up to y (yprms) ----
  piys <- mallocBytes (ysz * 4) :: IO (Ptr Word32)
  let mkPi !i !cnt
        | i >= ysz = return cnt
        | otherwise = do
            v <- peekElemOff fctrs i
            let !cnt' = if v >= 0xFFFF then cnt + 1 else cnt
            pokeElemOff piys i (fi cnt')
            mkPi (i + 1) cnt'
  piyTotal <- mkPi 0 k
  yprms <- mallocBytes (max 4 ((piyTotal - k) * 4)) :: IO (Ptr Word32)
  let mkPrms !i
        | i >= ysz = return ()
        | otherwise = do
            v <- peekElemOff fctrs i
            if v >= 0xFFFF
              then do
                pc <- peekElemOff piys i
                pokeElemOff yprms (fi pc - k - 1) (fi (2 * i + 1))
                mkPrms (i + 1)
              else mkPrms (i + 1)
  mkPrms 0

  let -- read-only table lookups: the table is complete before any of these is forced
      piAtIdx :: Int -> Int
      piAtIdx i = fi (unsafeDupablePerformIO (peekElemOff piys i))
      piOf :: Int -> Int
      piOf n = piAtIdx (toOddNdx n)
      !piy            = piyTotal
      !picbrtlmt      = piOf cbrtlmt
      !pisqrtlmtdivy  = piOf sqrtlmtdivy
      !pilmtstar      = piOf lmtstar
      !pisqrty        = piOf sqrty
      !picbrtlmtdivy  = max k (piOf (icbrtI lmtdivy))
      lmtDiv :: Int -> Int          -- limit div d  (d >= 7 here, so the result fits Int)
      lmtDiv d = fi (limW `quot` fi d)

  -- ---- the pre-cull pattern, page buffers ----
  let !patLen = prodpc + 16384 + 64
  ptrn <- callocBytes patLen :: IO (Ptr Word8)
  mapM_ (\p -> cullPage 0 ptrn nullPtr 0 p patLen p 0) preCull
  page <- mallocBytes sievesz :: IO (Ptr Word8)
  let !page64 = castPtr page :: Ptr Word64
  blk <- mallocBytes ((nwords + 1) * 4) :: IO (Ptr Word32)   -- block prefix counts (D) / word prefix counts (C, A, B)
  pbuf <- mallocBytes (compactSize * 8) :: IO (Ptr Int)        -- compacted muLoop indices
  chnk <- mallocBytes (nblk * 2) :: IO (Ptr Word16)           -- per-block survivor counts (delta counts)

  -- ---- Phi0 (pattern holds all pre-cull primes, so tinyphi is one lookup) ----
  fillPage page ptrn prodpc sievesz 0
  syncBlk 0 blk page64 nwords (sievesz * 30)
  let !kprmscirc = prodpc * 15
      !kprmshits = product (map (subtract 1) preCull) * 8 :: Int
      tinyPhi :: Word64 -> IO Int
      tinyPhi v = do
        let !oxndv = fi ((v - 1) `unsafeShiftR` 1) :: Int
            !cd    = oxndv `quot` kprmscirc
            !cm    = oxndv - cd * kprmscirc
        c <- pntCnt 0 blk page64 (2 * cm + 1)
        return (cd * kprmshits + c)
      phi0Loop !i !acc
        | i >= ysz = return acc
        | otherwise = do
            fc <- peekElemOff fctrs i
            let !f = fi fc :: Int
            if f <= 1
              then phi0Loop (i + 1) acc
              else do
                t <- tinyPhi (limW `quot` fi (2 * i + 1))
                phi0Loop (i + 1) (acc - ((f .&. 1) * 2 - 1) * t)
  phi0 <- phi0Loop 0 (0 :: Int)

  free page; free blk; free pbuf; free chnk

  -- ======== THREADED DRIVER ========
  -- Every page (or chunk of a page) is an independent TASK computed with the carried values (basephis, running pi)
  -- replaced by ZERO.  It returns a partial sum plus the coefficients by which the carried values must be
  -- multiplied; the main thread merges the tasks in page order (see the handoff).
  let !nb       = pilmtstar - k                        -- base primes <= lmtstar (those that carry a phi)
      !nbs      = max 1 nb
      !npages   = (fullsievesz + sievesz - 1) `quot` sievesz
      !wsz      = max 2 (min npages (8 * nthr))   -- pages whose result arrays are alive at once
      !c1lo     = picbrtlmtdivy - k
      !nC1      = max 0 (pisqrty - k - c1lo)
      !maxbpisz = max 0 (piOf (isqrtI lmtdivy) - k)
  -- pcum[i] = estimated cost (cycles) of plain-culling base primes 0 .. i-1 from a page (planner only)
  pcumP <- mallocBytes ((maxbpisz + 2) * 8) :: IO (Ptr Double)
  let fillPcum :: Int -> Double -> IO ()
      fillPcum !i !acc = do
        pokeElemOff pcumP i acc
        if i >= maxbpisz
          then return ()
          else do
            bpW <- peekElemOff yprms i
            fillPcum (i + 1) (acc + 8 * fi sievesz / fi bpW * 1.2)
  fillPcum 0 0

  let ypAt :: Int -> Int
      ypAt i = fi (unsafeDupablePerformIO (peekElemOff yprms i)) :: Int
      preAt :: Int -> Double
      preAt b = unsafeDupablePerformIO (peekElemOff pcumP (max 0 (min b maxbpisz)))

      pgOf :: Int -> PG
      pgOf lw =
        let !frstval        = lw * 30
            !nxtval         = frstval + sievesz * 30
            !grddfrstval    = max frstval 1
            !grddnxtval     = min nxtval lmtdivy
            !mps            = limW `quot` fi grddfrstval :: Word64
            !maxprm         = fi (isqrtW mps) :: Int
            !acsplit        = min nxtval sqrtlmt
            !lmtdivacsplit  = fi (limW `quot` fi acsplit) :: Int
            !pisqrtfrstval  = piOf (isqrtI grddfrstval)
            !pimaxc2        = piOf (min (lmtdivacsplit `quot` y) lmtstar)
            !bpisz          = piOf (isqrtI (min (nxtval - 1) lmtdivy)) - k
            !maxp1isz       = piOf (min maxprm lmtstar) - k
            !minp1i0        = max (max picbrtlmtdivy pisqrty) (max pisqrtfrstval pimaxc2) - k
            minp1iA         = piOf (max lmtstar (min (lmtdivacsplit `quot` acsplit) cbrtlmt)) - k
            p1isz           = piOf (min cbrtlmt maxprm) - k
            !nA             = if frstval <= sqrtlmt then max 0 (p1isz - minp1iA) else 0
        in PG frstval nxtval grddnxtval mps lmtdivacsplit bpisz minp1i0 (max 0 (maxp1isz - minp1i0)) minp1iA nA

      mpsqP :: PG -> Int -> Int -> Int           -- min (mps div d) cap
      mpsqP pg d cap = fi (min (pgMps pg `quot` fi d) (fi cap :: Word64))

      -- ---- the index bounds of every query loop (exactly those of the single-thread page loop) ----
      dB1 :: PG -> Int -> (Int, Int)             -- (maxfctri, minfctri), bp <= sqrt y
      dB1 pg bp =
        let !low  = mpsqP pg bp y
            !high = min (lmtDiv bp `quot` pgGrddNxt pg) y
            !maxm = min (lmtDiv bp `quot` bp `quot` bp) low
        in (toOddNdx maxm, toOddNdx (max high (y `quot` bp)))
      dB2 :: PG -> Int -> (Int, Int)             -- (maxp2i, minp2i), sqrt y < bp <= lmtstar
      dB2 pg bp =
        let !low  = mpsqP pg bp y
            !high = min (lmtDiv bp `quot` pgGrddNxt pg) y
            !maxm = min (lmtDiv bp `quot` bp `quot` bp) low
        in (piOf maxm - k - 1, piOf (min (max high bp) maxm) - k)
      c1B :: PG -> Int -> (Int, Int)             -- (hiI, loI)
      c1B pg bp =
        let !maxm = min (mpsqP pg bp y) (lmtDiv bp `quot` bp)
            !minm = max (max (lmtDiv bp `quot` bp `quot` bp) (y `quot` bp)) (lmtDiv bp `quot` pgGrddNxt pg)
        in (toOddNdx maxm, toOddNdx minm)
      c2B :: PG -> Int -> (Int, Int)             -- (maxp2i, minp2i)
      c2B pg p1 =
        let !maxp2 = min (mpsqP pg p1 y) (lmtDiv p1 `quot` p1)
            !minp2 = min (max (max (pgLda pg `quot` p1) (lmtDiv p1 `quot` p1 `quot` p1)) p1) maxp2
        in (piOf maxp2 - k - 1, piAtIdx (min (ysz - 1) (toOddNdx minp2)) - k)
      aB :: PG -> Int -> (Int, Int, Int, Int, Int)   -- (lmtdivp1, maxp2i, splitsz, aftspliti, minp2i)
      aB pg p1 =
        let !lmtdivp1 = lmtDiv p1
            !sq       = isqrtI lmtdivp1
            !maxp2    = min sq (mpsqP pg p1 sq)
            !minp2    = min (pgLda pg `quot` p1) sq
            !maxp2i   = piOf maxp2 - k - 1
            !minp2i   = piOf (max minp2 p1) - k
            !splitsz  = piOf (max (max (lmtdivp1 `quot` y) minp2) p1) - k
        in (lmtdivp1, maxp2i, splitsz, min (splitsz - 1) maxp2i, minp2i)
      bLoHi :: PG -> (Int, Int)                  -- B primes of the page are the primes in (lo, hi]
      bLoHi pg = ( max y (fi (limW `quot` fi (pgNxt pg)))
                 , if pgFrst pg == 0 then sqrtlmt else min sqrtlmt (fi (limW `quot` fi (pgFrst pg))) )

      -- ---- planner: estimated cost (cycles) of the work units of a page ----
      -- units: D base primes 0 .. bpisz-1, then C1, C2, A items, then B (one unit)
      cQD1 = 50 :: Double
      cQD2 = 52 :: Double
      cQC1 = 34 :: Double
      cQC2 = 10 :: Double
      cQA  = 11 :: Double
      cQB  = 11 :: Double
      cScan  = 3 :: Double
      cCullC = 2 :: Double
      cSync  = fi nwords / 8 :: Double
      baseC = fi sievesz * 0.05 + 2 * fi nwords * 0.5 :: Double
      unitCosts :: PG -> [Double]
      unitCosts pg =
        let !nD  = pgBpisz pg
            !nbE = min nD nb
            dC bpi =
              let !bp = ypAt bpi
                  !stepsC = 8 * fi sievesz / fi bp :: Double
              in if bp <= sqrty
                   then let (mx, mn) = dB1 pg bp
                            !sc = fi (max 0 (mx - mn)) :: Double
                        in sc * cScan + sc * 0.15 * cQD1 + stepsC * cCullC + cSync
                   else let (mx, mn) = dB2 pg bp
                            !q = fi (max 0 (mx - mn + 1)) :: Double
                        in q * cQD2 + stepsC * cCullC + cSync
            c1C i =
              let (mx, mn) = c1B pg (ypAt (c1lo + i))
                  !sc = fi (max 0 (mx - mn)) :: Double
              in sc * cScan + sc * 0.15 * cQC1
            c2C i =
              let (mx, mn) = c2B pg (ypAt (pgMinp1C2 pg + i))
              in fi (max 0 (mx - mn + 1)) * cQC2 :: Double
            aC i =
              let (_, mx, spl, aft, mn) = aB pg (ypAt (pgMinp1A pg + i))
              in fi (2 * max 0 (mx - spl + 1) + max 0 (aft - mn + 1)) * cQA :: Double
            bC = let (lo, hi) = bLoHi pg
                 in if hi <= lo then 0 else fi (hi - lo) * (cQB / log (max 2 (fi hi)) + 0.25) :: Double
        in [ dC i | i <- [0 .. nbE - 1] ] ++ [ preAt (i + 1) - preAt i | i <- [nbE .. nD - 1] ]
           ++ [ c1C i | i <- [0 .. nC1 - 1] ] ++ [ c2C i | i <- [0 .. pgNC2 pg - 1] ]
           ++ [ aC i | i <- [0 .. pgNA pg - 1] ] ++ [bC]

      -- cut the units of a page into chunks of about `tgt` cycles; a chunk starting at unit s must first
      -- plain-cull the D base primes below s (that redundant cost is included)
      cut :: Double -> Int -> Int -> [Double] -> [(Int, Int)]
      cut tgt nD tot = go 0 (baseC + preAt 0) (0 :: Double) 0
        where
          go :: Int -> Double -> Double -> Int -> [Double] -> [(Int, Int)]
          go !s _ _ _ [] = [(s, tot)]
          go !s !cur !own !u (c : cs)
            | cur + c > tgt && own > 0 = (s, u) : go u (baseC + preAt (min u nD)) 0 u (c : cs)
            | otherwise                = go s (cur + c) (own + c) (u + 1) cs

      planPage :: Double -> Int -> PG -> [Double] -> [(Int, Int, Int)]
      planPage tgt0pp p pg ucs =
        let !nD   = pgBpisz pg
            !tot  = length ucs
            !cost = baseC + sum ucs
            !tgt  = max tgt0pp (3 * (baseC + preAt nD))
        in if cost <= tgt
             then [(p, 0, maxBound)]
             else [ (p, s, if e >= tot then maxBound else e) | (s, e) <- cut tgt nD tot ucs ]

      !nLow     = min npages (sqrtlmt `quot` (sievesz * 30) + 1)    -- pages that hold A queries
      lowPGs    = [ pgOf (p * sievesz) | p <- [0 .. nLow - 1] ]
      lowUCs    = map unitCosts lowPGs
      cHigh0    = if nLow < npages then baseC + sum (unitCosts (pgOf (nLow * sievesz))) else 0 :: Double
      totEst    = sum [ baseC + sum ucs | ucs <- lowUCs ] + fi (npages - nLow) * cHigh0
      tgt0      = totEst / (fi nthr * 4)
      allTasks  = concat [ planPage tgt0 p pg ucs | (p, pg, ucs) <- zip3 [0 ..] lowPGs lowUCs ]
                  ++ [ (p, 0, maxBound) | p <- [nLow .. npages - 1] ]
      !ntasks   = length allTasks

  -- ---- one task: page `pgi`, units u0 .. u1-1 ----
  let cullRange :: Ptr Word8 -> Int -> Int -> Int -> IO ()
      cullRange pgb lw i0 i1 = go i0
        where
          go !i
            | i >= i1 = return ()
            | otherwise = do
                bpW <- peekElemOff yprms i
                let !bp = fi bpW :: Int
                _ <- cullPage 0 pgb nullPtr lw bp sievesz (bp * bp) (toWhlNdx bp .&. 7)
                go (i + 1)

      -- D over base primes bpi .. end-1 (all with bp <= lmtstar), zero carried; returns (curphi, dacc)
      dLoopZ :: WBuf -> Int -> PG -> Ptr Int -> Ptr Int -> Int -> Int -> Int -> Int -> IO (Int, Int)
      dLoopZ wb lw pg dcoefP incP = go
        where
          !nxtval  = pgNxt pg
          wpage    = wbPage wb
          wpage64  = castPtr wpage :: Ptr Word64
          wblk     = wbBlk wb
          wchnk    = wbChnk wb
          go !bpi !end !curphi !da
            | bpi >= end = return (curphi, da)
            | otherwise = do
                bpW <- peekElemOff yprms bpi
                let !bp   = fi bpW :: Int
                    !swpi = toWhlNdx bp .&. 7
                if bp <= sqrty
                  then do
                    let (!mx, !mn) = dB1 pg bp
                    da' <- muLoopCZ limW fctrs (dQuery lw wblk wpage64) (wbPbuf wb) (wbSg wb) bp mx mn (negate bpi) da
                    sgs <- peekElemOff (wbSg wb) 0
                    pokeElemOff dcoefP bpi sgs
                    pokeElemOff incP bpi curphi
                    eff <- dCull curphi lw wpage wpage64 wchnk nblk bp sievesz swpi
                    dSync lw wblk wchnk wpage64 nwords nxtval
                    go (bpi + 1) end (curphi - eff) da'
                  else do
                    let (!mx, !mn) = dB2 pg bp
                    da' <- pairLoop limW yprms (dQuery lw wblk wpage64) bp mx mn (negate bpi) da
                    pokeElemOff dcoefP bpi (max 0 (mx - mn + 1))
                    pokeElemOff incP bpi curphi
                    eff <- dCull curphi lw wpage wpage64 wchnk nblk bp sievesz swpi
                    dSync lw wblk wchnk wpage64 nwords (lmtDiv bp `quot` bp)
                    go (bpi + 1) end (curphi - eff) da'

      -- B items of a page: primes in (lo, hi] are sieved in windows of at most about one page; returns
      -- (sum of counts, number of primes)
      bItem :: WBuf -> Int -> Int -> Int -> IO (Int, Int)
      bItem wb lw lo hi = go lo 0 0
        where
          !maxw    = 30 * (sievesz + 56)
          gb       = wbGbuf wb
          wpage64  = castPtr (wbPage wb) :: Ptr Word64
          wblk     = wbBlk wb
          go !a !cs !np
            | a >= hi = return (cs, np)
            | otherwise = do
                let !b      = min hi (a + maxw)
                    !lwG    = (a + 1) `quot` 30
                    !sz     = b `quot` 30 - lwG + 1
                    !bpiszG = piOf (isqrtI b) - k
                fillPage gb ptrn prodpc sz lwG
                let cl !bpi
                      | bpi >= bpiszG = return ()
                      | otherwise = do
                          bpW <- peekElemOff yprms bpi
                          let !bp = fi bpW :: Int
                          _ <- cullPage 0 gb nullPtr lwG bp sz (bp * bp) (toWhlNdx bp .&. 7)
                          cl (bpi + 1)
                cl 0
                let byteLoop !j !cs1 !np1
                      | j >= sz = return (cs1, np1)
                      | otherwise = do
                          w <- peekByteOff gb j :: IO Word8
                          (cs2, np2) <- bitLoop ((lwG + j) * 30) (complement w) cs1 np1
                          byteLoop (j + 1) cs2 np2
                    bitLoop !base !m !cs1 !np1
                      | m == 0 = return (cs1, np1)
                      | otherwise = do
                          let !pos = countTrailingZeros m
                              !v   = base + whlRsd pos
                              !m'  = m .&. (m - 1)
                          if v > a && v <= b
                            then do c <- pntCnt lw wblk wpage64 (lmtDiv v)
                                    bitLoop base m' (cs1 + c) (np1 + 1)
                            else bitLoop base m' cs1 np1
                (cs', np') <- byteLoop 0 cs np
                go b cs' np'

      runTask :: WBuf -> Int -> Int -> Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO ()
      runTask wb pgi u0 u1 dcoefP incP sp = do
        let !lw       = pgi * sievesz
            !pg       = pgOf lw
            !nxtval   = pgNxt pg
            !nD       = pgBpisz pg
            !nbE      = min nD nb
            !nC2      = pgNC2 pg
            !nA       = pgNA pg
            !total    = nD + nC1 + nC2 + nA + 1
            !isLast   = u1 >= total
            !b0       = min u0 nD
            !b1       = min u1 nD
            wpage     = wbPage wb
            wpage64   = castPtr wpage :: Ptr Word64
            wblk      = wbBlk wb
            wchnk     = wbChnk wb
        fillPage wpage ptrn prodpc sievesz lw
        if u1 <= nD
          then
            -- only D base primes b0 .. b1-1 (the page itself is not needed afterwards)
            when (b0 < nbE) $ do
              cullRange wpage lw 0 b0
              cphi0 <- dInit lw wblk wchnk wpage64 nwords nxtval
              (_, da) <- dLoopZ wb lw pg dcoefP incP b0 (min b1 nbE) cphi0 0
              pokeElemOff sp 0 da
          else do
            -- this chunk needs the fully sieved page for its C / A / B items
            cullRange wpage lw 0 (min b0 nbE)
            curphi1 <-
              if b0 < nbE
                then do
                  cphi0 <- dInit lw wblk wchnk wpage64 nwords nxtval
                  (cp, da) <- dLoopZ wb lw pg dcoefP incP b0 nbE cphi0 0
                  pokeElemOff sp 0 da
                  return cp
                else if isLast then countUnset wpage64 nwords else return 0
            cullRange wpage lw nbE nD
            when (isLast && nD < nb) $ do
              let tailLoop !i
                    | i >= nb = return ()
                    | otherwise = pokeElemOff incP i curphi1 >> tailLoop (i + 1)
              tailLoop nD
            syncBlk lw wblk wpage64 nwords nxtval
            when isLast $ do
              tot <- peekElemOff wblk nwords
              pokeElemOff sp 3 (fi tot)
            let !i0   = max 0 (u0 - nD)
                !i1   = min (u1 - nD) (nC1 + nC2 + nA + 1)
                !c1a  = i0
                !c1b  = max 0 (min i1 nC1)
                !c2a  = max 0 (i0 - nC1)
                !c2b  = max 0 (min i1 (nC1 + nC2) - nC1)
                !aa0  = max 0 (i0 - nC1 - nC2)
                !ab0  = max 0 (min i1 (nC1 + nC2 + nA) - nC1 - nC2)
                !doB  = i0 <= nC1 + nC2 + nA && i1 > nC1 + nC2 + nA
                -- C1: zero carried; coefficient = sum of the signs
                c1Loop !i !ca !cc
                  | i >= c1b = return (ca, cc)
                  | otherwise = do
                      let !bpi = c1lo + i
                      bpW <- peekElemOff yprms bpi
                      let !bp = fi bpW :: Int
                          (!mx, !mn) = c1B pg bp
                      ca' <- muLoopCZ limW fctrs (pntCnt lw wblk wpage64) (wbPbuf wb) (wbSg wb) bp mx mn
                                      (negate bpi - k + 1) ca
                      sgs <- peekElemOff (wbSg wb) 0
                      c1Loop (i + 1) ca' (cc + sgs)
                -- C2: coefficient = number of queries
                c2Loop !i !ca !cc
                  | i >= c2b = return (ca, cc)
                  | otherwise = do
                      let !p1i = pgMinp1C2 pg + i
                      p1W <- peekElemOff yprms p1i
                      let !p1 = fi p1W :: Int
                          (!mxi, !mni) = c2B pg p1
                      ca' <- pairLoop limW yprms (pntCnt lw wblk wpage64) p1 mxi mni (negate p1i - k + 1) ca
                      c2Loop (i + 1) ca' (cc + max 0 (mxi - mni + 1))
                -- A: coefficient = sum of the weights
                aLoopU !i !aa !ac
                  | i >= ab0 = return (aa, ac)
                  | otherwise = do
                      let !p1i = pgMinp1A pg + i
                      p1W <- peekElemOff yprms p1i
                      let !p1 = fi p1W :: Int
                          (!lp1, !mxi, !spl, !aft, !mni) = aB pg p1
                      aa1 <- aLoop yprms wblk wpage64 lw lp1 2 0 mxi spl aa
                      aa2 <- aLoop yprms wblk wpage64 lw lp1 1 0 aft mni aa1
                      aLoopU (i + 1) aa2 (ac + 2 * max 0 (mxi - spl + 1) + max 0 (aft - mni + 1))
            (ca1, cc1) <- c1Loop c1a 0 0
            (ca2, cc2) <- c2Loop c2a ca1 cc1
            (aa3, ac3) <- aLoopU aa0 0 0
            (bs, bn)   <- if doB
                            then let (lo, hi) = bLoHi pg in bItem wb lw lo hi
                            else return (0, 0)
            pokeElemOff sp 1 (ca2 + aa3 - bs)
            pokeElemOff sp 2 (cc2 + ac3 - bn)
            pokeElemOff sp 4 bn

  -- ---- plan, buffers, workers ----
  tbuf <- mallocBytes (ntasks * 3 * 8) :: IO (Ptr Int)
  let pokeT :: Int -> [(Int, Int, Int)] -> IO ()
      pokeT _ [] = return ()
      pokeT !i ((p, s, e) : r) = do
        pokeElemOff tbuf (3 * i) p
        pokeElemOff tbuf (3 * i + 1) s
        pokeElemOff tbuf (3 * i + 2) e
        pokeT (i + 1) r
  pokeT 0 allTasks
  pfirst <- mallocBytes ((npages + 1) * 8) :: IO (Ptr Int)
  let mkFirst :: Int -> Int -> IO ()
      mkFirst !t !lastp
        | t >= ntasks = pokeElemOff pfirst npages ntasks
        | otherwise = do
            p <- peekElemOff tbuf (3 * t)
            if p /= lastp
              then pokeElemOff pfirst p t >> mkFirst (t + 1) p
              else mkFirst (t + 1) lastp
  mkFirst 0 (-1)
  slots   <- callocBytes (ntasks * 64) :: IO (Ptr Int)         -- 0 dpart 1 rpart 2 rcoef 3 tot 4 numB
  dcoefB  <- callocBytes (wsz * nbs * 8) :: IO (Ptr Int)
  incB    <- callocBytes (wsz * nbs * 8) :: IO (Ptr Int)
  carried <- callocBytes (nbs * 8) :: IO (Ptr Int)
  dones   <- mapM (const newEmptyMVar) [1 .. ntasks] :: IO [MVar ()]
  let !doneArr = A.listArray (0, ntasks - 1) dones :: A.Array Int (MVar ())
  nextRef  <- newIORef (0 :: Int)
  errRef   <- newIORef (Nothing :: Maybe SomeException)
  mergedTV <- newTVarIO (0 :: Int)
  abortTV  <- newTVarIO False

  let !gcap = sievesz + 64
      slotP :: Ptr Int -> Int -> Ptr Int
      slotP base pgi = base `plusPtr` ((pgi `rem` wsz) * nbs * 8)
      worker :: Int -> IO ()
      worker _ = do
        wpage <- mallocBytes sievesz :: IO (Ptr Word8)
        wblk  <- mallocBytes ((nwords + 1) * 4) :: IO (Ptr Word32)
        wpbuf <- mallocBytes (compactSize * 8) :: IO (Ptr Int)
        wchnk <- mallocBytes (nblk * 2) :: IO (Ptr Word16)
        wgbuf <- mallocBytes gcap :: IO (Ptr Word8)
        wsg   <- callocBytes 8 :: IO (Ptr Int)
        let wb = WBuf wpage wblk wpbuf wchnk wgbuf wsg
            loop = do
              t <- atomicModifyIORef' nextRef (\i -> (i + 1, i))
              when (t < ntasks) $ do
                pgi <- peekElemOff tbuf (3 * t)
                u0  <- peekElemOff tbuf (3 * t + 1)
                u1  <- peekElemOff tbuf (3 * t + 2)
                -- do not run ahead of the merge by more than the ring of result arrays
                atomically $ do
                  m  <- readTVar mergedTV
                  ab <- readTVar abortTV
                  when (not ab && m + wsz <= pgi) retry
                ab <- readTVarIO abortTV
                unless ab $ do
                  r <- try (runTask wb pgi u0 u1 (slotP dcoefB pgi) (slotP incB pgi) (slots `plusPtr` (t * 64)))
                  case r of
                    Left e   -> writeIORef errRef (Just (e :: SomeException)) >> atomically (writeTVar abortTV True)
                    Right () -> return ()
                putMVar (doneArr A.! t) ()
                loop
        loop `finally` (free wpage >> free wblk >> free wpbuf >> free wchnk >> free wgbuf >> free wsg)
  wdones <- mapM (\wi -> do { mv <- newEmptyMVar
                            ; _ <- forkIO (worker wi `finally` putMVar mv ())
                            ; return mv }) [0 .. nthr - 1]

  -- ---- ordered merge on this thread ----
  let mergeLoop :: Int -> Int -> Int -> Int -> Int -> IO (Int, Int, Int)
      mergeLoop !p !dacc !racc !piRun !nbp
        | p >= npages = return (dacc, racc, nbp)
        | otherwise = do
            ta <- peekElemOff pfirst p
            tb <- peekElemOff pfirst (p + 1)
            let waitAll !t | t >= tb   = return ()
                           | otherwise = takeMVar (doneArr A.! t) >> waitAll (t + 1)
            waitAll ta
            er <- readIORef errRef
            case er of
              Nothing -> return ()
              Just e  -> do { mapM_ takeMVar wdones; throwIO e }
            let sumSl !t !s0 !s1 !s2 !s3 !s4
                  | t >= tb = return (s0, s1, s2, s3, s4)
                  | otherwise = do
                      a0 <- peekElemOff slots (8 * t)
                      a1 <- peekElemOff slots (8 * t + 1)
                      a2 <- peekElemOff slots (8 * t + 2)
                      a3 <- peekElemOff slots (8 * t + 3)
                      a4 <- peekElemOff slots (8 * t + 4)
                      sumSl (t + 1) (s0 + a0) (s1 + a1) (s2 + a2) (s3 + a3) (s4 + a4)
            (dp, rp, rc, tot, nbn) <- sumSl ta 0 0 0 0 0
            let !dcP = slotP dcoefB p
                !inP = slotP incB p
            dotc <- dotInts dcP carried nb
            foldInts carried inP nb
            fillBytes dcP 0 (nb * 8)
            atomically (writeTVar mergedTV (p + 1))
            mergeLoop (p + 1) (dacc + dp + dotc) (racc + rp + rc * piRun) (piRun + tot) (nbp + nbn)

  (dacc, racc, numrevb) <- mergeLoop 0 0 0 (k - 1) 0
  mapM_ takeMVar wdones

  -- ---- Sigma ----
  let !pisqrtlmt = piy + numrevb
      !a = piy; !b = picbrtlmt; !c = pisqrtlmtdivy; !d = pilmtstar; !e = pisqrtlmt
      !s0 = a - 1 + (e * (e - 1)) `quot` 2 - (a * (a - 1)) `quot` 2
      !s1 = ((a - b) * (a - b - 1)) `quot` 2
      !s2 = a * (b - c - (c * (c - 3)) `quot` 2 + (d * (d - 3)) `quot` 2)
      !s3 = (b * (b - 1) * (b + b - 1)) `quot` 6 - b - (d * (d - 1) * (d + d - 1)) `quot` 6 + d
      yp i = fi (unsafeDupablePerformIO (peekElemOff yprms i)) :: Int
      sum4 !i !acc | i >= c - k = acc
                   | otherwise  = sum4 (i + 1) (acc + piOf (lmtDiv (yp i) `quot` y))
      sum5 !i !acc | i >= b - k = acc
                   | otherwise  = sum5 (i + 1) (acc + piOf (lmtDiv (yp i) `quot` yp i))
      sum6 !i !acc | i >= b - k = acc
                   | otherwise  = let !q = piOf (isqrtI (lmtDiv (yp i))) in sum6 (i + 1) (acc - q * q)
      !s4 = a * sum4 (d - k) 0
      !s5 = sum5 (c - k) 0
      !s6 = sum6 (d - k) 0
      !sigma = s0 + s1 + s2 + s3 + s4 + s5 + s6
      !result = racc + dacc + phi0 + sigma               -- racc = A - B + C

  free tm
  free fctrs; free piys; free yprms; free ptrn
  free pcumP; free tbuf; free pfirst; free slots; free dcoefB; free incB; free carried
  return (fi result)