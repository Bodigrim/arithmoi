{-# LANGUAGE CPP, BangPatterns, MagicHash, UnboxedTuples, UnliftedFFITypes #-}

-- Gourdon prime counting: page sieve primitives (culling, pattern fill, block counts, D-phase operations).

{-# OPTIONS_GHC -O2 -fmax-simplifier-iterations=20 -fregs-graph #-}

module Math.NumberTheory.Primes.Sieve.Sieve
  ( cullPage, fillPage, countUnset
  , syncChnk, syncBlk, syncBlkB, pntCnt, sumPop, pntCntB
  , dInit, dCull, dSync, dQuery
  ) where

import Math.NumberTheory.Primes.Sieve.WheelIndexing
#if !defined(__GHCIDE__)
import Math.NumberTheory.Primes.Sieve.Kernels ( plainKernels, countedKernels )
#endif

import Data.Word ( Word8, Word16, Word32, Word64 )
import Data.Bits
  ( popCount, complement, (.&.), (.|.), unsafeShiftR, unsafeShiftL )
import Foreign.Ptr ( plusPtr )
import Foreign.Storable ( peekElemOff, pokeElemOff, peekByteOff, pokeByteOff )
import Foreign.Marshal.Utils ( copyBytes )
import GHC.Exts
import GHC.IO ( IO(..) )
#if !defined(__GHCIDE__)
import Data.Array.Base ( unsafeAt )
#endif

------------------------------------------------------------------------
-- Sieve page primitives
------------------------------------------------------------------------

-- Cull the multiples bp*m (m coprime to 30, first one at value `sval`, whose wheel position
-- is `swpi`) from the page that starts at byte index `lowwhli`, over the first `whlsz` bytes.
-- Eight residue planes, byte stride bp.  `mode` 0: plain cull (returns 0); 1: also returns the
-- number of bits that were clear and are now set (the effective culls); 2: as 1, and also
-- decrements the block count in `chn` (one Word16 per 2^blkShift words) for each effective cull.
cullPage :: Int -> Ptr Word8 -> Ptr Word16 -> Int -> Int -> Int -> Int -> Int -> IO Int
cullPage mode buf chn lowwhli bp whlsz sval swpi =
#if !defined(__GHCIDE__)
  if mode == 1 then planes (0 :: Int) cull0 wpi0 0 else kernelCull
#else
  planes (0 :: Int) cull0 wpi0 0
#endif
  where
#if !defined(__GHCIDE__)
    -- start byte and start cofactor position are those the planes code computes below
    !(I# kbp#)  = bp
    !(I# ksz#)  = whlsz
    !(I# kci0#) = toWhlNdx cull0 `unsafeShiftR` 3
    !kidx       = (toWhlNdx bp .&. 7) * 8 + (wpi0 .&. 7)
    !(Ptr kba#) = buf
    !(Ptr kcn#) = chn
    kernelCull
      | mode == 0 = IO (\s# -> case unsafeAt plainKernels kidx kba# ksz# kci0# kbp# s# of
                                 (# s1#, _ #) -> (# s1#, 0 #))
      | otherwise = IO (\s# -> case unsafeAt countedKernels kidx kba# kcn# ksz# kci0# kbp# s# of
                                 (# s1#, n# #) -> (# s1#, I# n# #))
#endif
    !bprsd  = whlRsd swpi
    !lowval = lowwhli * 30
    !cull0 | sval >= lowval = sval - lowval
           | otherwise = let !c = (lowval - sval) `rem` (bp * 30)
                         in if c /= 0
                              then let !wpndx = bprsd + (c + bp - 1) `quot` bp
                                   in bp * (whlRndup wpndx - bprsd) - c
                              else 0
    !wpi0  | sval >= lowval = swpi
           | otherwise = let !c = (lowval - sval) `rem` (bp * 30)
                         in if c /= 0
                              then whlRndupNdx (bprsd + (c + bp - 1) `quot` bp)
                              else swpi
    planes !i !cullval !wpi !tot
      | i >= 8 = return tot
      | otherwise = do
          let !culli = toWhlNdx cullval
              !m     = (1 :: Word8) `unsafeShiftL` (culli .&. 7)
              !ci0   = culli `unsafeShiftR` 3
          let !sh = culli .&. 7
          n <- case mode of
                 0 -> strideU ci0 m >> return 0
                 1 -> strideC ci0 m sh 0
                 _ -> strideK ci0 m sh 0
          planes (i + 1) (cullval + bp * whlGap wpi) (wpi + 1) (tot + n)
    strideU !ci !m
      | ci >= whlsz = return ()
      | otherwise = do
          v <- peekByteOff buf ci :: IO Word8
          pokeByteOff buf ci (v .|. m)
          strideU (ci + bp) m
    -- branchless: `eff` is 1 exactly when the bit was clear (the cull is effective)
    strideC !ci !m !sh !acc
      | ci >= whlsz = return acc
      | otherwise = do
          v <- peekByteOff buf ci :: IO Word8
          pokeByteOff buf ci (v .|. m)
          strideC (ci + bp) m sh (acc + fi (complement v `unsafeShiftR` sh .&. 1))
    !shB = 3 + blkShift
    strideK !ci !m !sh !acc
      | ci >= whlsz = return acc
      | otherwise = do
          v <- peekByteOff buf ci :: IO Word8
          pokeByteOff buf ci (v .|. m)
          let !eff = complement v `unsafeShiftR` sh .&. 1 :: Word8
              !bi  = ci `unsafeShiftR` shB
          c <- peekElemOff chn bi
          pokeElemOff chn bi (c - fi eff)
          strideK (ci + bp) m sh (acc + fi eff)

-- copy the repeating pre-cull pattern over a page
fillPage :: Ptr Word8 -> Ptr Word8 -> Int -> Int -> Int -> IO ()
fillPage buf ptrn prodpc pgsz lowwhli = go 0
  where
    go !bufi
      | bufi >= pgsz = return ()
      | otherwise = do
          let !ptrni = (lowwhli + bufi) `rem` prodpc
              !cpy   = min 16384 (pgsz - bufi)
          copyBytes (buf `plusPtr` bufi) (ptrn `plusPtr` ptrni) cpy
          go (bufi + 16384)

-- number of clear bits (survivors) in nwords 64-bit words
countUnset :: Ptr Word64 -> Int -> IO Int
countUnset b64 nwords = go 0 0
  where
    go !i !acc
      | i >= nwords = return acc
      | otherwise = do
          w <- peekElemOff b64 i
          go (i + 1) (acc + popCount (complement w))

-- blkcnts[i] = survivors in words 0..i-1, for the words needed to cover value uprlmt
-- (nothing when uprlmt is below the page)
syncBlk :: Int -> Ptr Word32 -> Ptr Word64 -> Int -> Int -> IO ()
syncBlk lowwhli blk b64 nwords uprlmt = do
  let !rel = toWhlNdx uprlmt - lowwhli * 8
      -- (rel + 64) >> 6 with an ARITHMETIC shift: <= 0 whenever rel < 0, so nothing is synced
      !n   = max 0 (min nwords ((rel + 64) `unsafeShiftR` 6))
  pokeElemOff blk 0 0
  let go !i !acc
        | i >= n = return ()
        | otherwise = do
            w <- peekElemOff b64 i
            let !acc' = acc + popCount (complement w)
            pokeElemOff blk (i + 1) (fi acc')
            go (i + 1) acc'
  go 0 (0 :: Int)
{-# INLINE syncBlk #-}

-- number of survivors (phi form) in the page at values <= pnt, relative to the page start
pntCnt :: Int -> Ptr Word32 -> Ptr Word64 -> Int -> IO Int
pntCnt lowwhli blk b64 pnt = do
  let !rel = toWhlNdx pnt - lowwhli * 8
  if rel < 0
    then return 0
    else do
      let !w = rel `unsafeShiftR` 6
          !r = rel .&. 63
      wv <- peekElemOff b64 w
      bc <- peekElemOff blk w
      let !msk = ((2 :: Word64) `unsafeShiftL` r) - 1   -- r = 63 gives all ones
      return (fi bc + popCount (complement wv .&. msk))
{-# INLINE pntCnt #-}

-- Delta-count block counts: chn[b] = survivors in block b (2^blkShift words); returns the page total
syncChnk :: Ptr Word64 -> Ptr Word16 -> Int -> IO Int
syncChnk b64 chn (I# nblk) = IO (\s0 -> case outer 0# 0# s0 of (# s1, r #) -> (# s1, I# r #))
  where
    !(I# bw#) = blkWords
    outer :: Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
    outer b tot s
      | isTrue# (b >=# nblk) = (# s, tot #)
      | otherwise =
          let !i0 = b *# bw#
          in case inner i0 (i0 +# bw#) 0# s of
               (# s1, c #) -> case unIO (pokeElemOff chn (I# b) (fromIntegral (I# c) :: Word16)) s1 of
                 (# s2, _ #) -> outer (b +# 1#) (tot +# c) s2
    inner :: Int# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
    inner i e acc s
      | isTrue# (i >=# e) = (# s, acc #)
      | otherwise = case unIO (peekElemOff b64 (I# i)) s of
          (# s1, w #) -> case popCount (complement w) of
            I# p -> inner (i +# 1#) e (acc +# p) s1

-- blk[b] = survivors in blocks 0..b-1, for the blocks needed to cover value uprlmt (a cumulative
-- sum of the block counts; nothing when uprlmt is below the page)
syncBlkB :: Int -> Ptr Word32 -> Ptr Word16 -> Int -> Int -> IO ()
syncBlkB lowwhli blk chn nblk uprlmt = do
  let !rel = toWhlNdx uprlmt - lowwhli * 8
      !n   = max 0 (min nblk ((rel + 64 * blkWords) `unsafeShiftR` (6 + blkShift)))
  pokeElemOff blk 0 0
  let go !b !acc
        | b >= n = return ()
        | otherwise = do
            c <- peekElemOff chn b
            let !acc' = acc + (fi c :: Int)
            pokeElemOff blk (b + 1) (fi acc')
            go (b + 1) acc'
  go 0 0
{-# INLINE syncBlkB #-}

-- number of clear bits in words i0 .. w-1 (unboxed loop state, see the note at unIO)
sumPop :: Ptr Word64 -> Int -> Int -> IO Int
sumPop b64 (I# i0) (I# w) = IO (\s0 -> case go i0 0# s0 of (# s1, r #) -> (# s1, I# r #))
  where
    go :: Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
    go i acc s
      | isTrue# (i >=# w) = (# s, acc #)
      | otherwise = case unIO (peekElemOff b64 (I# i)) s of
          (# s1, x #) -> case popCount (complement x) of
            I# p -> go (i +# 1#) (acc +# p) s1
{-# INLINE sumPop #-}

-- survivors at values <= pnt, using block prefix counts plus a popcount inside the block
pntCntB :: Int -> Ptr Word32 -> Ptr Word64 -> Int -> IO Int
pntCntB lowwhli blk b64 pnt = do
  let !rel = toWhlNdx pnt - lowwhli * 8
  if rel < 0
    then return 0
    else do
      let !w  = rel `unsafeShiftR` 6
          !r  = rel .&. 63
          !b0 = w .&. complement (blkWords - 1)
      bc <- peekElemOff blk (w `unsafeShiftR` blkShift)
      wv <- peekElemOff b64 w
      let !msk = ((2 :: Word64) `unsafeShiftL` r) - 1
      full <- sumPop b64 b0 w
      return (fi bc + full + popCount (complement wv .&. msk))
{-# INLINE pntCntB #-}

-- D-phase operations, selected at compile time: delta counts (default) or S1's direct counts
dInit :: Int -> Ptr Word32 -> Ptr Word16 -> Ptr Word64 -> Int -> Int -> IO Int
dQuery :: Int -> Ptr Word32 -> Ptr Word64 -> Int -> IO Int
dCull :: Int -> Int -> Ptr Word8 -> Ptr Word64 -> Ptr Word16 -> Int -> Int -> Int -> Int -> IO Int
dSync :: Int -> Ptr Word32 -> Ptr Word16 -> Ptr Word64 -> Int -> Int -> IO ()
#if defined(DIRECT_COUNT)
dInit lw blk _ b64 nwords nxtval = do
  t <- countUnset b64 nwords
  syncBlk lw blk b64 nwords nxtval
  return t
dQuery = pntCnt
dSync lw blk _ b64 nwords = syncBlk lw blk b64 nwords
-- cull base prime bp from the page and return the number of effective culls.
-- Arguments: running survivor count, page low index, page, page words, block counts, number of blocks,
-- bp, page bytes, wheel position of bp.
dCull _ lw page _ chn _ bp sz = cullPage 1 page chn lw bp sz (bp * bp)
#else
dInit lw blk chn b64 nwords nxtval = do
  let !nblk = nwords `unsafeShiftR` blkShift
  t <- syncChnk b64 chn nblk
  syncBlkB lw blk chn nblk nxtval
  return t
dQuery = pntCntB
dSync lw blk chn _ nwords = syncBlkB lw blk chn (nwords `unsafeShiftR` blkShift)
dCull _ lw page _ chn _ bp sz = cullPage 2 page chn lw bp sz (bp * bp)
#endif
{-# INLINE dCull #-}
{-# INLINE dInit #-}
{-# INLINE dQuery #-}
{-# INLINE dSync #-}
