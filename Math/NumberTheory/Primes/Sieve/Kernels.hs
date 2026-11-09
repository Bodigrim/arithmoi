{-# LANGUAGE CPP #-}
#if !defined(__GHCIDE__)
{-# LANGUAGE TemplateHaskell #-}
#endif
{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
{-# LANGUAGE MagicHash, UnboxedTuples, UnliftedFFITypes #-}
{- HLINT ignore "Use id" -}

-- Gourdon prime counting: Template Haskell generated, unrolled culling kernels (see the comment below).  With -DNO_TH
-- (or HLS, which defines __GHCIDE__) this module is empty and Gourdon.Sieve uses its scalar culler instead.

{-# OPTIONS_GHC -O2 -fmax-simplifier-iterations=20 -fregs-graph #-}

#if !defined(__GHCIDE__)
module Math.NumberTheory.Primes.Sieve.Kernels ( plainKernels, countedKernels ) where
#else
module Math.NumberTheory.Primes.Sieve.Kernels () where
#endif

#if !defined(__GHCIDE__)
import Math.NumberTheory.Primes.Sieve.WheelIndexing ( blkShift )

import Data.Bits
import Data.Array ( Array )
import Data.Array.Base ( listArray )
-- Template Haskell (generators only; the generated code needs none of these at run time)
import Language.Haskell.TH
  ( ExpQ, DecQ, Exp(VarE), litE, intPrimL, wordPrimL, listE
  , mkName, sigD, valD, varP, normalB )
#if __GLASGOW_HASKELL__ >= 902
import GHC.Exts
  ( Addr#, Int#, Word#, State#, (+#), (-#), (*#), quotInt#
  , word2Int#, not#, and#, or#, uncheckedIShiftRL#, uncheckedShiftRL#
  , readWord8OffAddr#, writeWord8OffAddr#, readWord16OffAddr#, writeWord16OffAddr# )
import GHC.Prim
  ( Word8#, Word16#, wordToWord8#, word8ToWord#, wordToWord16#, word16ToWord#
  , plusAddr#, minusAddr#, ltAddr#, minusWord# )
#else
import GHC.Exts
  ( Addr#, Int#, Word#, State#
  , (+#), (-#), (*#), quotInt#, not#, and#, or#
  , word2Int#, uncheckedShiftRL#, uncheckedIShiftRL#
  , readWord8OffAddr#, writeWord8OffAddr#, readWord16OffAddr#
  , plusAddr#, minusAddr#, ltAddr#, minusWord#
  , readWord8OffAddr#, writeWord8OffAddr#, readWord16OffAddr#, writeWord16OffAddr# )
-- Prior to GHC 9.2, Word8# and Word16# are represented as a standard Word#.
-- Casting them to Word# is simply the identity function.
type Word8# = Word#
type Word16# = Word#
wordToWord8# :: Word# -> Word#
wordToWord8# w# = w#
word8ToWord# :: Word# -> Word#
word8ToWord# w# = w#
wordToWord16# :: Word# -> Word#
wordToWord16# w# = w#
word16ToWord# :: Word# -> Word#
word16ToWord# w# = w#
#endif
#endif

------------------------------------------------------------------------
-- Template Haskell: unrolled culling kernels (plainKernels, countedKernels)
------------------------------------------------------------------------
-- One kernel makes ONE pass over the page buffer with byte stride p and culls all 8 wheel
-- slots per stride (30*p values = p bytes), instead of 8 separate strided passes.
-- Kernel k = pIdx*8 + g, where
--   pIdx = wheel position (0..7) of the base prime p (p mod 30 = whlRsd pIdx),
--   g    = wheel position (0..7) of the cofactor q of the first multiple p*q to cull.
-- With v0 = p*q the start value, r0 = v0 mod 30 is a static function of (pIdx, g).  Slot i
-- (i = 0..7) culls v0 + p*cum_i, cum_i = sum of the first i full gaps starting at position g.
-- Relative to the start byte its byte offset is  r_i = (r0 + p*cum_i) div 30  (dynamic, p is
-- a run-time value) and its bit is  pos((r0 + res(pIdx)*cum_i) mod 30)  (static, p mod 30
-- is res(pIdx)).  The main loop runs while the LAST slot of the stride is inside the buffer;
-- the tail guards each slot (the r_i never decrease).
-- Plain kernel  : \ba# sz# ci0# bp# s# -> (# s#, () #)
--   ba# buffer, sz# bytes to cull over, ci0# start byte, bp# = p.
-- Counted kernel: \ba# _cn# sz# ci0# bp# s# -> (# s#, n# #)
--   as plain, and for every bit that was clear it decrements the Word16 block count
--   _cn#[(byte offset) >> (3 + blkShift)] and adds one to n# (branchless).
-- The generated code names only GHC.Exts primops and listArray (imported above).
--
-- Naming note: the per-slot byte offsets _r0# .. _r7# and the count array _cn# are bound in
-- one Template Haskell quotation but referenced from another one by name (via mkName / dynamic
-- variables, see rv below), so GHC cannot see those uses and would emit -Wunused-matches /
-- -Wunused-local-binds warnings for them.  The leading underscore suppresses those warnings;
-- the binders and the references must keep exactly the same spelling.
#if !defined(__GHCIDE__)
$( let
     sRes, sGap :: [Int]
     sRes = [1,7,11,13,17,19,23,29]
     sGap = [6,4,2,4,2,4,6,2]

     posOf :: Int -> Int
     posOf r = length (takeWhile (/= r) sRes)

     r0Of :: Int -> Int -> Int
     r0Of pIdx g = ((sRes !! pIdx) * (sRes !! g)) `mod` 30

     cumOf :: Int -> Int -> Int
     cumOf g i = sum [ sGap !! ((g + j) `mod` 8) | j <- [0 .. i - 1] ]

     bitOf :: Int -> Int -> Int -> Int
     bitOf pIdx g i = posOf ((r0Of pIdx g + (sRes !! pIdx) * cumOf g i) `mod` 30)

     shBT :: Int
     shBT = 3 + blkShift

     lit :: Int -> ExpQ
     lit = litE . intPrimL . toInteger

     -- reference to the slot-i byte offset binder _r<i># of the enclosing kernel
     rv :: Int -> ExpQ
     rv i = return (VarE (mkName ("_r" ++ show i ++ "#")))

     -- plain cull of slot i (ai# and _r<i># are captured by name from the kernel)
     cullP :: Int -> Int -> Int -> ExpQ
     cullP pIdx g i =
       let rq = rv i
           mq = litE (wordPrimL ((1 :: Integer) `shiftL` bitOf pIdx g i))
       in [| \s0# -> case readWord8OffAddr# (ai# :: Addr#) $rq s0# of
                       (# s1#, v# :: Word8# #) ->
                         let !wVal# = word8ToWord# v# :: Word#
                             !nv#   = wordToWord8# (wVal# `or#` ($mq :: Word#)) :: Word8#
                         in case writeWord8OffAddr# (ai# :: Addr#) $rq nv# s1# of
                              s2# -> s2# |]

     -- counted cull of slot i: threads the running count of effective culls
     cullC :: Int -> Int -> Int -> ExpQ
     cullC pIdx g i =
       let rq = rv i
           b  = bitOf pIdx g i
           mq = litE (wordPrimL ((1 :: Integer) `shiftL` b))
           bq = lit b
           sq = lit shBT
       in [| \acc# s0# -> case readWord8OffAddr# (ai# :: Addr#) $rq s0# of
                (# s1#, v# :: Word8# #) ->
                  let !wVal# = word8ToWord# v# :: Word#
                      !nv#   = wordToWord8# (wVal# `or#` ($mq :: Word#)) :: Word8#
                      !eff#  = (not# wVal# `uncheckedShiftRL#` ($bq :: Int#)) `and#` 1## :: Word#
                      !bi#   = (((ai# :: Addr#) `minusAddr#` (ba# :: Addr#)) +# $rq)
                                 `uncheckedIShiftRL#` ($sq :: Int#) :: Int#
                  in case writeWord8OffAddr# (ai# :: Addr#) $rq nv# s1# of
                       s2# -> case readWord16OffAddr# (_cn# :: Addr#) bi# s2# of
                         (# s3#, c# :: Word16# #) ->
                           case writeWord16OffAddr# (_cn# :: Addr#) bi#
                                  (wordToWord16# (word16ToWord# c# `minusWord#` eff#)) s3# of
                             s4# -> (# s4#, acc# +# word2Int# eff# #) |]

     -- all 8 slots of one stride (every slot is inside the buffer)
     chainP :: Int -> Int -> ExpQ
     chainP pIdx g = foldr (\i k -> [| \s# -> case $(cullP pIdx g i) s# of so# -> $k so# |])
                           [| \s# -> s# |] [0 .. 7]

     chainC :: Int -> Int -> ExpQ
     chainC pIdx g = foldr (\i k -> [| \acc# s# -> case $(cullC pIdx g i) acc# s# of
                                                     (# so#, an# #) -> $k an# so# |])
                           [| \acc# s# -> (# s#, acc# #) |] [0 .. 7]

     -- tail stride: each slot guarded by its address being below lmt#
     closeP :: Int -> Int -> ExpQ
     closeP pIdx g = go 0
       where
         go :: Int -> ExpQ
         go i | i > 7 = [| \_ s# -> s# |]
              | otherwise =
                  [| \ai# s# -> case (ai# `plusAddr#` $(rv i)) `ltAddr#` lmt# of
                                  0# -> s#
                                  _  -> case $(cullP pIdx g i) s# of so# -> $(go (i + 1)) ai# so# |]

     closeC :: Int -> Int -> ExpQ
     closeC pIdx g = go 0
       where
         go :: Int -> ExpQ
         go i | i > 7 = [| \_ acc# s# -> (# s#, acc# #) |]
              | otherwise =
                  [| \ai# acc# s# -> case (ai# `plusAddr#` $(rv i)) `ltAddr#` lmt# of
                                       0# -> (# s#, acc# #)
                                       _  -> case $(cullC pIdx g i) acc# s# of
                                               (# so#, an# #) -> $(go (i + 1)) ai# an# so# |]

     mkPlain :: Int -> Int -> ExpQ
     mkPlain pIdx g =
       let r0 = r0Of pIdx g
           c1 = cumOf g 1; c2 = cumOf g 2; c3 = cumOf g 3; c4 = cumOf g 4
           c5 = cumOf g 5; c6 = cumOf g 6; c7 = cumOf g 7
       in [| \ba# sz# ci0# bp# s# ->
              let _r0# = 0#
                  _r1# = ($(lit r0) +# (bp# *# $(lit c1))) `quotInt#` 30#
                  _r2# = ($(lit r0) +# (bp# *# $(lit c2))) `quotInt#` 30#
                  _r3# = ($(lit r0) +# (bp# *# $(lit c3))) `quotInt#` 30#
                  _r4# = ($(lit r0) +# (bp# *# $(lit c4))) `quotInt#` 30#
                  _r5# = ($(lit r0) +# (bp# *# $(lit c5))) `quotInt#` 30#
                  _r6# = ($(lit r0) +# (bp# *# $(lit c6))) `quotInt#` 30#
                  _r7# = ($(lit r0) +# (bp# *# $(lit c7))) `quotInt#` 30#
                  a0#   = ba# `plusAddr#` ci0#
                  lmt#  = ba# `plusAddr#` sz#
                  almt# = lmt# `plusAddr#` (0# -# _r7#)
                  loopa# ai# si# =
                    case ai# `ltAddr#` almt# of
                      0# -> $(closeP pIdx g) ai# si#
                      _  -> case $(chainP pIdx g) si# of
                              so# -> loopa# (ai# `plusAddr#` bp#) so#
              in case loopa# a0# s# of so# -> (# so#, () #) |]

     mkCounted :: Int -> Int -> ExpQ
     mkCounted pIdx g =
       let r0 = r0Of pIdx g
           c1 = cumOf g 1; c2 = cumOf g 2; c3 = cumOf g 3; c4 = cumOf g 4
           c5 = cumOf g 5; c6 = cumOf g 6; c7 = cumOf g 7
       in [| \ba# _cn# sz# ci0# bp# s# ->
              let _r0# = 0#
                  _r1# = ($(lit r0) +# (bp# *# $(lit c1))) `quotInt#` 30#
                  _r2# = ($(lit r0) +# (bp# *# $(lit c2))) `quotInt#` 30#
                  _r3# = ($(lit r0) +# (bp# *# $(lit c3))) `quotInt#` 30#
                  _r4# = ($(lit r0) +# (bp# *# $(lit c4))) `quotInt#` 30#
                  _r5# = ($(lit r0) +# (bp# *# $(lit c5))) `quotInt#` 30#
                  _r6# = ($(lit r0) +# (bp# *# $(lit c6))) `quotInt#` 30#
                  _r7# = ($(lit r0) +# (bp# *# $(lit c7))) `quotInt#` 30#
                  a0#   = ba# `plusAddr#` ci0#
                  lmt#  = ba# `plusAddr#` sz#
                  almt# = lmt# `plusAddr#` (0# -# _r7#)
                  loopa# ai# acc# si# =
                    case ai# `ltAddr#` almt# of
                      0# -> $(closeC pIdx g) ai# acc# si#
                      _  -> case $(chainC pIdx g) acc# si# of
                              (# so#, an# #) -> loopa# (ai# `plusAddr#` bp#) an# so#
              in loopa# a0# 0# s# |]


     kernelArr :: (Int -> Int -> ExpQ) -> ExpQ
     kernelArr mk = do
       es <- listE [ mk (i `shiftR` 3) (i .&. 7) | i <- [0 .. 63] ]
       [| listArray (0 :: Int, 63) $(return es) |]

     topBind :: String -> ExpQ -> DecQ
     topBind nm e = valD (varP (mkName nm)) (normalB e) []
   in sequence
        [ sigD (mkName "plainKernels")
            [t| forall s. Array Int (Addr# -> Int# -> Int# -> Int# -> State# s -> (# State# s, () #)) |]
        , topBind "plainKernels" (kernelArr mkPlain)
        , sigD (mkName "countedKernels")
            [t| forall s. Array Int (Addr# -> Addr# -> Int# -> Int# -> Int# -> State# s -> (# State# s, Int# #)) |]
        , topBind "countedKernels" (kernelArr mkCounted)
        ]
 )
#endif
