{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -O2 -fmax-simplifier-iterations=20 -fregs-graph -fspec-constr-count=8 #-}

-- |
-- Module:      Math.NumberTheory.Primes.Sieve.Eratosthenes
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
--
-- Sieve of Eratosthenes: page sieve with the 30-wheel byte layout of
-- "Math.NumberTheory.Primes.Sieve.WheelIndexing".
--
-- Bit convention (the one of the former Sieve module, used throughout):
--
--   * bit i of a 'PrimeSieve' with offset v0 stands for the value
--     @v0 + 'ndxToPrm' i@ (per 30 values the residues 1,7,11,13,17,19,23,29);
--   * a SET bit means COMPOSITE (or not coprime to 30), a CLEAR bit means survivor/prime;
--   * v0 is a multiple of 30, and the bit for the value 1 is always marked composite.
--
-- 'sieveTo' produces the same convention, indices 0 .. @'toWhlNdx' bound@.
--
-- Range: values up to 2^64 - 1.  All value arithmetic is done in 'Word64' (or 'Integer'
-- once per page), page-relative quantities in 'Int'.  'psieveList'/'psieveFrom' end at
-- 2^64 (the last chunk may contain unreliable bits above 2^64 - 1); the counting and
-- selecting functions respect the limit exactly.  Assumes a 64-bit little-endian platform.
--
-- Counting and selecting ('countPrimesBetween', 'nthPrimeAfter', 'nthPrimeAtOrBefore')
-- work on SEGMENTS of 'cfgSegPages' pages (a page is 512 KB, 15 728 640 values), one
-- spark per segment, groups of 'cfgGroupSegs' segments at a time; they run in parallel
-- with the threaded RTS and @+RTS -N@, sequentially otherwise.  Within a segment the
-- base primes below 'cfgLargeFrom' are culled page by page (cache resident), the larger
-- ones once over the whole segment, which saves their per-page start computation when
-- most of them have no multiple in a given page.  With the default configuration
-- (one page per segment) this is the plain page sieve.
module Math.NumberTheory.Primes.Sieve.Eratosthenes
    ( primes
    , psieveFrom
    , psieveDownFrom
    , PrimeSieve(..)
    , psieveList
    , primeList
    , primeListDown
    , primeSieve
    , sieveBits
    , sieveRange
    , sieveTo
      -- * Counting and selecting by segment-parallel sieving
    , SieveCfg(..)
    , defaultCfg
    , Base
    , baseFor
    , baseSize
    , countPrimesBetween
    , countPrimesBetweenCfg
    , nthPrimeAfter
    , nthPrimeAfterCfg
    , nthPrimeAtOrBefore
    , nthPrimeAtOrBeforeCfg
    ) where

import Control.Monad (void, when)
import Control.Monad.ST
import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)
import Data.Bit
import Data.Bits
import Data.Coerce
import Data.Int (Int64)
import Data.Proxy
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word
import Foreign.Marshal.Alloc (allocaBytes, mallocBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peek, poke, peekElemOff, pokeByteOff)
import GHC.Conc (par)
import System.IO.Unsafe (unsafePerformIO)

import Math.NumberTheory.Primes.Sieve.Sieve (cullPage, fillPage)
import Math.NumberTheory.Primes.Sieve.WheelIndexing
import Math.NumberTheory.Primes.Types

------------------------------------------------------------------------
-- Chunk geometry and limits
------------------------------------------------------------------------

-- Bytes per chunk (one page of the page sieve).
sieveBytes :: Int
sieveBytes = pageBytes

-- Number of bits per chunk.
sieveBits :: Int
sieveBits = 8 * sieveBytes

-- Range of a chunk.
sieveRange :: Int
sieveRange = 30 * sieveBytes

-- 64-bit words per chunk.
chunkWords :: Int
chunkWords = sieveBits `quot` 64

-- Largest value handled.
maxValue :: Integer
maxValue = 2 ^ (64 :: Int) - 1

-- Largest byte index k with 30*k <= 2^64 - 1.
maxByteIndex :: Int
maxByteIndex = 614891469123651720

-- Largest base prime needed is below 2^32.
maxBasePrimeLimit :: Word64
maxBasePrimeLimit = 4294967296

-- Value, saturated at 2^64 - 1.
satTop :: Integer -> Word64
satTop = fromInteger . min maxValue

-- Number of wheel indices whose value is <= v (v < 0 gives 0): the first index with a
-- value > v, and the exclusive upper bound of the indices with value <= v.
wheelCount :: Integer -> Int
wheelCount v
  | v < 0     = 0
  | otherwise = fromInteger (8 * q + toInteger (whlNdxP1 (fromInteger r)))
  where (q, r) = v `quotRem` 30

-- Value of a global wheel index.
valueOfIndex :: Int -> Integer
valueOfIndex i = 30 * toInteger (i `unsafeShiftR` 3) + toInteger (whlRsd (i .&. 7))

-- | Compact store of primality flags: offset (multiple of 30) and bits,
--   a set bit is a composite, see the module header.
data PrimeSieve = PS !Integer {-# UNPACK #-} !(U.Vector Bit)

-- | Sieve primes up to (and including) a bound (or 7, if bound is smaller).
--   For small enough bounds, this is more efficient than
--   using the segmented sieve.
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

-- Survivors of a sieve in ascending order, scanned a 64-bit word at a time:
-- the complemented word has a one for each survivor, countTrailingZeros finds the
-- lowest (bit i of a word is the i-th least significant bit), w .&. (w-1) clears it.
-- Zero words cost one comparison.
primeListInternal :: forall a. Num a => PrimeSieve -> [a]
primeListInternal (PS v0 bs) = goWord 0
  where
    !len = U.length bs
    !ws  = cloneToWords bs :: U.Vector Word   -- a copy, negligible next to the per-prime work
    !nw  = U.length ws
    off :: a
    off = fromInteger v0
    goWord :: Int -> [a]
    goWord !i
      | i >= nw   = []
      | otherwise = scan i (survivors i)
    survivors :: Int -> Word64
    survivors i =
      let !w     = complement (fromIntegral (U.unsafeIndex ws i)) :: Word64
          !valid = len - 64 * i                      -- >= 1 as i < nw
      in if valid >= 64 then w else w .&. ((1 `unsafeShiftL` valid) - 1)
    scan :: Int -> Word64 -> [a]
    scan !i !w
      | w == 0    = goWord (i + 1)
      | otherwise = (off + fromIntegral (ndxToPrm (64 * i + countTrailingZeros w)))
                      : scan i (w .&. (w - 1))

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

------------------------------------------------------------------------
-- Segmented sieve: the list of chunks
------------------------------------------------------------------------

-- Chunks sieved ahead of the consumer by sparks.  Only effective with the threaded RTS
-- and more than one capability (otherwise sparks are never run); 0 disables it.
lookaheadChunks :: Int
lookaheadChunks = 2

withLookahead :: Int -> [PrimeSieve] -> [PrimeSieve]
withLookahead n xs0 = go (drop n xs0) xs0
  where
    go (ahead : aheads) (x : xs) = ahead `par` (x : go aheads xs)
    go _                xs       = xs

-- | List of primes in the form of a list of 'PrimeSieve's, more compact than
--   'primes', thus it may be better to use @'psieveList' >>= 'primeList'@
--   than keeping the list of primes alive during the entire run.
psieveList :: [PrimeSieve]
psieveList = withLookahead lookaheadChunks (makeSieves 0 0 U.empty)

-- | @'psieveFrom' n@ creates the list of 'PrimeSieve's starting roughly
--   at @n@. Due to the organisation of the sieve, the list may contain
--   a few primes less than @n@.
--   This form uses less memory than @['Integer']@, hence it may be preferable
--   to use this if it is to be reused.
psieveFrom :: Integer -> [PrimeSieve]
psieveFrom n
  | n > maxValue = []
  | otherwise    = withLookahead lookaheadChunks (makeSieves k0 0 U.empty)
  where
    k0 = fromInteger (max 0 n `quot` 30) :: Int   -- first byte index of the first chunk

-- | @'psieveDownFrom' n@ creates the list of 'PrimeSieve's going downwards from the one that
--   contains @n@ to the one that starts at 0.  Chunks are aligned to multiples of 'sieveRange'
--   (the first one is a whole chunk, so it may hold primes above @n@: use 'primeListDown'
--   with the bound @n@ to read it), each chunk covers the values @v0 .. v0 + sieveRange - 1@.
--   Empty when @n < 2@ or @n@ exceeds @2^64 - 1@, the limit of the sieve.
--   All base primes are built once, for the first chunk, and shared by the others.
psieveDownFrom :: Integer -> [PrimeSieve]
psieveDownFrom n
  | n < 2 || n > maxValue = []
  | otherwise             = withLookahead lookaheadChunks (go kTop)
  where
    kTop = (fromInteger (n `quot` 30) `quot` sieveBytes) * sieveBytes :: Int
    plim = min maxBasePrimeLimit
               (isqrtW (satTop (30 * (toInteger kTop + toInteger sieveBytes))) + 1)
    bps  = basePrimes plim
    go :: Int -> [PrimeSieve]
    go !k
      | k < 0     = []
      | otherwise = PS (30 * toInteger k) (chunkVec bps k) : go (k - sieveBytes)

-- | The primes of a 'PrimeSieve' that are at most @bound@, in descending order: the
--   counterpart of 'primeList' for use with 'psieveDownFrom'.  The bound must fit into the
--   result type @a@.  (Primes above the bound are never produced, which also takes care of
--   the unreliable bits above @2^64 - 1@ in the last chunk.)
primeListDown :: forall a. Integral a => Integer -> PrimeSieve -> [Prime a]
primeListDown bound (PS v0 bs)
  | len <= 0  = coerce (small :: [a])
  | otherwise = coerce (goWord ((len - 1) `quot` 64))
  where
    -- number of bits that can hold a value <= bound
    !len = min (U.length bs) (wheelCount (bound - v0))
    !ws  = cloneToWords bs :: U.Vector Word
    off :: a
    off = fromInteger v0
    small :: [a]
    small | v0 == 0   = [ fromIntegral p | p <- [5, 3, 2 :: Int], toInteger p <= bound ]
          | otherwise = []
    goWord :: Int -> [a]
    goWord !i
      | i < 0     = small
      | otherwise = scan i (survivors i)
    survivors :: Int -> Word64
    survivors i =
      let !w     = complement (fromIntegral (U.unsafeIndex ws i)) :: Word64
          !valid = len - 64 * i                      -- >= 1 as i <= (len - 1) / 64
      in if valid >= 64 then w else w .&. ((1 `unsafeShiftL` valid) - 1)
    scan :: Int -> Word64 -> [a]
    scan !i !w
      | w == 0    = goWord (i - 1)
      | otherwise = let !b = 63 - countLeadingZeros w
                    in (off + fromIntegral (ndxToPrm (64 * i + b))) : scan i (clearBit w b)

-- The chunk with first byte index k has offset 30*k and covers the values
-- 30*k + 1 .. 30*(k + sieveBytes) - 1.  The base primes (those >= 19) live in a vector
-- that is rebuilt, with at least doubled limit, whenever the next chunk needs larger ones.
makeSieves :: Int -> Word64 -> U.Vector Word32 -> [PrimeSieve]
makeSieves !k !plim bps
  | k > maxByteIndex = []
  | need <= plim     = PS (30 * toInteger k) (chunkVec bps k) : makeSieves (k + sieveBytes) plim bps
  | otherwise        = makeSieves k plim' (basePrimes plim')
  where
    need  = isqrtW (satTop (30 * (toInteger k + toInteger sieveBytes))) + 1
    plim' = min maxBasePrimeLimit (max need (2 * plim))

-- One chunk.
chunkVec :: U.Vector Word32 -> Int -> U.Vector Bit
chunkVec bps k = unsafePerformIO $ do
  mv <- MU.unsafeNew chunkWords :: IO (MU.IOVector Word)
  withPage pageBytes bps k chunkWords $ \p ->
    let go !i
          | i >= chunkWords = return ()
          | otherwise = do
              w <- peekElemOff p i
              MU.unsafeWrite mv i (fromIntegral w)
              go (i + 1)
    in go 0
  fmap castFromWords (U.unsafeFreeze mv)
{-# NOINLINE chunkVec #-}

-- Primes p with 19 <= p <= lim (7..17 are handled by the pre-sieve pattern), lim <= 2^32.
basePrimes :: Word64 -> U.Vector Word32
basePrimes lim
  | lim < 19  = U.empty
  | otherwise = U.drop 4 (survivorValues (runST (sieveTo (toInteger lim) >>= U.unsafeFreeze)))
                -- the first four survivors are 7, 11, 13, 17

-- The values of the survivors (clear bits) of a sieve starting at 0, as a vector, found a word
-- at a time with countTrailingZeros (no intermediate list).
survivorValues :: U.Vector Bit -> U.Vector Word32
survivorValues bs = runST $ do
    mv <- MU.unsafeNew n
    let goW !i !j
          | i >= nw   = return ()
          | otherwise = goB i (survivorsWord i) j
        goB !i !w !j
          | w == 0    = goW (i + 1) j
          | otherwise = do
              MU.unsafeWrite mv j (fromIntegral (ndxToPrm (64 * i + countTrailingZeros w)))
              goB i (w .&. (w - 1)) (j + 1)
    goW 0 0
    U.unsafeFreeze mv
  where
    !len = U.length bs
    !n   = len - countBits bs
    !ws  = cloneToWords bs :: U.Vector Word
    !nw  = U.length ws
    survivorsWord :: Int -> Word64
    survivorsWord i =
      let !w     = complement (fromIntegral (U.unsafeIndex ws i)) :: Word64
          !valid = len - 64 * i
      in if valid >= 64 then w else w .&. ((1 `unsafeShiftL` valid) - 1)

-- | Sieve up to bound in one go (in pages internally); indices 0 .. @'toWhlNdx' bound@.
--   The bound must fit into an 'Int'.
sieveTo :: forall s. Integer -> ST s (MU.MVector s Bit)
sieveTo bound = do
    mv <- MU.unsafeNew nW :: ST s (MU.MVector s Word)
    let copyOut :: Ptr Word64 -> Int -> Int -> IO ()
        copyOut p n off = loop 0
          where
            loop !i
              | i >= n = return ()
              | otherwise = do
                  w <- peekElemOff p i
                  unsafeSTToIO (MU.unsafeWrite mv (off + i) (fromIntegral w))
                  loop (i + 1)
        go :: Int -> IO ()
        go !j
          | off >= nW = return ()
          | otherwise = do
              withPage pageBytes bps (j * sieveBytes) n (\p -> copyOut p n off)
              go (j + 1)
          where
            off = j * chunkWords
            n   = min chunkWords (nW - off)
    unsafeIOToST (go 0)
    return (MU.slice 0 nbits (castFromWordsM mv))
  where
    bI    = fromInteger bound `max` 7 :: Int64
    nbits = toWhlNdx (fi bI) + 1
    nW    = (nbits + 63) `quot` 64
    bps   = basePrimes (isqrtW (fi bI))

------------------------------------------------------------------------
-- Page sieve (the culling primitives cullPage and fillPage come from the Sieve module)
------------------------------------------------------------------------

-- Small primes folded into the repeating pre-cull pattern.
preSieved :: [Int64]
preSieved = [7, 11, 13, 17]

-- Pattern period in bytes (one byte covers 30 values).
prodpc :: Int
prodpc = 7 * 11 * 13 * 17

-- The pattern is stored with a tail so that fillPage can copy 16384 bytes from any offset.
ptrnLen :: Int
ptrnLen = prodpc + 16384

-- Multiples of 7, 11, 13, 17 (including the primes themselves, repaired on page 0).
ptrn :: Ptr Word8
ptrn = unsafePerformIO $ do
  p <- mallocBytes ptrnLen
  let byteAt :: Int -> Word8
      byteAt b = foldl (\acc j -> if any (\q -> (30 * fromIntegral b + fromIntegral (whlRsd j)) `rem` q == 0) preSieved
                                    then acc .|. (1 `unsafeShiftL` j)
                                    else acc) 0 [0 .. 7]
      go !b | b >= ptrnLen = return ()
            | otherwise    = pokeByteOff p b (byteAt b) >> go (b + 1)
  go 0
  return p
{-# NOINLINE ptrn #-}

-- The first multiple bp*m (m coprime to 30, m >= bp) at or after the start of the page with
-- value `lowval` (= 30 * first byte index), as (value offset from the page start, wheel
-- position of the cofactor m).  Done in Word64 so that it is exact up to 2^64; everything
-- handed to cullPage is page-relative and small.  An offset >= 30 * page bytes means "none".
firstMultiple :: Word64 -> Word64 -> (Int, Int)
firstMultiple lowval bp
  | sval >= lowval = (if d >= 0x4000000000000000 then maxBound else fi d, swpi)
  | c == 0         = (0, swpi)
  | otherwise      = (bpI * (whlRndup wpndx - bprsd) - cI, whlRndupNdx wpndx)
  where
    bpI   = fi bp :: Int
    sval  = bp * bp
    d     = sval - lowval
    swpi  = whlNdxP1 (fi (bp `rem` 30)) - 1
    bprsd = whlRsd swpi
    c     = (lowval - sval) `rem` (bp * 30)
    cI    = fi c :: Int
    wpndx = bprsd + (cI + bpI - 1) `quot` bpI
{-# INLINE firstMultiple #-}

-- Sieve the `nW` words (bytes [k, k + 8*nW) of the wheel) into a scratch buffer and run
-- `act` on it (set bit = composite).  `bps` must hold every prime >= 19 whose square is
-- below the top of the buffer; k must not exceed maxByteIndex.
--
-- A buffer of at most one page is culled by all base primes in one pass.  A larger one (a
-- segment) is culled in two tiers: the base primes below `largeFrom` page by page (each
-- page stays cache resident), the remaining ones once over the whole segment.  Both give
-- the same bits; the large tier saves the per-page start computation of primes that have
-- at most about one multiple per page.
withPage :: Int -> U.Vector Word32 -> Int -> Int -> (Ptr Word64 -> IO r) -> IO r
withPage largeFrom bps k nW act =
  allocaBytes pgsz $ \buf -> do
    fillPage buf ptrn prodpc pgsz k
    let !nb = U.length bps
        bpAt :: Int -> Word64
        bpAt i = fromIntegral (U.unsafeIndex bps i)
        -- first index of a base prime >= largeFrom (the vector is ascending)
        firstLarge :: Int -> Int -> Int
        firstLarge !lo !hi
          | lo >= hi  = lo
          | otherwise = let !mid = (lo + hi) `unsafeShiftR` 1
                        in if (fromIntegral (bpAt mid) :: Int) >= largeFrom
                             then firstLarge lo mid
                             else firstLarge (mid + 1) hi
        !lgi | pgsz <= pageBytes = nb
             | otherwise         = firstLarge 0 nb
        -- small tier: base primes 0 .. lgi-1, one page (sub-page of the buffer) at a time
        subPage :: Int -> IO ()
        subPage !o
          | o >= pgsz = return ()
          | otherwise = do
              let !sz = min pageBytes (pgsz - o)
                  !kb = k + o
              when (kb <= maxByteIndex) $ do
                let !lowv = 30 * fromIntegral kb :: Word64
                    !top  = satTop (30 * (toInteger kb + toInteger sz))
                    !pv   = 30 * sz
                    !sub  = buf `plusPtr` o :: Ptr Word8
                    cull !i
                      | i >= lgi = return ()
                      | otherwise =
                          let !bp = bpAt i
                          in if bp * bp >= top
                               then return ()
                               else do
                                 let (!c0, !w0) = firstMultiple lowv bp
                                 when (c0 < pv) $
                                   void (cullPage 0 sub nullPtr 0 (fi bp) sz c0 w0)
                                 cull (i + 1)
                cull 0
              subPage (o + pageBytes)
        -- large tier: base primes lgi .. nb-1, once over the whole buffer
        !segTop = satTop (30 * (toInteger k + toInteger pgsz))
        !segLow = 30 * fromIntegral k :: Word64
        !segPv  = 30 * pgsz
        large :: Int -> IO ()
        large !i
          | i >= nb = return ()
          | otherwise =
              let !bp = bpAt i
              in if bp * bp >= segTop
                   then return ()
                   else do
                     let (!c0, !w0) = firstMultiple segLow bp
                     when (c0 < segPv) $
                       void (cullPage 0 buf nullPtr 0 (fi bp) pgsz c0 w0)
                     large (i + 1)
    subPage 0
    when (lgi < nb) (large lgi)
    -- byte 0 of page 0: 7, 11, 13, 17 (bits 1..4) were culled by the pattern but are prime;
    -- the value 1 (bit 0) is marked composite
    when (k == 0) $ do
      v <- peek buf :: IO Word8
      poke buf ((v .&. 0xE1) .|. 0x01)
    act (castPtr buf)
  where
    !pgsz = 8 * nW
{-# INLINE withPage #-}

------------------------------------------------------------------------
-- Counting and selecting
------------------------------------------------------------------------

-- | Parameters of the segment-parallel functions.
data SieveCfg = SieveCfg
  { cfgSegPages  :: !Int  -- ^ pages (512 KB each) per segment, one spark per segment
  , cfgGroupSegs :: !Int  -- ^ segments per group: sieved in parallel, then examined in order;
                          --   a search overshoots its target by at most one group
  , cfgLargeFrom :: !Int  -- ^ base primes from this size up are culled once per segment
                          --   instead of once per page (irrelevant for one-page segments)
  } deriving (Show)

-- | One page per segment, 32 segments per group: the plain page sieve.
defaultCfg :: SieveCfg
defaultCfg = SieveCfg 1 32 pageBytes

segBytesOf, segBitsOf, segWordsOf :: SieveCfg -> Int
segBytesOf c = cfgSegPages c * sieveBytes
segBitsOf  c = 8 * segBytesOf c
segWordsOf c = segBytesOf c `quot` 8

-- Value at the (exclusive) end of segment q.
segEndValue :: SieveCfg -> Int -> Integer
segEndValue c q = 30 * (toInteger (q + 1) * toInteger (segBytesOf c))

-- Survivors (clear bits) of word w at bit offset `base`, restricted to bit indices [lo, hi).
survivorsMasked :: Int -> Int -> Int -> Word64 -> Word64
survivorsMasked lo hi base w = complement w .&. m1 .&. m2
  where
    m1 | lo > base       = complement 0 `unsafeShiftL` (lo - base)
       | otherwise       = complement 0
    m2 | hi < base + 64  = (1 `unsafeShiftL` (hi - base)) - 1
       | otherwise       = complement 0
{-# INLINE survivorsMasked #-}

-- Number of survivors with bit index in [lo, hi) of a sieved buffer.
countWords :: Ptr Word64 -> Int -> Int -> IO Int
countWords p lo hi = go (lo `unsafeShiftR` 6) 0
  where
    !iEnd = (hi + 63) `unsafeShiftR` 6
    go !i !acc
      | i >= iEnd = return acc
      | otherwise = do
          w <- peekElemOff p i
          go (i + 1) (acc + popCount (survivorsMasked lo hi (i `unsafeShiftL` 6) w))

-- Bit index of the r-th (r >= 1) of the set bits of s.
nthSetBit :: Word64 -> Int -> Int
nthSetBit !s !r
  | r <= 1    = countTrailingZeros s
  | otherwise = nthSetBit (s .&. (s - 1)) (r - 1)

-- Survivor count of the segment with first byte index k (nW words), restricted to the segment
-- bit indices [lo, hi).
countSeg :: Int -> U.Vector Word32 -> Int -> Int -> Int -> Int -> Int
countSeg largeFrom bps k nW lo hi =
  unsafePerformIO $ withPage largeFrom bps k nW $ \p -> countWords p lo hi
{-# NOINLINE countSeg #-}

-- Segment bit index of the r-th (r >= 1) survivor in [lo, hi) of the segment; the caller
-- guarantees that it exists.
selectSeg :: Int -> U.Vector Word32 -> Int -> Int -> Int -> Int -> Int -> Int
selectSeg largeFrom bps k nW lo hi r = unsafePerformIO $ withPage largeFrom bps k nW $ \p -> do
  let !iEnd = (hi + 63) `unsafeShiftR` 6
      go !i !rr
        | i >= iEnd = error "Eratosthenes.selectSeg: rank out of range"
        | otherwise = do
            w <- peekElemOff p i
            let !base = i `unsafeShiftL` 6
                !s    = survivorsMasked lo hi base w
                !c    = popCount s
            if rr <= c then return (base + nthSetBit s rr) else go (i + 1) (rr - c)
  go (lo `unsafeShiftR` 6) r
{-# NOINLINE selectSeg #-}

-- Spark every element, then hand the list back.
sparkAll :: [a] -> [a]
sparkAll xs = foldr par xs xs

-- | Base primes (those >= 19, below 2^32) together with the limit they are complete to.
data Base = Base !Word64 !(U.Vector Word32)

emptyBase :: Base
emptyBase = Base 0 U.empty

-- | The base primes needed for sieving values up to the bound (at most 2^64 - 1).  Building them
--   is a sequential sieve up to the square root of the bound: about 0.2 s for 10^18 and a few
--   seconds for 2^64 (203 million primes, 813 MB), so a program doing several searches should
--   build them once and pass them to the @Cfg@ functions.
baseFor :: Integer -> Base
baseFor bound = Base plim (basePrimes plim)
  where plim = min maxBasePrimeLimit (isqrtW (satTop bound) + 1)

-- | Number of base primes held.
baseSize :: Base -> Int
baseSize (Base _ v) = U.length v

-- Make the base primes sufficient for sieving values below the given end value.
growBase :: Integer -> Base -> Base
growBase endVal b@(Base plim _)
  | need <= plim = b
  | otherwise    = let plim' = min maxBasePrimeLimit (max need (2 * plim))
                   in Base plim' (basePrimes plim')
  where
    need = isqrtW (satTop endVal) + 1

-- Survivor counts of the whole segments qs, restricted to the global wheel indices
-- [gLo, gHi), computed by sparks (in the order of qs).
groupCounts :: SieveCfg -> U.Vector Word32 -> Int -> Int -> [Int] -> [Int]
groupCounts cfg bps gLo gHi qs = sparkAll
  [ countSeg (cfgLargeFrom cfg) bps (q * sBytes) sWords (max 0 (gLo - base)) (min sBits (gHi - base))
  | q <- qs
  , let base = q * sBits ]
  where
    sBytes = segBytesOf cfg
    sBits  = segBitsOf cfg
    sWords = segWordsOf cfg

-- The first segment (of the given indices and their counts) holding the r-th survivor:
-- (segment, rank within the segment from the bottom of its counted range, count of the segment),
-- or what remains of r.
findSeg :: [Int] -> [Int] -> Integer -> Either Integer (Int, Integer, Int)
findSeg (q : qs) (c : cs) !r
  | r <= toInteger c = Right (q, r, c)
  | otherwise        = findSeg qs cs (r - toInteger c)
findSeg _ _ !r = Left r

-- | @countPrimesBetween x y@ is the number of primes p with @x < p <= y@, found by sieving
--   (cost proportional to @y - x@, plus the base primes up to the square root of @y@).
--   Segments are sieved in parallel with the threaded RTS.  Requires @y <= 2^64 - 1@.
countPrimesBetween :: Integer -> Integer -> Integer
countPrimesBetween = countPrimesBetweenCfg defaultCfg emptyBase

-- | 'countPrimesBetween' with explicit parameters and a prebuilt 'Base' (which is extended
--   if it turns out to be too small).
countPrimesBetweenCfg :: SieveCfg -> Base -> Integer -> Integer -> Integer
countPrimesBetweenCfg cfg base0 x0 y
  | y > maxValue = error "countPrimesBetween: bound exceeds 2^64 - 1"
  | y <= x       = 0
  | otherwise    = nSmall + toInteger (go q0 base0 0)
  where
    x      = max 0 x0
    nSmall = toInteger (length [ p | p <- [2, 3, 5 :: Integer], x < p, p <= y ])
    sBits  = segBitsOf cfg
    gLo    = wheelCount x
    gHi    = wheelCount y
    q0     = gLo `quot` sBits
    qN     = (gHi - 1) `quot` sBits
    go :: Int -> Base -> Int -> Int
    go !q b !acc
      | gHi <= gLo || q > qN = acc
      | otherwise =
          let q1 = min qN (q + cfgGroupSegs cfg - 1)
          in case growBase (segEndValue cfg q1) b of
               b'@(Base _ bps) -> go (q1 + 1) b' (acc + sum (groupCounts cfg bps gLo gHi [q .. q1]))

-- | @nthPrimeAfter x r@ is the r-th prime (r >= 1) strictly greater than x (x >= 0), found by
--   sieving forward from x: whole segments are counted in parallel (groups of segments), then the
--   segment holding the target is sieved again and the survivor selected with
--   'countTrailingZeros'.  Intended for finishing @nthPrime@ from a known prime count
--   (x = the end of a counted range, r = n - count).  Errors if there is no such prime
--   below 2^64.
nthPrimeAfter :: Integer -> Integer -> Integer
nthPrimeAfter = nthPrimeAfterCfg defaultCfg emptyBase

-- | 'nthPrimeAfter' with explicit parameters and a prebuilt 'Base'.
nthPrimeAfterCfg :: SieveCfg -> Base -> Integer -> Integer -> Integer
nthPrimeAfterCfg cfg base0 x0 r0
  | r0 < 1           = error "nthPrimeAfter: rank must be positive"
  | r0 <= nSmall     = smalls !! fromInteger (r0 - 1)
  | x >= maxValue    = error "nthPrimeAfter: no such prime below 2^64"
  | otherwise        = go (gLo `quot` sBits) base0 (r0 - nSmall)
  where
    x      = max 0 x0
    smalls = [ p | p <- [2, 3, 5 :: Integer], p > x ]
    nSmall = toInteger (length smalls)
    sBytes = segBytesOf cfg
    sBits  = segBitsOf cfg
    sWords = segWordsOf cfg
    gLo    = wheelCount x
    gHi    = wheelCount maxValue
    qMax   = (gHi - 1) `quot` sBits
    go :: Int -> Base -> Integer -> Integer
    go !q b !r
      | q > qMax  = error "nthPrimeAfter: no such prime below 2^64"
      | otherwise =
          let q1 = min qMax (q + cfgGroupSegs cfg - 1)
          in case growBase (segEndValue cfg q1) b of
               b'@(Base _ bps) ->
                 case findSeg [q .. q1] (groupCounts cfg bps gLo gHi [q .. q1]) r of
                   Left r'             -> go (q1 + 1) b' r'
                   Right (qq, rr, _)   ->
                     let base = qq * sBits
                         bix  = selectSeg (cfgLargeFrom cfg) bps (qq * sBytes) sWords
                                          (max 0 (gLo - base)) (min sBits (gHi - base)) (fromInteger rr)
                     in valueOfIndex (base + bix)

-- | @nthPrimeAtOrBefore x r@ is the r-th largest prime (r >= 1) that is <= x (x <= 2^64 - 1),
--   found by sieving downwards from x like 'nthPrimeAfter' sieves upwards.  Errors if there
--   are fewer than r primes <= x.
nthPrimeAtOrBefore :: Integer -> Integer -> Integer
nthPrimeAtOrBefore = nthPrimeAtOrBeforeCfg defaultCfg emptyBase

-- | 'nthPrimeAtOrBefore' with explicit parameters and a prebuilt 'Base'.
nthPrimeAtOrBeforeCfg :: SieveCfg -> Base -> Integer -> Integer -> Integer
nthPrimeAtOrBeforeCfg cfg base0 x r0
  | r0 < 1        = error "nthPrimeAtOrBefore: rank must be positive"
  | x > maxValue  = error "nthPrimeAtOrBefore: bound exceeds 2^64 - 1"
  | otherwise     = descend qTop r0
  where
    smalls = [ p | p <- [5, 3, 2 :: Integer], p <= x ]
    nSm    = toInteger (length smalls)
    sBytes = segBytesOf cfg
    sBits  = segBitsOf cfg
    sWords = segWordsOf cfg
    gHi    = wheelCount x
    qTop   = if gHi <= 0 then -1 else (gHi - 1) `quot` sBits
    base1  = if qTop >= 0 then growBase (segEndValue cfg qTop) base0 else base0
    Base _ bps = base1
    descend :: Int -> Integer -> Integer
    descend !q !r
      | q < 0     = if r <= nSm then smalls !! fromInteger (r - 1)
                                else error "nthPrimeAtOrBefore: fewer than r primes"
      | otherwise =
          let q0 = max 0 (q - cfgGroupSegs cfg + 1)
              qs = [q, q - 1 .. q0]
          in case findSeg qs (groupCounts cfg bps 0 gHi qs) r of
               Left r'                -> descend (q0 - 1) r'
               Right (qq, rr, c)      ->
                 let base = qq * sBits
                     bix  = selectSeg (cfgLargeFrom cfg) bps (qq * sBytes) sWords
                                      0 (min sBits (gHi - base)) (c - fromInteger rr + 1)
                 in valueOfIndex (base + bix)
