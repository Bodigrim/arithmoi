-- |
-- Module:      Math.NumberTheory.Primes.Counting.Impl
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
--
-- Number of primes not exceeding @limit@, @&#960;(limit)@, and @n@-th prime.
--
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -fspec-constr-count=24 #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Math.NumberTheory.Primes.Counting.Impl
    ( primeCount
    , primeCountMaxArg
    , nthPrime
    ) where

import Math.NumberTheory.Primes.Sieve.Eratosthenes
                             (PrimeSieve(..), primeSieve, psieveFrom)
import Math.NumberTheory.Primes.Sieve.Indexing (toPrim)
import Math.NumberTheory.Primes.Counting.Approximate (nthPrimeApprox)
import Math.NumberTheory.Primes.Types
import Math.NumberTheory.Roots (integerSquareRoot)
import Math.NumberTheory.Utils.FromIntegral

import Data.Word (Word64, Word32)
import Data.Bits (Bits(shiftR, (.&.), (.|.)))
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Array.Base (STUArray, MArray(unsafeNewArray_),
                        unsafeAt, unsafeFreezeSTUArray, unsafeRead, unsafeWrite)
import Data.Bit (Bit(..), unBit, nthBitIndex, countBits)
import qualified Data.Vector.Unboxed as U

-- | Maximal allowed argument of 'primeCount'. Currently 8e18.
primeCountMaxArg :: Integer
primeCountMaxArg = 8000000000000000000

-- | @'primeCount' limit == &#960;(limit)@ is the number of primes not exceeding @limit@.
--
--   For efficiency, the calculations are done on 64-bit unsigned integers, therefore @limit@
--   must not exceed 'primeCountMaxArg'.
--
--   Requires @/O/(limit^0.5)@ space, the time complexity is roughly @/O/(limit^0.7)@.
--   @'primeCount' limit@ uses Legendre's algorithm in an improved form using "partial sieving"
--   and processing by "splitting" based on whether the product of the "base prime" and a
--   product of higher co-primes is less than or equal to, or greater than the square root of
--   @limit@ where "base primes" are lower than the square root of the square root of @limit@;
--   above this limit all remaining values are primes so are used in unique prime pairs to
--   calculate the additional amounts to add to the accumulated "Phi" for the answer.

--   NOTE: This is not related to the later work (about 1870) by Daniel Friedrich Ernst Meissel,
--   nor the extension to Meissel's work by Professor D. H. Lehmer in 1959 to adapt these types
--   of algorithms to use on a mainframe computer of that time; both of whose purpose was to
--   reduce the number of calculations for a given counting range and/or reduce the storage
--   requirements.  Neither of these used "partial sieving" to get anywhere near the asymptotic
--   complexity of this implementation.

--   See the "HowPrimeCountingWorks.md" file in this directory for a more complete explanation
--   of how this implementation works.

primeCount :: Integer -> Integer
primeCount limit
  | limit > primeCountMaxArg = error $ "primeCount: can't handle bound " ++ show limit
  | limit < 9 = if limit < 2 then 0 else (limit + 1) `div` 2
  | otherwise =
  let
    
    -- initialize constants...
    ilimit = fromIntegral limit
    sqrtlmt = fromIntegral $ integerSquareRoot ilimit
    sqrtsqrtlmt = integerSquareRoot sqrtlmt
    maxndx = toIndex sqrtlmt -- last index of arrays

    -- `numbps` will be the number of odd base primes up to `limit`^(1/4);
    -- `roughssz` is the current effective length of `roughs` after reduction;
    -- `phindxs` is the count of odd primes to limit by index not including bps,
    --    the above is an index to the `roughs`/`phis` that repr the index;
    -- `roughs` are the values remaining after culling base primes < its index,
    --    the above values when divided by two is the index for `phindxs`;
    -- `phis` is count of odd primes to a limit set by its index...
    (numbps, roughssz, phindxs, roughs, phis) = runST $ do -- run in ST monad...

      -- initialize monadic versions of `phindxs`/`roughs`/`phis`...
      mphindxs <- unsafeNewArray_ (0, maxndx) :: ST s (STUArray s Int Word32)
      -- initially is odd phi of the index repr 3 shr 1 is 1 -> value 1, etc...
      forM_ [ 0 .. maxndx ] $ \ i -> unsafeWrite mphindxs i (fromIntegral i)
      unsafeWrite mphindxs 0 1 -- for correctness, never used!
      mrs <- unsafeNewArray_ (0, maxndx) :: ST s (STUArray s Int Word32)
      -- odd values from 1 as in 1, 3 ... to maximum for roughs...
      forM_ [ 0 .. maxndx ] $ \ i ->
        unsafeWrite mrs i (fromIntegral i * 2 + 1)
      mphis <- unsafeNewArray_ (0, maxndx) :: ST s (STUArray s Int Word64)
      -- initialized to limit // roughs; are phis including count for one...
      forM_ [ 0 .. maxndx ] $ \ i -> do
        r <- unsafeRead mrs i; let d = fromIntegral r
        unsafeWrite mphis i (fromIntegral $ phip2 $ divide ilimit d)

      -- all work requiring modifying arrays and values done recursively here;
      -- the "partial sieving" loop with one `bp` sieving pass per loop;
      -- uses `roi` output and `rii` input roughs processing indices...
      let loop !nbps !rsilmt = do -- `rslmt` is maximum current ndx for `roughs`
            bpw32 <- unsafeRead mrs 1
            let bp = fromIntegral bpw32
            if bp > sqrtsqrtlmt then do -- means `bp` primes <= limit^(1/4)
              fmsops <- unsafeFreezeSTUArray mphindxs
              fmrs <- unsafeFreezeSTUArray mrs
              fmlops <- unsafeFreezeSTUArray mphis
              return (nbps, rsilmt + 1, fmsops, fmrs, fmlops) -- done loop!
            else do -- for each base prime `bp`...
                let -- mark `mrs` values that are multiples of bp if still there
                    cullmrs cullpos =
                      if cullpos > sqrtlmt then return () else do
                        cnt <- unsafeRead mphindxs (cullpos `shiftR` 1)
                        let ndx = fromIntegral cnt - nbps
                        tstr <- unsafeRead mrs ndx
                        when (tstr == fromIntegral cullpos)
                          (unsafeWrite mrs ndx 0)
                        cullmrs (cullpos + bp + bp)

                    -- recursive function to process all remaining `mrs` by
                    -- forming products of unique pairs with `bp`...
                    split rii !roi =
                      if rii > rsilmt then return (roi - 1) else do
                        m <- unsafeRead mrs rii -- multiplier may not be prime!
                        if m == 0 then split (rii + 1) roi else do -- skip marked
                          -- only unculled values; may not be prime
                          olv <- unsafeRead mphis rii -- large odd "pi" to adjust
                          let mbp = fromIntegral m * fromIntegral bp
                          adjv <- -- depends on condition...
                            if mbp <= fromIntegral sqrtlmt then do
                              -- ilimit `div` mbp too large...
                              let cnti = fromIntegral mbp `shiftR` 1
                              adji <- unsafeRead mphindxs cnti
                              let adjndx = fromIntegral adji - nbps
                              unsafeRead mphis adjndx
--                              adj <- unsafeRead mphis adjndx
--                              return $ adj - fromIntegral nbps
                            else do
                              -- ilimit `div` mbp in index range; use directly!
                              let adjndx = toIndex (divide ilimit mbp)
                              adj <- unsafeRead mphindxs adjndx -- phi form...
                              return $ fromIntegral adj - fromIntegral nbps + 1
                          -- write adjusted value into `mphis` at new offset...
                          unsafeWrite mphis roi (olv - adjv)
                          unsafeWrite mrs roi m -- move rougn values in sync
                          split (rii + 1) (roi + 1) -- recursively loop 

                    -- update `mphindxs` array for last cull pass...
                    adjcnt cm !mxci = -- cull multiple and maximum index
                      if cm < bp then return () else do
                        ofstc <- unsafeRead mphindxs (cm `shiftR` 1)
                        let c = ofstc - fromIntegral nbps
                            e = (cm * bp) `shiftR` 1
                            adjci ci =
                              if ci < e then adjcnt (cm - 2) ci else do
                                ov <- unsafeRead mphindxs ci
                                unsafeWrite mphindxs ci (ov - c)
                                adjci (ci - 1)
                        adjci mxci
                
                -- the code that uses the above "let"'s...
                unsafeWrite mrs 1 0 -- mark first non-one rough for deletion
                cullmrs (bp * bp) -- mark the other rough multiples of `bp`
                maxrsi <- split 0 0 -- adjust `roughs` and `phis` for cull
                let topcullpnt = (sqrtlmt `div` bp - 1) .|. 1 -- odd <= sqrtlmt
                adjcnt topcullpnt maxndx -- update `phindxs` for culling pass
                loop (nbps + 1) maxrsi -- recurse for all base primes

      loop 0 maxndx -- calling recursive "partial sieving" loop!

    -- the offset of the sum of the other `phis`...
    othroddpis = sum [ unsafeAt phis bpi | bpi <- [ 1 .. roughssz - 1 ] ]
    -- subtracted from the first first element of `phis`...
    phi0 = unsafeAt phis 0 - othroddpis -- + pi0crct -- to produce an intermediate phi
    
    -- recursively calculate the additional odd "phis" for all pairs of
    -- unique primes/`roughs` starting above limit^(1/4) to limit^(1/2);
    -- these are exactly the remaining values in `roughs` above the "one";
    -- Note that all roughs above the first "one" element are now prime;
    -- pre-comp of all additional ones for following "pairs" calculation...
    phi0adj = fromIntegral $ (roughssz - 2) * (roughssz - 1) `div` 2
    accum p1i !ans =
      if p1i >= roughssz - 1 then ans else -- for all roughs skipping "one"...
      let p1 = fromIntegral $ unsafeAt roughs p1i -- `p1` - first of prime pair
          qp1 = ilimit `div` p1 -- pre divide for "p1" value
          ndx = unsafeAt phindxs (toIndex (fromIntegral (qp1 `div` p1)))
          endndx = fromIntegral ndx - numbps -- last `p1` index!
          adj = fromIntegral $ (endndx - p1i) * (numbps + p1i - 1)
          comp p2i !ac = -- `rii` is index of the second of the `roughs`
            if p2i > endndx then accum (p1i + 1) ac else -- exit if reach end index!
            let p2 = fromIntegral (unsafeAt roughs p2i) -- second rough
                cnti = toIndex (divide qp1 p2) -- ndx for comp "pi"
            in comp (p2i + 1) (ac + fromIntegral (unsafeAt phindxs cnti))
      in if endndx <= p1i then ans -- terminate if `p1`^3 >= `ilimit`!
         -- adjust for ones added and not used due to `endndx` termination...
         else comp (p1i + 1) (ans - adj)

    numsqrtprms = fromIntegral $ numbps + roughssz

  -- finally call addition of the phis for all pairs of unique primes added to
  -- the offset of the other odd "phis" already adjusted for the subtracting
  -- of odd "phis" from odd "phis" plus the total primes to the square root of
  -- the `limit` counting range minus one according to the formula...
  in fromIntegral $ accum 1 (phi0 + phi0adj) + numsqrtprms - 1

--------------------------------------------------------------------------------
--                               Auxiliaries                                  --
--------------------------------------------------------------------------------

{-# INLINE divide #-}
divide :: Word64 -> Word64 -> Int
divide n d = fromIntegral $ n `div` d

{-# INLINE phip2 #-}
phip2 :: Int -> Int
phip2 x = (x + 1) `shiftR` 1    

{-# INLINE toIndex #-}
toIndex :: Int -> Int
toIndex x = (x - 1) `shiftR` 1

--------------------------------------------------------------------------------
--                                Nth Prime                                   --
--------------------------------------------------------------------------------

-- | @'nthPrime' n@ calculates the @n@-th prime. Numbering of primes is
--   @1@-based, so @'nthPrime' 1 == 2@.
--
--   Requires @/O/((n*log n)^0.5)@ space, the time complexity is roughly @/O/((n*log n)^0.7@.
--   The argument must be strictly positive.
nthPrime :: Int -> Prime Integer
nthPrime 1 = Prime 2
nthPrime 2 = Prime 3
nthPrime 3 = Prime 5
nthPrime 4 = Prime 7
nthPrime 5 = Prime 11
nthPrime 6 = Prime 13
nthPrime n
    | n < 1
    = error "Prime indexing starts at 1"
    | n < 200000
    = Prime $ countToNth (n - 3) [primeSieve (p0 + p0 `quot` 32 + 37)]
    | p0 > toInteger (maxBound :: Int)
    = error $ "nthPrime: index " ++ show n ++ " is too large to handle"
    | miss > 0
    = Prime $ tooLow  n (fromInteger p0) miss
    | otherwise
    = Prime $ tooHigh n (fromInteger p0) (negate miss)
      where
        p0 = nthPrimeApprox (toInteger n)
        miss = n - fromInteger (primeCount p0)

--------------------------------------------------------------------------------
--                                The Works                                   --
--------------------------------------------------------------------------------

-- TODO: do something better in case we guess too high.
-- Not too pressing, since I think a) nthPrimeApprox always underestimates
-- in the range we can handle, and b) it's always "goodEnough"

tooLow :: Int -> Int -> Int -> Integer
tooLow n p0 shortage
  | p1 > toInteger (maxBound :: Int)
  = error $ "nthPrime: index " ++ show n ++ " is too large to handle"
  | goodEnough
  = lowSieve p0 shortage
  | c1 < n
  = lowSieve (fromInteger p1) (n-c1)
  | otherwise
  = lowSieve p0 shortage   -- a third count wouldn't make it faster, I think
  where
    gap = truncate (log (intToDouble p0 :: Double))
    est = toInteger shortage * gap
    p1  = toInteger p0 + est
    goodEnough = 3*est*est*est < 2*p1*p1    -- a second counting would be more work than sieving
    c1  = fromInteger (primeCount p1)

tooHigh :: Int -> Int -> Int -> Integer
tooHigh n p0 surplus
  | c < n
  = lowSieve b (n-c)
  | otherwise
  = tooHigh n b (c-n)
  where
    gap = truncate (log (intToDouble p0 :: Double))
    b = p0 - (surplus * gap * 11) `quot` 10
    c = fromInteger (primeCount (toInteger b))

lowSieve :: Int -> Int -> Integer
lowSieve a miss = countToNth (miss+rep) psieves
      where
        strt = a + 1 + (a .&. 1)
        psieves@(PS vO ba:_) = psieveFrom (toInteger strt)
        rep | o0 < 0    = 0
            | otherwise = sum [1 | i <- [0 .. r2], unBit (ba `U.unsafeIndex` i)]
              where
                o0 = toInteger strt - vO - 9   -- (strt - 2) - v0 - 7
                r0 = fromInteger o0 `rem` 30
                r1 = r0 `quot` 3
                r2 = min 7 (if r1 > 5 then r1-1 else r1)

-------------------------------------------------------------------------------
-- Prime counting

-- find the n-th set bit in a list of PrimeSieves,
-- aka find the (n+3)-rd prime
countToNth :: Int -> [PrimeSieve] -> Integer
countToNth !_ [] = error "countToNth: Prime stream ended prematurely"
countToNth !n (PS v0 bs : more) = case nthBitIndex (Bit True) n bs of
  Just i -> v0 + toPrim i
  Nothing -> countToNth (n - countBits bs) more
