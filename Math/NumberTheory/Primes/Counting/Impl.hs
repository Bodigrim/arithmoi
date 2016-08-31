-- |
-- Module:      Math.NumberTheory.Primes.Counting.Impl
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: non-portable
--
-- Number of primes not exceeding @n@, @&#960;(n)@, and @n@-th prime.
--
{-# LANGUAGE CPP, BangPatterns, FlexibleContexts #-}
{-# OPTIONS_GHC -fspec-constr-count=24 #-}
{-# OPTIONS_HADDOCK hide #-}
module Math.NumberTheory.Primes.Counting.Impl
    ( primeCount
    , primeCountMaxArg
    , nthPrime
    , nthPrimeMaxArg
    ) where

#include "MachDeps.h"

import Math.NumberTheory.Primes.Sieve.Eratosthenes
import Math.NumberTheory.Primes.Sieve.Indexing
import Math.NumberTheory.Primes.Counting.Approximate
import Math.NumberTheory.Powers.Squares
import Math.NumberTheory.Powers.Cubes
import Math.NumberTheory.Logarithms
import Math.NumberTheory.Unsafe

import Data.Array.ST
import Control.Monad.ST
import Data.Bits
import Data.Int

#if SIZEOF_HSWORD < 8
#define COUNT_T Int64
#else
#define COUNT_T Int
#endif

-- | Maximal allowed argument of 'primeCount'. Currently 8e18.
primeCountMaxArg :: Integer
primeCountMaxArg = 8000000000000000000

-- | @'primeCount' n == &#960;(n)@ is the number of (positive) primes not exceeding @n@.
--
--   For efficiency, the calculations are done on 64-bit signed integers, therefore @n@ must
--   not exceed 'primeCountMaxArg'.
--
--   Requires @/O/(n^0.5)@ space, the time complexity is roughly @/O/(n^0.7)@.
--   For small bounds, @'primeCount' n@ simply counts the primes not exceeding @n@,
--   for bounds from @30000@ on, Meissel's algorithm is used in the improved form due to
--   D.H. Lehmer, cf.
--   <http://en.wikipedia.org/wiki/Prime_counting_function#Algorithms_for_evaluating_.CF.80.28x.29>.
primeCount :: Integer -> Integer
primeCount n
    | n > primeCountMaxArg = error $ "primeCount: can't handle bound " ++ show n
    | n < 2     = 0
    | n < 1000  = fromIntegral . length . takeWhile (<= n) . primeList . primeSieve $ max 242 n
    | n < 30000 = runST $ do
        ba <- sieveTo n
        (s,e) <- getBounds ba
        ct <- countFromTo s e ba
        return (fromIntegral $ ct+3)
    | otherwise =
        let !ub = cop $ fromInteger n
            !sr = integerSquareRoot' ub
            !cr = nxtEnd $ integerCubeRoot' ub + 15
            nxtEnd k = k - (k `rem` 30) + 31
            !phn1 = calc ub cr
            !cs = cr+6
            !pdf = sieveCount ub cs sr
        in phn1 - pdf

-- | Maximal allowed argument of 'nthPrime'. Currently 1.5e17.
nthPrimeMaxArg :: Integer
nthPrimeMaxArg = 150000000000000000

-- | @'nthPrime' n@ calculates the @n@-th prime. Numbering of primes is
--   @1@-based, so @'nthPrime' 1 == 2@.
--
--   Requires @/O/((n*log n)^0.5)@ space, the time complexity is roughly @/O/((n*log n)^0.7@.
--   The argument must be strictly positive, and must not exceed 'nthPrimeMaxArg'.
nthPrime :: Integer -> Integer
nthPrime n
    | n < 1         = error "Prime indexing starts at 1"
    | n > nthPrimeMaxArg = error $ "nthPrime: can't handle index " ++ show n
    | n < 200000    = nthPrimeCt n
    | ct0 < n       = tooLow n p0 (n-ct0) approxGap
    | otherwise     = tooHigh n p0 (ct0-n) approxGap
      where
        p0 = nthPrimeApprox n
        approxGap = (7 * fromIntegral (integerLog2' p0)) `quot` 10
        ct0 = primeCount p0

--------------------------------------------------------------------------------
--                                The Works                                   --
--------------------------------------------------------------------------------

-- TODO: do something better in case we guess too high.
-- Not too pressing, since I think a) nthPrimeApprox always underestimates
-- in the range we can handle, and b) it's always "goodEnough"

tooLow :: Integer -> Integer -> Integer -> Integer -> Integer
tooLow n a miss gap
    | goodEnough    = lowSieve a miss
    | c1 < n        = lowSieve p1 (n-c1)
    | otherwise     = lowSieve a miss   -- a third count wouldn't make it faster, I think
      where
        est = miss*gap
        p1  = a + (est * 19) `quot` 20
        goodEnough = 3*est*est*est < 2*p1*p1    -- a second counting would be more work than sieving
        c1  = primeCount p1

tooHigh :: Integer -> Integer -> Integer -> Integer -> Integer
tooHigh n a surp gap
    | c < n     = lowSieve b (n-c)
    | otherwise = tooHigh n b (c-n) gap
      where
        b = a - (surp * gap * 11) `quot` 10
        c = primeCount b

lowSieve :: Integer -> Integer -> Integer
lowSieve a miss = countToNth (miss+rep) psieves
      where
        strt = if (fromInteger a .&. (1 :: Int)) == 1
                 then a+2
                 else a+1
        psieves@(PS vO ba:_) = psieveFrom strt
        rep | o0 < 0    = 0
            | otherwise = sum [1 | i <- [0 .. r2], ba `unsafeAt` i]
              where
                o0 = strt - vO - 9   -- (strt - 2) - v0 - 7
                r0 = fromInteger o0 `rem` 30
                r1 = r0 `quot` 3
                r2 = min 7 (if r1 > 5 then r1-1 else r1)

-- highSieve :: Integer -> Integer -> Integer -> Integer
-- highSieve a surp gap = error "Oh shit"

sieveCount :: COUNT_T -> COUNT_T -> COUNT_T -> Integer
sieveCount ub cr sr = runST $ do
    let psieves = psieveFrom (fromIntegral cr)
        pisr = approxPrimeCount sr
        picr = approxPrimeCount cr
        diff = pisr - picr
        size = fromIntegral (diff + diff `quot` 50) + 30
    store <- unsafeNewArray_ (0,size-1) :: ST s (STUArray s Int COUNT_T)
    let feed voff !wi !ri uar sves
          | ri == sieveBits = case sves of
                                (PS vO ba : more) -> feed (fromInteger vO) wi 0 ba more
                                _ -> error "prime stream ended prematurely"
          | pval > sr   = do
              stu <- unsafeThaw uar
              eat 0 0 voff (wi-1) ri stu sves
          | uar `unsafeAt` ri = do
              unsafeWrite store wi (ub `quot` pval)
              feed voff (wi+1) (ri+1) uar sves
          | otherwise = feed voff wi (ri+1) uar sves
            where
              pval = voff + toPrim ri
        eat !acc !btw voff !wi !si stu sves
            | si == sieveBits =
                case sves of
                  [] -> error "Premature end of prime stream"
                  (PS vO ba : more) -> do
                      nstu <- unsafeThaw ba
                      eat acc btw (fromInteger vO) wi 0 nstu more
            | wi < 0    = return acc
            | otherwise = do
                qb <- unsafeRead store wi
                let dist = qb - voff - 7
                if dist < fromIntegral sieveRange
                  then do
                      let (b,j) = idxPr (dist+7)
                          !li = (b `shiftL` 3) .|. j
                      new <- if li < si then return 0 else countFromTo si li stu
                      let nbtw = btw + fromIntegral new + 1
                      eat (acc+nbtw) nbtw voff (wi-1) (li+1) stu sves
                  else do
                      let (cpl,fds) = dist `quotRem` fromIntegral sieveRange
                          (b,j) = idxPr (fds+7)
                          !li = (b `shiftL` 3) .|. j
                          ctLoop !lac 0 (PS vO ba : more) = do
                              nstu <- unsafeThaw ba
                              new <- countFromTo 0 li nstu
                              let nbtw = btw + lac + 1 + fromIntegral new
                              eat (acc+nbtw) nbtw (fromIntegral vO) (wi-1) (li+1) nstu more
                          ctLoop lac s (ps : more) = do
                              !new <- countAll ps
                              ctLoop (lac + fromIntegral new) (s-1) more
                          ctLoop _ _ [] = error "Primes ended"
                      new <- countFromTo si (sieveBits-1) stu
                      ctLoop (fromIntegral new) (cpl-1) sves
    case psieves of
      (PS vO ba : more) -> feed (fromInteger vO) 0 0 ba more
      _ -> error "No primes sieved"

calc :: COUNT_T -> COUNT_T -> Integer
calc lim plim = runST $ do
    !parr <- sieveTo (fromIntegral plim)
    (plo,phi) <- getBounds parr
    !pct <- countFromTo plo phi parr
    !ar1 <- unsafeNewArray_ (0,end-1)
    unsafeWrite ar1 0 lim
    unsafeWrite ar1 1 1
    !ar2 <- unsafeNewArray_ (0,end-1)
    let go cap pix old new
            | pix == 2  =   coll cap old
            | otherwise = do
                isp <- unsafeRead parr pix
                if isp
                    then do
                        let !n = fromInteger (toPrim pix)
                        !ncap <- treat cap n old new
                        go ncap (pix-1) new old
                    else go cap (pix-1) old new
        coll stop ar =
            let cgo !acc i
                    | i < stop  = do
                        !k <- unsafeRead ar i
                        !v <- unsafeRead ar (i+1)
                        cgo (acc + fromIntegral v*cp6 k) (i+2)
                    | otherwise = return (acc+fromIntegral pct+2)
            in cgo 0 0
    go 2 start ar1 ar2
  where
    (bt,ri) = idxPr plim
    !start = 8*bt + ri
    !size = fromIntegral $ (integerSquareRoot lim) `quot` 4
    !end = 2*size

treat :: Int -> COUNT_T -> STUArray s Int COUNT_T -> STUArray s Int COUNT_T -> ST s Int
treat end n old new = do
    qi0 <- locate n 0 (end `quot` 2 - 1) old
    let collect stop !acc ix
            | ix < end  = do
                !k <- unsafeRead old ix
                if k < stop
                    then do
                        v <- unsafeRead old (ix+1)
                        collect stop (acc-v) (ix+2)
                    else return (acc,ix)
            | otherwise = return (acc,ix)
        goTreat !wi !ci qi
            | qi < end  = do
                !key <- unsafeRead old qi
                !val <- unsafeRead old (qi+1)
                let !q0 = key `quot` n
                    !r0 = fromIntegral (q0 `rem` 30030)
                    !nkey = q0 - fromIntegral (cpDfAr `unsafeAt` r0)
                    nk0 = q0 + fromIntegral (cpGpAr `unsafeAt` (r0+1) + 1)
                    !nlim = n*nk0
                (wi1,ci1) <- copyTo end nkey old ci new wi
                ckey <- unsafeRead old ci1
                (!acc, !ci2) <- if ckey == nkey
                                  then do
                                    !ov <- unsafeRead old (ci1+1)
                                    return (ov-val,ci1+2)
                                  else return (-val,ci1)
                (!tot, !nqi) <- collect nlim acc (qi+2)
                unsafeWrite new wi1 nkey
                unsafeWrite new (wi1+1) tot
                goTreat (wi1+2) ci2 nqi
            | otherwise = copyRem end old ci new wi
    goTreat 0 0 qi0

--------------------------------------------------------------------------------
--                               Auxiliaries                                  --
--------------------------------------------------------------------------------

locate :: COUNT_T -> Int -> Int -> STUArray s Int COUNT_T -> ST s Int
locate p low high arr = do
    let go lo hi
          | lo < hi     = do
            let !md = (lo+hi) `quot` 2
            v <- unsafeRead arr (2*md)
            case compare p v of
                LT -> go lo md
                EQ -> return (2*md)
                GT -> go (md+1) hi
          | otherwise   = return (2*lo)
    go low high

{-# INLINE copyTo #-}
copyTo :: Int -> COUNT_T -> STUArray s Int COUNT_T -> Int
       -> STUArray s Int COUNT_T -> Int -> ST s (Int,Int)
copyTo end lim old oi new ni = do
    let go ri wi
            | ri < end  = do
                ok <- unsafeRead old ri
                if ok < lim
                    then do
                        !ov <- unsafeRead old (ri+1)
                        unsafeWrite new wi ok
                        unsafeWrite new (wi+1) ov
                        go (ri+2) (wi+2)
                    else return (wi,ri)
            | otherwise = return (wi,ri)
    go oi ni

{-# INLINE copyRem #-}
copyRem :: Int -> STUArray s Int COUNT_T -> Int -> STUArray s Int COUNT_T -> Int -> ST s Int
copyRem end old oi new ni = do
    let go ri wi
          | ri < end    = do
            unsafeRead old ri >>= unsafeWrite new wi
            go (ri+1) (wi+1)
          | otherwise   = return wi
    go oi ni

{-# INLINE cp6 #-}
cp6 :: COUNT_T -> Integer
cp6 k =
  case k `quotRem` 30030 of
    (q,r) -> 5760*fromIntegral q +
                fromIntegral (cpCtAr `unsafeAt` fromIntegral r)

cop :: COUNT_T -> COUNT_T
cop m = m - fromIntegral (cpDfAr `unsafeAt` fromIntegral (m `rem` 30030))


--------------------------------------------------------------------------------
--                           Ugly helper arrays                               --
--------------------------------------------------------------------------------

cpCtAr :: UArray Int Int16
cpCtAr = runSTUArray $ do
    ar <- newArray (0,30029) 1
    let zilch s i
            | i < 30030 = unsafeWrite ar i 0 >> zilch s (i+s)
            | otherwise = return ()
        accumulate ct i
            | i < 30030 = do
                v <- unsafeRead ar i
                let !ct' = ct+v
                unsafeWrite ar i ct'
                accumulate ct' (i+1)
            | otherwise = return ar
    zilch 2 0
    zilch 6 3
    zilch 10 5
    zilch 14 7
    zilch 22 11
    zilch 26 13
    accumulate 1 2

cpDfAr :: UArray Int Int8
cpDfAr = runSTUArray $ do
    ar <- newArray (0,30029) 0
    let note s i
            | i < 30029 = unsafeWrite ar i 1 >> note s (i+s)
            | otherwise = return ()
        accumulate d i
            | i < 30029 = do
                v <- unsafeRead ar i
                if v == 0
                    then accumulate 2 (i+2)
                    else do unsafeWrite ar i d
                            accumulate (d+1) (i+1)
            | otherwise = return ar
    note 2 0
    note 6 3
    note 10 5
    note 14 7
    note 22 11
    note 26 13
    accumulate 2 3

cpGpAr :: UArray Int Int8
cpGpAr = runSTUArray $ do
    ar <- newArray (0,30030) 0
    unsafeWrite ar 30030 1
    let note s i
            | i < 30029 = unsafeWrite ar i 1 >> note s (i+s)
            | otherwise = return ()
        accumulate d i
            | i < 1     = return ar
            | otherwise = do
                v <- unsafeRead ar i
                if v == 0
                    then accumulate 2 (i-2)
                    else do unsafeWrite ar i d
                            accumulate (d+1) (i-1)
            | otherwise = return ar
    note 2 0
    note 6 3
    note 10 5
    note 14 7
    note 22 11
    note 26 13
    accumulate 2 30027

