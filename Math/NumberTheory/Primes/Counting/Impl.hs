-- |
-- Module:      Math.NumberTheory.Primes.Counting.Impl
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
--
-- Number of primes not exceeding @n@, @&#960;(n)@, and @n@-th prime.
--
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fspec-constr-count=24 #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Math.NumberTheory.Primes.Counting.Impl
    ( primeCount
    , primeCountMaxArg
    , nthPrime
    ) where

import Math.NumberTheory.Primes.Sieve.Eratosthenes
    (PrimeSieve(..), primeList, primeSieve, psieveFrom, sieveTo, sieveBits, sieveRange)
import Math.NumberTheory.Primes.Sieve.Indexing (toPrim, idxPr)
import Math.NumberTheory.Primes.Counting.Approximate (nthPrimeApprox, approxPrimeCount)
import Math.NumberTheory.Primes.Types
import Math.NumberTheory.Roots
import Math.NumberTheory.Utils.FromIntegral

import Control.Monad.ST
import Data.Array.Base
import Data.Array.ST
import Data.Bit
import Data.Bits
import Data.Int
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

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
    | n < 1000  = intToInteger . length . takeWhile (<= n) . map unPrime . primeList . primeSieve $ max 242 n
    | n < 30000 = runST $ do
        ba <- sieveTo n
        let (s, e) = (0, MU.length ba - 1)
        ct <- countFromTo s e ba
        return (intToInteger $ ct+3)
    | otherwise =
        let !ub = cop $ fromInteger n
            !sr = integerSquareRoot ub
            !cr = nxtEnd $ integerCubeRoot ub + 15
            nxtEnd k = k - (k `rem` 30) + 31
            !phn1 = calc ub cr
            !cs = cr+6
            !pdf = sieveCount ub cs sr
        in phn1 - pdf

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

-- highSieve :: Integer -> Integer -> Integer -> Integer
-- highSieve a surp gap = error "Oh shit"

sieveCount :: Int64 -> Int64 -> Int64 -> Integer
sieveCount ub cr sr = runST (sieveCountST ub cr sr)

sieveCountST :: forall s. Int64 -> Int64 -> Int64 -> ST s Integer
sieveCountST ub cr sr = do
    let psieves = psieveFrom (int64ToInteger cr)
        pisr = approxPrimeCount sr
        picr = approxPrimeCount cr
        diff = pisr - picr
        size = int64ToInt (diff + diff `quot` 50) + 30
    store <- MU.unsafeNew size :: ST s (MU.MVector s Int64)
    let feed :: Int64 -> Int -> Int -> U.Vector Bit -> [PrimeSieve] -> ST s Integer
        feed voff !wi !ri uar sves
          | ri == sieveBits = case sves of
                                (PS vO ba : more) -> feed (fromInteger vO) wi 0 ba more
                                _ -> error "prime stream ended prematurely"
          | pval > sr   = do
              stu <- U.unsafeThaw uar
              eat 0 0 voff (wi-1) ri stu sves
          | unBit (uar `U.unsafeIndex` ri) = do
              MU.unsafeWrite store wi (ub `quot` pval)
              feed voff (wi+1) (ri+1) uar sves
          | otherwise = feed voff wi (ri+1) uar sves
            where
              pval = voff + toPrim ri
        eat :: Integer -> Integer -> Int64 -> Int -> Int -> MU.MVector s Bit -> [PrimeSieve] -> ST s Integer
        eat !acc !btw voff !wi !si stu sves
            | si == sieveBits =
                case sves of
                  [] -> error "Premature end of prime stream"
                  (PS vO ba : more) -> do
                      nstu <- U.unsafeThaw ba
                      eat acc btw (fromInteger vO) wi 0 nstu more
            | wi < 0    = return acc
            | otherwise = do
                qb <- MU.unsafeRead store wi
                let dist = qb - voff - 7
                if dist < intToInt64 sieveRange
                  then do
                      let (b,j) = idxPr (dist+7)
                          !li = (b `shiftL` 3) .|. j
                      new <- if li < si then return 0 else countFromTo si li stu
                      let nbtw = btw + intToInteger new + 1
                      eat (acc+nbtw) nbtw voff (wi-1) (li+1) stu sves
                  else do
                      let (cpl,fds) = dist `quotRem` intToInt64 sieveRange
                          (b,j) = idxPr (fds+7)
                          !li = (b `shiftL` 3) .|. j
                          ctLoop !lac 0 (PS vO ba : more) = do
                              nstu <- U.unsafeThaw ba
                              new <- countFromTo 0 li nstu
                              let nbtw = btw + lac + 1 + intToInteger new
                              eat (acc+nbtw) nbtw (integerToInt64 vO) (wi-1) (li+1) nstu more
                          ctLoop lac s (ps : more) = do
                              let !new = countAll ps
                              ctLoop (lac + intToInteger new) (s-1) more
                          ctLoop _ _ [] = error "Primes ended"
                      new <- countFromTo si (sieveBits-1) stu
                      ctLoop (intToInteger new) (cpl-1) sves
    case psieves of
      (PS vO ba : more) -> feed (fromInteger vO) 0 0 ba more
      _ -> error "No primes sieved"

calc :: Int64 -> Int64 -> Integer
calc lim plim = runST (calcST lim plim)

calcST :: forall s. Int64 -> Int64 -> ST s Integer
calcST lim plim = do
    !parr <- sieveTo (int64ToInteger plim)
    let (plo, phi) = (0, MU.length parr - 1)
    !pct <- countFromTo plo phi parr
    !ar1 <- MU.unsafeNew end
    MU.unsafeWrite ar1 0 lim
    MU.unsafeWrite ar1 1 1
    !ar2 <- MU.unsafeNew end
    let go :: Int -> Int -> MU.MVector s Int64 -> MU.MVector s Int64 -> ST s Integer
        go cap pix old new
            | pix == 2  =   coll cap old
            | otherwise = do
                Bit isp <- MU.unsafeRead parr pix
                if isp
                    then do
                        let !n = fromInteger (toPrim pix)
                        !ncap <- treat cap n old new
                        go ncap (pix-1) new old
                    else go cap (pix-1) old new
        coll :: Int -> MU.MVector s Int64 -> ST s Integer
        coll stop ar =
            let cgo !acc i
                    | i < stop  = do
                        !k <- MU.unsafeRead ar i
                        !v <- MU.unsafeRead ar (i+1)
                        cgo (acc + int64ToInteger v*cp6 k) (i+2)
                    | otherwise = return (acc+intToInteger pct+2)
            in cgo 0 0
    go 2 start ar1 ar2
  where
    (bt,ri) = idxPr plim
    !start = 8*bt + ri
    !size = int64ToInt $ integerSquareRoot lim `quot` 4
    !end = 2*size

treat :: Int -> Int64 -> MU.MVector s Int64 -> MU.MVector s Int64 -> ST s Int
treat end n old new = do
    qi0 <- locate n 0 (end `quot` 2 - 1) old
    let collect stop !acc ix
            | ix < end  = do
                !k <- MU.unsafeRead old ix
                if k < stop
                    then do
                        v <- MU.unsafeRead old (ix+1)
                        collect stop (acc-v) (ix+2)
                    else return (acc,ix)
            | otherwise = return (acc,ix)
        goTreat !wi !ci qi
            | qi < end  = do
                !key <- MU.unsafeRead old qi
                !val <- MU.unsafeRead old (qi+1)
                let !q0 = key `quot` n
                    !r0 = int64ToInt (q0 `rem` 30030)
                    !nkey = q0 - int8ToInt64 (cpDfAr `unsafeAt` r0)
                    nk0 = q0 + int8ToInt64 (cpGpAr `unsafeAt` (r0+1) + 1)
                    !nlim = n*nk0
                (wi1,ci1) <- copyTo end nkey old ci new wi
                ckey <- MU.unsafeRead old ci1
                (!acc, !ci2) <- if ckey == nkey
                                  then do
                                    !ov <- MU.unsafeRead old (ci1+1)
                                    return (ov-val,ci1+2)
                                  else return (-val,ci1)
                (!tot, !nqi) <- collect nlim acc (qi+2)
                MU.unsafeWrite new wi1 nkey
                MU.unsafeWrite new (wi1+1) tot
                goTreat (wi1+2) ci2 nqi
            | otherwise = copyRem end old ci new wi
    goTreat 0 0 qi0

--------------------------------------------------------------------------------
--                               Auxiliaries                                  --
--------------------------------------------------------------------------------

locate :: Int64 -> Int -> Int -> MU.MVector s Int64 -> ST s Int
locate p low high arr = do
    let go lo hi
          | lo < hi     = do
            let !md = (lo+hi) `quot` 2
            v <- MU.unsafeRead arr (2*md)
            case compare p v of
                LT -> go lo md
                EQ -> return (2*md)
                GT -> go (md+1) hi
          | otherwise   = return (2*lo)
    go low high

{-# INLINE copyTo #-}
copyTo :: Int -> Int64 -> MU.MVector s Int64 -> Int
       -> MU.MVector s Int64 -> Int -> ST s (Int,Int)
copyTo end lim old oi new ni = do
    let go ri wi
            | ri < end  = do
                ok <- MU.unsafeRead old ri
                if ok < lim
                    then do
                        !ov <- MU.unsafeRead old (ri+1)
                        MU.unsafeWrite new wi ok
                        MU.unsafeWrite new (wi+1) ov
                        go (ri+2) (wi+2)
                    else return (wi,ri)
            | otherwise = return (wi,ri)
    go oi ni

{-# INLINE copyRem #-}
copyRem :: Int -> MU.MVector s Int64 -> Int -> MU.MVector s Int64 -> Int -> ST s Int
copyRem end old oi new ni = do
    let go ri wi
          | ri < end    = do
            MU.unsafeRead old ri >>= MU.unsafeWrite new wi
            go (ri+1) (wi+1)
          | otherwise   = return wi
    go oi ni

{-# INLINE cp6 #-}
cp6 :: Int64 -> Integer
cp6 k =
  case k `quotRem` 30030 of
    (q,r) -> 5760*int64ToInteger q +
                int16ToInteger (cpCtAr `unsafeAt` int64ToInt r)

cop :: Int64 -> Int64
cop m = m - int8ToInt64 (cpDfAr `unsafeAt` int64ToInt (m `rem` 30030))


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
    note 2 0
    note 6 3
    note 10 5
    note 14 7
    note 22 11
    note 26 13
    accumulate 2 30027

-------------------------------------------------------------------------------
-- Prime counting

-- find the n-th set bit in a list of PrimeSieves,
-- aka find the (n+3)-rd prime
countToNth :: Int -> [PrimeSieve] -> Integer
countToNth !_ [] = error "countToNth: Prime stream ended prematurely"
countToNth !n (PS v0 bs : more) = case nthBitIndex (Bit True) n bs of
  Just i -> v0 + toPrim i
  Nothing -> countToNth (n - countBits bs) more

-- count all set bits in a chunk, do it wordwise for speed.
countAll :: PrimeSieve -> Int
countAll (PS _ bs) = countBits bs

-- count set bits between two indices (inclusive)
-- start and end must both be valid indices and start <= end
countFromTo :: Int -> Int -> MU.MVector s Bit -> ST s Int
countFromTo start end =
  fmap countBits . U.unsafeFreeze . MU.slice start (end - start + 1)
