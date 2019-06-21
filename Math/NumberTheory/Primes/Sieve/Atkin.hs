-- |
-- Module:      Math.NumberTheory.Primes.Sieve.Atkin
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Atkin sieve.
--

{-# LANGUAGE BangPatterns #-}

module Math.NumberTheory.Primes.Sieve.Atkin
  ( atkinPrimeList
  , atkinSieve
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Bit
import Data.Foldable
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

import Math.NumberTheory.Moduli.Chinese
import Math.NumberTheory.Roots
import qualified Math.NumberTheory.Primes.Sieve.Eratosthenes as E
import Math.NumberTheory.Primes.Types
import Math.NumberTheory.Utils

atkinPrimeList :: PrimeSieve -> [Int]
atkinPrimeList (PrimeSieve low len segments)
  | len60 == 0 = []
  | len60 == 1 = takeWhile (< high) $ dropWhile (< low) $ 2 : 3 : 5 : doIx 0
  | otherwise
  =  dropWhile (< low) (2 : 3 : 5 : doIx 0)
  ++ concatMap doIx [1 .. len60 - 2]
  ++ takeWhile (< high) (doIx $ len60 - 1)
  where
    low60 = low `quot` 60
    len60 = (low + len + 59) `quot` 60 - low60
    high = low + len

    js = map fromWheel30 [0..15]

    doIx k
      = map ((+ 60 * (low60 + k)) . fst)
      $ filter (unBit . snd)
      $ zip js
      $ toList
      $ V.map (U.! k) segments

data PrimeSieve = PrimeSieve
  { _psLowBound :: !Int
  , _psLength   :: !Int
  , _psSegments :: V.Vector (U.Vector Bit)
  } deriving (Show)

atkinSieve
  :: Int
  -> Int
  -> PrimeSieve
atkinSieve low len = PrimeSieve low len segments
  where
    low60 = low `quot` 60
    len60 = (low + len + 59) `quot` 60 - low60
    params = V.generate 16 (\i -> SieveParams (fromWheel30 i) low60 len60)
    segments = V.map sieveSegment params

data SieveParams = SieveParams
  { spDelta    :: !Int
  , spLowBound :: !Int
  , spLength   :: !Int
  } deriving (Show)

spHighBound :: SieveParams -> Int
spHighBound sp = spLowBound sp + spLength sp

sieveSegment
  :: SieveParams
  -> U.Vector Bit
sieveSegment sp = runST $ do
  vec <- MU.new (spLength sp)
  U.forM_ (fgs V.! toWheel30 (spDelta sp)) $
    traverseLatticePoints sp vec
  algo3steps456 sp vec
  U.unsafeFreeze vec

-- | Solutions of k * f^2 + l * g^2 = delta (mod 60)
-- where (k, l) = (4, 1) for delta = 1 (mod 4)
--              = (3, 1) for delta = 1 (mod 6)
--              = (3,-1) for delta =11 (mod 12)
fgs :: V.Vector (U.Vector (Int, Int))
fgs = V.generate 16 (dispatch . fromWheel30)
  where
    dispatch delta
      | delta `mod` 4 == 1
      = U.fromList [ (f, g) | f <- [1..15], g <- [1..30], (4*f*f + g*g - delta) `rem` 60 == 0]
      | delta `mod` 6 == 1
      = U.fromList [ (f, g) | f <- [1..10], g <- [1..30], (3*f*f + g*g - delta) `rem` 60 == 0]
      | delta `mod` 12 == 11
      = U.fromList [ (f, g) | f <- [1..10], g <- [1..30], (3*f*f - g*g - delta) `rem` 60 == 0]
      | otherwise
      = error "fgs: unexpected delta"

traverseLatticePoints
  :: SieveParams
  -> MU.MVector s Bit
  -> (Int, Int)
  -> ST s ()
traverseLatticePoints sp vec (x0, y0)
  | spDelta sp `mod` 4 == 1
  = traverseLatticePoints1 sp vec (x0, y0)
  | spDelta sp `mod` 6 == 1
  = traverseLatticePoints2 sp vec (x0, y0)
  | spDelta sp `mod` 12 == 11
  = traverseLatticePoints3 sp vec (x0, y0)
  | otherwise
  = error "traverseLatticePoints: unexpected delta"

traverseLatticePoints1
  :: SieveParams
  -> MU.MVector s Bit
  -> (Int, Int)
  -> ST s ()
traverseLatticePoints1 !sp vec (!x0, !y0) =
  go kMax xMax y0
  where
    forwardY  (k, y) = (k +     y + 15, y + 30)
    forwardX  (k, x) = (k + 2 * x + 15, x + 15)
    backwardX (k, x) = (k - 2 * x + 15, x - 15)

    -- Step 1
    k0 = (4 * x0 * x0 + y0 * y0 - spDelta sp) `quot` 60 - spLowBound sp

    -- Step 2
    (kMax, xMax)
      = backwardX
      $ head
      $ dropWhile (\(k, _) -> k < spLength sp)
      $ iterate forwardX
      $ (k0, x0)

    -- Step 4
    adjustY (!k, !y)
      | k >= 0
      = (k, y)
      | otherwise
      = adjustY $ forwardY (k, y)

    -- Step 6
    doActions (!k, !y)
      | k < spLength sp
      = unsafeFlipBit vec k >> doActions (forwardY (k, y))
      | otherwise
      = pure ()

    go !k !x !y
      | x <= 0 = pure ()
      | otherwise = do
        let (k', y') = adjustY (k, y)
        doActions (k', y')
        let (k'', x') = backwardX (k', x)
        go k'' x' y'

traverseLatticePoints2
  :: SieveParams
  -> MU.MVector s Bit
  -> (Int, Int)
  -> ST s ()
traverseLatticePoints2 sp vec (x0, y0) =
  go kMax xMax y0
  where
    forwardY  (k, y) = (k + y + 15, y + 30)
    forwardX  (k, x) = (k + x +  5, x + 10)
    backwardX (k, x) = (k - x +  5, x - 10)

    -- Step 1
    k0 = (3 * x0 * x0 + y0 * y0 - spDelta sp) `quot` 60 - spLowBound sp

    -- Step 2
    (kMax, xMax)
      = backwardX
      $ head
      $ dropWhile (\(k, _) -> k < spLength sp)
      $ iterate forwardX
      $ (k0, x0)

    -- Step 4
    adjustY (!k, !y)
      | k >= 0
      = (k, y)
      | otherwise
      = adjustY $ forwardY (k, y)

    -- Step 6
    doActions (!k, !y)
      | k < spLength sp
      = unsafeFlipBit vec k >> doActions (forwardY (k, y))
      | otherwise
      = pure ()

    go !k !x !y
      | x <= 0 = pure ()
      | otherwise = do
        let (k', y') = adjustY (k, y)
        doActions (k', y')
        let (k'', x') = backwardX (k', x)
        go k'' x' y'

traverseLatticePoints3
  :: SieveParams
  -> MU.MVector s Bit
  -> (Int, Int)
  -> ST s ()
traverseLatticePoints3 sp vec (x0, y0) =
  go k0 x0 y0
  where
    forwardY  (k, y) = (k - y - 15, y + 30)
    forwardX  (k, x) = (k + x +  5, x + 10)

    -- Step 1
    k0 = (3 * x0 * x0 - y0 * y0 - spDelta sp) `quot` 60 - spLowBound sp

    -- Step 6
    doActions (!k, !x, !y)
      | k >= 0 && y < x
      = unsafeFlipBit vec k >> (let (k', y') = forwardY (k, y) in doActions (k', x, y'))
      | otherwise
      = pure ()

    go !k !x !y
      | k >= spLength sp
      , x <= y
      = pure ()
      | k >= spLength sp
      = let (k', y') = forwardY (k, y) in
        go k' x y'
      | otherwise
      = do
        doActions (k, x, y)
        let (k', x') = forwardX (k, x)
        go k' x' y

-- | Perform steps 4-6 of Algorithm 3.X.
algo3steps456
  :: SieveParams
  -> MU.MVector s Bit
  -> ST s ()
algo3steps456 sp vec =
  forM_ ps $ \p ->
    crossMultiples sp vec (p * p)
  where
    low  = 7
    high = integerSquareRoot (60 * spHighBound sp - 1)
    ps   = takeWhile (<= high) $ dropWhile (< low) $ map unPrime E.primes

-- | Cross out multiples of the first argument
-- in a given sieve.
crossMultiples
  :: SieveParams
  -> MU.MVector s Bit
  -> Int -- coprime with 60
  -> ST s ()
crossMultiples sp vec m =
  forM_ [k1, k1 + m .. spHighBound sp - 1] $
    \k -> MU.unsafeWrite vec (k - spLowBound sp) (Bit False)
  where
    -- k0 is the smallest non-negative k such that 60k+delta = 0 (mod m)
    k0 = (`quot` 60) $ (\k -> if k < 0 then k + 60 * m else k) $ fst $ fromJust $ chinese (spDelta sp, 60) (0, m)
    -- k1 = k0 (mod m), k1 >= lowBound
    (q, r) = spLowBound sp `quotRem` m
    k1 = if r < k0 then q * m + k0 else (q + 1) * m + k0
