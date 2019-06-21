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
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

import Math.NumberTheory.Roots
import qualified Math.NumberTheory.Primes.Sieve.Eratosthenes as E
import Math.NumberTheory.Primes.Types
import Math.NumberTheory.Utils

atkinPrimeList :: PrimeSieve -> [Int]
atkinPrimeList (PrimeSieve low len segments)
  | len60 == 0 = []
  | otherwise = takeWhile (< high) $ dropWhile (< low) $ 2 : 3 : 5 : merge l0 l1
  where
    list00 = map (\k -> 60 * (low60 + k) + fromWheel30  0) (listBits $ segments V.!  0)
    list01 = map (\k -> 60 * (low60 + k) + fromWheel30  1) (listBits $ segments V.!  1)
    list02 = map (\k -> 60 * (low60 + k) + fromWheel30  2) (listBits $ segments V.!  2)
    list03 = map (\k -> 60 * (low60 + k) + fromWheel30  3) (listBits $ segments V.!  3)
    list04 = map (\k -> 60 * (low60 + k) + fromWheel30  4) (listBits $ segments V.!  4)
    list05 = map (\k -> 60 * (low60 + k) + fromWheel30  5) (listBits $ segments V.!  5)
    list06 = map (\k -> 60 * (low60 + k) + fromWheel30  6) (listBits $ segments V.!  6)
    list07 = map (\k -> 60 * (low60 + k) + fromWheel30  7) (listBits $ segments V.!  7)
    list08 = map (\k -> 60 * (low60 + k) + fromWheel30  8) (listBits $ segments V.!  8)
    list09 = map (\k -> 60 * (low60 + k) + fromWheel30  9) (listBits $ segments V.!  9)
    list10 = map (\k -> 60 * (low60 + k) + fromWheel30 10) (listBits $ segments V.! 10)
    list11 = map (\k -> 60 * (low60 + k) + fromWheel30 11) (listBits $ segments V.! 11)
    list12 = map (\k -> 60 * (low60 + k) + fromWheel30 12) (listBits $ segments V.! 12)
    list13 = map (\k -> 60 * (low60 + k) + fromWheel30 13) (listBits $ segments V.! 13)
    list14 = map (\k -> 60 * (low60 + k) + fromWheel30 14) (listBits $ segments V.! 14)
    list15 = map (\k -> 60 * (low60 + k) + fromWheel30 15) (listBits $ segments V.! 15)

    lst0 = merge list00 list01
    lst1 = merge list02 list03
    lst2 = merge list04 list05
    lst3 = merge list06 list07
    lst4 = merge list08 list09
    lst5 = merge list10 list11
    lst6 = merge list12 list13
    lst7 = merge list14 list15

    ls0 = merge lst0 lst1
    ls1 = merge lst2 lst3
    ls2 = merge lst4 lst5
    ls3 = merge lst6 lst7

    l0 = merge ls0 ls1
    l1 = merge ls2 ls3

    low60 = low `quot` 60
    len60 = (low + len + 59) `quot` 60 - low60
    high = low + len

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xs@(x:xs') ys@(y:ys')
  | x < y     = x : merge xs' ys
  | otherwise = y : merge xs ys'

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
    k0 = solveCongruence (spDelta sp) m
    -- k1 = k0 (mod m), k1 >= lowBound
    (q, r) = spLowBound sp `quotRem` m
    k1 = if r < k0 then q * m + k0 else (q + 1) * m + k0

-- Find the smallest k such that 60k+delta = 0 (mod m)
-- Should be equal to
-- (`quot` 60) $ fromJust $ chineseCoprime (delta, 60) (0, m)
solveCongruence :: Int -> Int -> Int
solveCongruence delta m = (- recip60 * delta) `mod` m
  where
    recip60 = fromInteger $ fromJust $ recipMod 60 (toInteger m)
