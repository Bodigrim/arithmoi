module Math.NumberTheory.Primes.Sieve.SmoothSieve
  ( smoothSieve
  ) where

import Data.Vector (Vector, replicate)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import Control.Monad

-- Returns a vector containing the smooth numbers together with
-- their factorisation modulo two. If a number is not smooth,
-- the corresponding vector is empty.
smoothSieve :: Int -> Int -> V.Vector [Int]
smoothSieve n b
    | n < 2 || b < 2 = V.empty
    | otherwise      = runST $ do
        -- The first entry is 2, the last one is n.
        let array = V.replicate (n - 1) []
        arrayM <- V.thaw array
        smoothSieveM arrayM n b
        smoothArray <- V.unsafeFreeze arrayM
        pure smoothArray

smoothSieveM :: MV.MVector s [Int] -> Int -> Int -> ST s ()
smoothSieveM arrayM n b = do
    let bound = minimum [n,b]
    forM_ [2..bound] $ \prime -> do
        factors <- MV.read arrayM (prime - 2)
        when (null factors) $ do
            let divisor = prime
            primePowers arrayM n prime divisor

primePowers :: MV.MVector s [Int] -> Int -> Int -> Int -> ST s ()
primePowers arrayM n prime divisor =
    when (divisor <= n) $ do
        addFactor arrayM n prime divisor
        primePowers arrayM n prime (prime*divisor)

addFactor :: MV.MVector s [Int] -> Int -> Int -> Int -> ST s ()
addFactor arrayM n prime divisor =
    forM_ [divisor, divisor*2..n] $ \multiple ->
        MV.modify arrayM (prime :) (multiple - 2)
