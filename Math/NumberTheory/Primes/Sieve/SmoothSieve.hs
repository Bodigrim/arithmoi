module Math.NumberTheory.Primes.Sieve.SmoothSieve
  ( smoothSieve
    smoothNumbers
  ) where

import Data.Vector (Vector, replicate, find, accumulate)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import Control.Monad
import Data.Maybe
-- Finds the number of primes less than or equal to a given number
import Math.NumberTheory.Primes.Counting.Impl

-- Returns a vector whose i^th component is the factorisation vector
-- of i with respect to a factor base b as i goes from 2 to n.
-- For example, given factor base [2,3,5,7], the factorisation vector
-- of 63 is [0,2,0,1]
smoothSieve :: Int -> Int -> V.Vector (V.Vector Int)
smoothSieve n b
    | n < 2 || b < 2 = V.empty
    | otherwise      = runST $ do
        -- It may be more efficient to skip this step and let b
        -- be the number of primes
        -- Discuss whether use Int or Integer
        let numberOfPrimes = fromInteger (primeCount (toInteger b))
            -- The first entry is 2, the last one is n.
            array = V.replicate (n - 1) (V.replicate numberOfPrimes 0)
            -- This index keeps track of the number of primes
            primePosition = V.singleton 0
        arrayM <- V.thaw array
        primePositionM <- V.thaw primePosition
        smoothSieveM arrayM primePositionM n b
        smoothArray <- V.unsafeFreeze arrayM
        pure smoothArray

smoothSieveM :: MV.MVector s (V.Vector Int) -> MV.MVector s Int -> Int -> Int -> ST s ()
smoothSieveM arrayM primePositionM n b = do
    let bound = minimum [n,b]
    forM_ [2..bound] $ \prime -> do
        vectorOfFactors <- MV.read arrayM (prime - 2)
        -- Check if all the entries in the vector are zero
        when (isNothing (V.find (/= 0) vectorOfFactors)) $ do
            let powers = takeWhile (< n) (iterate (* prime) prime)
            forM_ powers $ \divisor ->
                addFactor arrayM primePositionM n divisor
            MV.modify primePositionM (+1) 0

addFactor :: MV.MVector s (V.Vector Int) -> MV.MVector s Int -> Int -> Int -> ST s ()
addFactor arrayM primePositionM n divisor = do
    index <- MV.read primePositionM 0
    let addComponent v = accumulate (+) v (V.singleton (index, 1))
    forM_ [divisor, divisor*2..n] $ \multiple ->
        MV.modify arrayM addComponent (multiple - 2)
