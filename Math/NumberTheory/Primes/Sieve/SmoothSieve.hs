module Math.NumberTheory.Primes.Sieve.SmoothSieve
  ( smoothData
  , smoothNumbers
  ) where

import Control.Monad
import Control.Monad.ST
import Data.IntSet as IS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as UMV
-- To convert between boxed and unboxed
import Data.Vector.Generic (convert)

-- Finds the number of primes less than or equal to a given number
--import Math.NumberTheory.Primes.Counting.Impl

smoothData :: Int -> Int -> (U.Vector Int, V.Vector IS.IntSet)
smoothData n b = (smoothIndices, smoothFactorisations)
    where
        -- Need to convert from unboxed to boxed
        smoothFactorisations = V.map (factorisationsArray V.!) (convert indicesArray)
        indicesArray = U.findIndices (== 1) numbersArray
        smoothIndices = U.map (+2) indicesArray
        (numbersArray, factorisationsArray) = smoothSieve n b

smoothNumbers :: Int -> Int -> U.Vector Int
smoothNumbers n b = fst (smoothData n b)

-- Input an integer n and b and it returns a tuple of vectors. The first
-- element of the tuple is the vector of the numbers 2 to n divided by
-- all the primes in the factor base (primes less than b). The second
-- element is a vector consisting of their factorisations modulo 2.
-- In details, the i^th component of this vector is the factorisation
-- vector of the i modulo 2 with respect to the factor base as
-- i goes from 2 to n.
-- For example, given factor base [2,3,5,7], the factorisation vector
-- of 63 is [0,0,0,1] = [True, True, True, False]
-- At the moment, using Int rather than Integers
smoothSieve :: Int -> Int -> (U.Vector Int, V.Vector IS.IntSet)
smoothSieve n b
    | n < 2 || b < 2 = (U.empty, V.empty)
    | otherwise      = runST $ do
        -- It may be more efficient to skip this step and let b
        -- be the number of primes
        -- Discuss whether use Int or Integer
        let --numberOfPrimes = fromInteger (primeCount (toInteger b))
            -- Array keeping track of divisions
            numbers = U.generate (n - 1) (\x -> x + 2)
            factorisations = V.replicate (n - 1) IS.empty
            -- This index keeps track of the number of primes
            -- Is there more elegant way to keep track of the
            -- number of primes?
            primePosition = U.singleton 0
        numbersM <- U.thaw numbers
        factorisationsM <- V.thaw factorisations
        primePositionM <- U.thaw primePosition
        smoothSieveM numbersM factorisationsM primePositionM n b
        numbersArray <- U.unsafeFreeze numbersM
        factorisationsArray <- V.unsafeFreeze factorisationsM
        pure (numbersArray, factorisationsArray)

smoothSieveM :: UMV.MVector s Int -> MV.MVector s IS.IntSet -> UMV.MVector s Int -> Int -> Int -> ST s ()
smoothSieveM numbersM factorisationsM primePositionM n b = do
    let bound = minimum [n,b]
    forM_ [2..bound] $ \index -> do
        prime <- UMV.read numbersM (index - 2)
        -- Check if number is equal to its index
        -- i.e. if the number is a prime
        when (prime == index) $ do
            let powers = takeWhile (< n) (iterate (* prime) prime)
            forM_ powers $ \divisor ->
                addFactor numbersM factorisationsM primePositionM n prime divisor
            UMV.modify primePositionM (+1) 0

addFactor :: UMV.MVector s Int -> MV.MVector s IS.IntSet -> UMV.MVector s Int -> Int -> Int -> Int -> ST s ()
addFactor numbersM factorisationsM primePositionM n prime divisor = do
    -- This is the position storing information about prime
    index <- UMV.read primePositionM 0
    let addComponent set = if (IS.member index set) then (IS.delete index set) else (IS.insert index set)
    forM_ [divisor, divisor*2..n] $ \multiple -> do
        UMV.modify numbersM (`div` prime) (multiple - 2)
        MV.modify factorisationsM addComponent (multiple - 2)
