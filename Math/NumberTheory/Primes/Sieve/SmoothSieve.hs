module Math.NumberTheory.Primes.Sieve.SmoothSieve
  ( smoothData
  , smoothNumbers
  ) where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import Control.Monad
-- Finds the number of primes less than or equal to a given number
import Math.NumberTheory.Primes.Counting.Impl

smoothData :: Int -> Int -> (V.Vector Int, V.Vector (V.Vector Bool))
smoothData n b = (smoothIndices, smoothFactorisations)
    where
        smoothFactorisations = V.map (factorisationsArray V.!) indicesArray
        indicesArray = V.findIndices (== 1) numbersArray
        smoothIndices = V.map (+2) indicesArray
        (numbersArray, factorisationsArray) = smoothSieve n b

smoothNumbers :: Int -> Int -> V.Vector Int
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
smoothSieve :: Int -> Int -> (V.Vector Int, V.Vector (V.Vector Bool))
smoothSieve n b
    | n < 2 || b < 2 = (V.empty, V.empty)
    | otherwise      = runST $ do
        -- It may be more efficient to skip this step and let b
        -- be the number of primes
        -- Discuss whether use Int or Integer
        let numberOfPrimes = fromInteger (primeCount (toInteger b))
            -- Array keeping track of divisions
            numbers = V.generate (n - 1) (\x -> x + 2)
            factorisations = V.replicate (n - 1) (V.replicate numberOfPrimes True)
            -- This index keeps track of the number of primes
            -- Is there more elegant way to keep track of the
            -- number of primes?
            primePosition = V.singleton 0
        numbersM <- V.thaw numbers
        factorisationsM <- V.thaw factorisations
        primePositionM <- V.thaw primePosition
        smoothSieveM numbersM factorisationsM primePositionM n b
        numbersArray <- V.unsafeFreeze numbersM
        factorisationsArray <- V.unsafeFreeze factorisationsM
        pure (numbersArray, factorisationsArray)

smoothSieveM :: MV.MVector s Int -> MV.MVector s (V.Vector Bool) -> MV.MVector s Int -> Int -> Int -> ST s ()
smoothSieveM numbersM factorisationsM primePositionM n b = do
    let bound = minimum [n,b]
    forM_ [2..bound] $ \index -> do
        prime <- MV.read numbersM (index - 2)
        -- Check if number is equal to its index
        -- i.e. if the number is a prime
        when (prime == index) $ do
            let powers = takeWhile (< n) (iterate (* prime) prime)
            forM_ powers $ \divisor ->
                addFactor numbersM factorisationsM primePositionM n prime divisor
            MV.modify primePositionM (+1) 0

addFactor :: MV.MVector s Int -> MV.MVector s (V.Vector Bool) -> MV.MVector s Int -> Int -> Int -> Int -> ST s ()
addFactor numbersM factorisationsM primePositionM n prime divisor = do
    -- This is the position storing information about prime
    index <- MV.read primePositionM 0
    -- There must be a better way to change the entry
    let addComponent v = V.update v (V.singleton (index, not (v V.! index)))
    forM_ [divisor, divisor*2..n] $ \multiple -> do
        MV.modify numbersM (`div` prime) (multiple - 2)
        MV.modify factorisationsM addComponent (multiple - 2)
