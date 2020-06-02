{-# LANGUAGE ViewPatterns #-}

module Math.NumberTheory.Primes.Factorisation.QuadraticSieve
  ( quadraticSieve
  , gaussianElimination
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Maybe
import qualified Data.Vector as V
--import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Mutable as MV
--import qualified Data.Vector.Unboxed.Mutable as MU
--import qualified Data.IntMap as I
import qualified Data.IntSet as S
import Math.NumberTheory.Roots
import Math.NumberTheory.Primes
--import Math.NumberTheory.Primes.Sieve.SmoothSieve
--import Math.NumberTheory.Moduli.JacobiSymbol
import Math.NumberTheory.Moduli.Sqrt
import Math.NumberTheory.Utils.FromIntegral

-- Given an odd positive composite Integer n and Int parametres b and t,
-- the Quadratic Sieve attempt to decompose n into smaller factors p and q.
-- Carefully check conversion bounds. Need to make sure Integer does not get
-- too big in order to meaningfully convert to Int. t cannot be too large.
quadraticSieve :: Integer -> Int -> Int -> V.Vector S.IntSet
quadraticSieve n b t = runST $ do
    let factorBase = [nextPrime 2..precPrime b]
        -- This is not the ceiling of the square root.
        squareRoot = (integerSquareRoot n) + 1
        -- This algorithm takes the input Integer n and the Int parametre
        -- t and returns the sieving internal (x^2 - n, factorisation) as x goes
        -- from the square root of n to t.
        sievingInterval = V.generate t (\index -> (reduce index, S.empty))
        reduce i = integerToInt ((squareRoot + (intToInteger i)) ^ (2 :: Int) - n)
    sievingIntervalM <- V.thaw sievingInterval
    smoothSieveM sievingIntervalM factorBase n
    sievingIntervalF <- V.unsafeFreeze sievingIntervalM
    let indexedFactorisations = findSmoothNumbers sievingIntervalF
        -- This is to acces indexedSmoothNumbers aftwerwards. There is
        -- probably a better way to remember vector
        factorisations = snd indexedFactorisations
        leadingPrimes = V.singleton S.empty
    factorisationsM <- V.thaw factorisations
    leadingPrimesM <- V.thaw leadingPrimes
    gaussianElimination factorisationsM leadingPrimesM
    factorisationsF <- V.unsafeFreeze factorisationsM
    -- let perfectCombination = linearSolve factorisations
    pure factorisationsF

-- This algorithm takes the sievingInterval, the factorBase and the
-- modularSquareRoots and returns the smooth numbers in the interval with
-- respect to the factorBase together with their factorisations.
smoothSieveM :: MV.MVector s (Int, S.IntSet) -> [Prime Int] -> Integer -> ST s ()
smoothSieveM sievingIntervalM factorBase n = do
    let t = MV.length sievingIntervalM
        squareRoot = (integerSquareRoot n) + 1
    forM_ factorBase $ \(prime) -> do
        let modularSquareRoots = map integerToInt (sqrtsModPrime n ((fromJust . toPrimeIntegral) prime))
        forM_ modularSquareRoots $ \modularSquareRoot -> do
            let startingIndex = integerToInt ((intToInteger modularSquareRoot - squareRoot) `mod` (intToInteger . unPrime) prime)
            forM_ [startingIndex, startingIndex + (unPrime prime)..(t - 1)] $ \entry -> do
                let change (y, is) = (y `div` unPrime prime, S.insert (unPrime prime) is)
                MV.modify sievingIntervalM change entry

-- This function returns the smooth numbers together with their index in order
-- to retrieve later the value of x and x^2 - n.
-- This function should be better written.
findSmoothNumbers :: V.Vector (Int, S.IntSet) -> (V.Vector Int, V.Vector S.IntSet)
findSmoothNumbers sievingIntervalF = V.unzip smoothTuples
     where
         smoothTuples = V.map (\(a, (_, b)) -> (a, b)) indexedSmooth
         indexedSmooth = V.filter (\(_, (s, _)) -> (s == 1)) indexedSieving
         indexedSieving = V.zip (V.generate t id) sievingIntervalF
         t = V.length sievingIntervalF

-- This algorithm takes the factorisations of the smooth numbers reduced
-- modulo 2 and returns a combitation which gives a prefect square
-- There is one thing that ideally should change. At the moment, a mutable
-- IntSet is passed as an argument to keep track of what primes appear
-- in the leading diagonal of the matrix.
gaussianElimination :: MV.MVector s S.IntSet ->  MV.MVector s S.IntSet -> ST s ()
gaussianElimination factorisationsM leadingPrimesM = do
    -- This is to remember the index of the factorisation
    let s = MV.length factorisationsM
    forM_ [0..(s - 1)] $ \indexFactorisation -> do
        primeFactorisation <- MV.read factorisationsM indexFactorisation
        -- Consider case primeFactorisation is empty
        -- In this case, findMin throws an exception
        listOfLeadingPrimes <- MV.read leadingPrimesM 0
        -- What is the correct syntax to go to the next iteration of the loop
        --let leadingPrime = case (primeFactorisation `difference` listOfLeadingPrimes) of
            --S.empty -> -- break loop
            --_       -> findMin (primeFactorisation `difference` listOfLeadingPrimes)
        let leadingPrime = S.findMin (primeFactorisation `S.difference` listOfLeadingPrimes)
        MV.modify leadingPrimesM (S.insert leadingPrime) 0
        newLeadingPrimes <- MV.read leadingPrimesM 0
        -- Delete entries in same column
        MV.write factorisationsM indexFactorisation (primeFactorisation `S.intersection` newLeadingPrimes)
        let extraPrimes = primeFactorisation `S.difference` newLeadingPrimes
        forM_ [(indexFactorisation + 1)..(s - 1)] $ \column -> do
            primeColumn <- MV.read factorisationsM column
            -- This should be written afterwards
            let xor a b = (a `S.difference` b) `S.union` (b `S.difference` a)
            if (S.member leadingPrime primeColumn)
                then MV.modify factorisationsM (xor extraPrimes) column
                -- Definitely not the correct syntax
                -- What is the correct syntax to go to the next iteration of the loop
                else MV.modify factorisationsM (id) column

--
-- findFirstSquare :: U.Vector Int -> IntSet -> Integer
-- findFirstSquare factorBase perfectCombination = w
--
-- findSecondSquare :: (MV.Vector Integer, MU.Vector Int, MV.Vector (IntMap Int)) -> IntSet -> Integer
-- findSecondSquare (numbers, reducedNumbers, factorisations) perfectCombination = y
--
-- factorise :: Integer -> Integer -> Integer -> (Integer, Integer)
-- factorise n w y = (p, q)
