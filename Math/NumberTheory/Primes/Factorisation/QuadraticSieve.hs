{-# LANGUAGE ViewPatterns #-}

module Math.NumberTheory.Primes.Factorisation.QuadraticSieve
  ( quadraticSieve
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
    factorisationsM <- V.thaw factorisations
    --gaussianElimination factorisationsM
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
-- gaussianElimination :: MV.MVector s S.IntSet -> ST s ()
-- gaussianElimination factorisations = do
--     -- This is to remember the index of the factorisation
--     let s = MV.length factorisations
--     forM_ [0..(s - 1)] $ \indexFactorisation -> do
--         primeFactorisation <- MV.read factorisations indexFactorisation
--         -- Consider case primeFactorisation is empty
--         let () = S.partition
--         -- Delete entries in same column
--         MV.write factorisations indexFactorisation (S.singleton firstPrime)
--         -- Delete entries in further columns
--         forM_ [indexFactorisation..(s - 1)] $ \column -> do


--
-- findFirstSquare :: U.Vector Int -> IntSet -> Integer
-- findFirstSquare factorBase perfectCombination = w
--
-- findSecondSquare :: (MV.Vector Integer, MU.Vector Int, MV.Vector (IntMap Int)) -> IntSet -> Integer
-- findSecondSquare (numbers, reducedNumbers, factorisations) perfectCombination = y
--
-- factorise :: Integer -> Integer -> Integer -> (Integer, Integer)
-- factorise n w y = (p, q)
