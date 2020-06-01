module Math.NumberTheory.Primes.Factorisation.QuadraticSieve
  ( quadraticSieve
  ) where

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector as V
--import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Mutable as MV
--import qualified Data.Vector.Unboxed.Mutable as MU
--import qualified Data.IntMap as I
import qualified Data.IntSet as S
import Math.NumberTheory.Roots
import Math.NumberTheory.Primes
--import Math.NumberTheory.Primes.Sieve.SmoothSieve
import Math.NumberTheory.Moduli.JacobiSymbol
import Math.NumberTheory.Moduli.Sqrt
import Math.NumberTheory.Utils.FromIntegral

-- Given an odd positive composite Integer n and Int parametres b and t,
-- the Quadratic Sieve attempt to decompose n into smaller factors p and q.
-- Carefully check conversion bounds. Need to make sure Integer does not get
-- too big in order to meaningfully convert to Int. t cannot be too large.
quadraticSieve :: Integer -> Integer -> Int -> (V.Vector (Integer, S.IntSet), V.Vector [Integer])
quadraticSieve n b t = runST $ do
    let factorBase = findFactorBase n b
        modularSquareRoots = V.map (\x -> sqrtsModPrime n x) factorBase
        -- This is not the ceiling of the square root.
        squareRoot = (integerSquareRoot n) + 1
        -- This algorithm takes the input Integer n and the Int parametre
        -- t and returns the sieving internal (x^2 - n, factorisation) as x goes
        -- from the square root of n to t.
        sievingInterval = V.generate t (\index -> (reduce index, S.empty))
        reduce i = (squareRoot + (toInteger i)) ^ (2 :: Int) - n
    sievingIntervalM <- V.thaw sievingInterval
    smoothSieveM sievingIntervalM factorBase modularSquareRoots squareRoot
    sievingIntervalF <- V.unsafeFreeze sievingIntervalM
    pure (sievingIntervalF, modularSquareRoots)

-- This algorithm takes n and b and returns the factor base, an unboxed
-- vector of Int. This comprises all the odd primes less than b whose
-- quadratic residue with n is One.
-- Could this algorithm be faster? Do we need to check each prime one
-- by one or can we run a sieveBlock?
findFactorBase :: Integer -> Integer -> V.Vector (Prime Integer)
findFactorBase n b = factorBase
    where
        -- in jacobi, both arguments have to be of Integer type.
        -- Is there a faster way not to check 2?
        factorBase = V.filter (\x -> (unPrime x == 2) || (jacobi n (unPrime x) == One)) vectorOfPrimes
        -- Remove 2 from list of Primes
        vectorOfPrimes = V.fromList listOfPrimes
        listOfPrimes = [nextPrime 2..precPrime b]

-- This algorithm takes the sievingInterval, the factorBase and the
-- modularSquareRoots and returns the smooth numbers in the interval with
-- respect to the factorBase ogether with their factorisations.
smoothSieveM :: MV.MVector s (Integer, S.IntSet) -> V.Vector (Prime Integer) -> V.Vector [Integer] -> Integer -> ST s ()
smoothSieveM sievingIntervalM factorBase modularSquareRoots squareRoot = do
    -- Is it better to pass t as an argument rather than computing it again?
    let t = MV.length sievingIntervalM
        s = V.length factorBase
    -- Loops through factorBase remembering index
    forM_ [0..(s - 1)] $ \indexOfPrime -> do
        let primeInteger = unPrime (factorBase V.! indexOfPrime)
            -- Is this necessary. Can just convert later directly with squareRoot
            residue = squareRoot `mod` primeInteger
            prime = integerToInt primeInteger
        forM_ (modularSquareRoots V.! indexOfPrime) $ \modularSquareRoot -> do
            -- What is the best way for reducing modulo the prime?
            let startingIndex = integerToInt (reduceModuloPrime (modularSquareRoot - residue) primeInteger)
                -- This function should be implemented somewhere else in arithmoi
                reduceModuloPrime x p = if (x >= 0) then (x `mod` p) else (p + (x `mod` p))
            forM_ [startingIndex, startingIndex + prime..(t - 1)] $ \entry -> do
                -- Better way to write function on tuple
                -- See if it's better to insert Prime or indexOfPrime
                let change tuple = ((fst tuple) `div` primeInteger, S.insert indexOfPrime (snd tuple))
                MV.modify sievingIntervalM change entry

-- convertToBinary :: V.Vector (IntMap Int) -> V.Vector IntSet
-- convertToBinary facorisations = binaryFactorisations
--
-- -- This algorithm takes the factorisations of the smooth numbers reduced
-- -- modulo 2 and returns a combitation which gives a prefect square
-- linearSolve :: V.Vector IntSet -> IntSet
-- linearSolve binaryFactorisations = pefectCombination
--
-- findFirstSquare :: U.Vector Int -> IntSet -> Integer
-- findFirstSquare factorBase perfectCombination = w
--
-- findSecondSquare :: (MV.Vector Integer, MU.Vector Int, MV.Vector (IntMap Int)) -> IntSet -> Integer
-- findSecondSquare (numbers, reducedNumbers, factorisations) perfectCombination = y
--
-- factorise :: Integer -> Integer -> Integer -> (Integer, Integer)
-- factorise n w y = (p, q)
