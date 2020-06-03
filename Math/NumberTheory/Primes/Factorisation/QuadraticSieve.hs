{-# LANGUAGE ViewPatterns #-}

module Math.NumberTheory.Primes.Factorisation.QuadraticSieve
  ( quadraticSieve
  , testGauss
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Maybe
import qualified Data.List as L
import qualified Data.IntSet as S
import qualified Data.IntMap as I
import qualified Data.Vector as V
--import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Mutable as MV
--import qualified Data.Vector.Unboxed.Mutable as MU
import Math.NumberTheory.Roots
import Math.NumberTheory.Primes
import Math.NumberTheory.Moduli.Sqrt
import Math.NumberTheory.Utils.FromIntegral

-- Given an odd positive composite Integer n and Int parametres b and t,
-- the Quadratic Sieve attempt to decompose n into smaller factors p and q.
-- Carefully check conversion bounds. Need to make sure Integer does not get
-- too big in order to meaningfully convert to Int. t cannot be too large.
quadraticSieve :: Integer -> Int -> Int -> Integer
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
        -- This is to acces indexedSmooth aftwerwards. There is
        -- probably a better way to remember vector
        -- Consider writing a vector whose non-smooth number entries
        -- are empty and the smooth ones are signaled
        factorisations = snd indexedFactorisations
        s = V.length factorisations
        -- These data keep track of Gaussian elimination
        -- Could this be combined into one?
        leadingPrimes = V.singleton S.empty
        rowNumbers = V.singleton []
    factorisationsM <- V.thaw factorisations
    leadingPrimesM <- V.thaw leadingPrimes
    rowNumbersM <- V.thaw rowNumbers
    gaussianElimination factorisationsM leadingPrimesM rowNumbersM
    factorisationsF <- V.unsafeFreeze factorisationsM
    rowNumbersF <- V.unsafeFreeze rowNumbersM
    let freeVariables = V.singleton (S.fromList [0..(s - 1)])
        solutionRules = V.replicate s S.empty
    freeVariablesM <- V.thaw freeVariables
    solutionRulesM <- V.thaw solutionRules
    linearSolve freeVariablesM solutionRulesM factorisationsF (rowNumbersF V.! 0)
    freeVariablesF <- V.unsafeFreeze freeVariablesM
    solutionRulesF <- V.unsafeFreeze solutionRulesM
    let free = freeVariablesF V.! 0
        -- Pick smallest solution. Later on, all solutions will be looped through
        -- Must throw error when free is empty.
        solution = convertToSolution (S.singleton (S.findMax free)) solutionRulesF --convertToSolution free solutionRulesF
        x = findFirstSquare solution (fst indexedFactorisations) n
        y = findSecondSquare solution (snd indexedFactorisations) n
    pure (gcd (x - y) n)

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
findSmoothNumbers sievingInterval = V.unzip smoothTuples
     where
         smoothTuples = V.map (\(a, (_, b)) -> (a, b)) indexedSmooth
         indexedSmooth = V.filter (\(_, (residue, _)) -> (residue == 1)) indexedSieving
         indexedSieving = V.zip (V.generate t id) sievingInterval
         t = V.length sievingInterval

-- This algorithm takes the factorisations of the smooth numbers reduced
-- modulo 2 and rperforms row reduction.
-- There is one thing that ideally should change. At the moment, two extra
-- arguments are passed into the functions in order to keep track of
-- what primes appear in the leading diagonal of the matrix. The first one
-- is an IntSet, the second one is a list. The second one contains more
-- information, however it may be expensive to continously convert
-- between one and the other in the course of the algorithm.
gaussianElimination :: MV.MVector s S.IntSet ->  MV.MVector s S.IntSet -> MV.MVector s [Int] -> ST s ()
gaussianElimination factorisationsM leadingPrimesM rowNumbersM = do
    -- This is to remember the index of the factorisation
    let s = MV.length factorisationsM
    forM_ [0..(s - 1)] $ \indexFactorisation -> do
        primeFactorisation <- MV.read factorisationsM indexFactorisation
        -- Consider case primeFactorisation is empty
        -- In this case, findMin throws an exception
        listOfLeadingPrimes <- MV.read leadingPrimesM 0
        when (primeFactorisation `S.difference` listOfLeadingPrimes /= S.empty) $ do
            let leadingPrime = S.findMin (primeFactorisation `S.difference` listOfLeadingPrimes)
            MV.modify leadingPrimesM (S.insert leadingPrime) 0
            MV.modify rowNumbersM (leadingPrime :) 0
            newLeadingPrimes <- MV.read leadingPrimesM 0
            -- Delete entries in same column
            MV.write factorisationsM indexFactorisation (primeFactorisation `S.intersection` newLeadingPrimes)
            let extraPrimes = primeFactorisation `S.difference` newLeadingPrimes
            forM_ [(indexFactorisation + 1)..(s - 1)] $ \column -> do
                primeColumn <- MV.read factorisationsM column
                when (leadingPrime `S.member` primeColumn) $ do
                    let xor a b = (a `S.difference` b) `S.union` (b `S.difference` a)
                    MV.modify factorisationsM (xor extraPrimes) column

-- This function takes a matrix in row reduced form and finds its kernel.
-- Note output in solutionRules does not consist only of free variables.
linearSolve :: MV.MVector s S.IntSet -> MV.MVector s S.IntSet -> V.Vector S.IntSet -> [Int] -> ST s ()
linearSolve freeVariablesM solutionRulesM rowReducedMatrix rowNumbers = do
    let s = V.length rowReducedMatrix
    forM_ rowNumbers $ \rowPrime -> do
        freeVariables <- MV.read freeVariablesM 0
        -- Risky, but by construction this list is guaranteed to be non empty
        -- Rather than findIndices, consider using functions of IntSet
        let (x:xs) = L.findIndices (\t -> (rowPrime `S.member` (rowReducedMatrix V.! t))) [0..(s - 1)]
            -- This is the index of the previous pivot
            upper = fromMaybe s (L.findIndex (\t -> (not (t `S.member` freeVariables))) [0..(s - 1)])
        -- Add free variables to solutionRules
        forM_ [(x + 1)..(upper - 1)] $ \freeIndex ->
            MV.modify solutionRulesM (freeIndex `S.insert`) freeIndex
        -- Update free variables by removing current pivot
        MV.modify freeVariablesM (x `S.delete`) 0
        -- Update solution rule for current pivot making sure that it only
        -- consists of free variables
        -- Only interested in indices given by xs but don't know how to access
        -- only this information
        solutionRule <- V.unsafeFreeze solutionRulesM
        let freeRule = S.fold replace S.empty (S.fromAscList xs)
            replace t previousRules = (solutionRule V.! t) `S.union` previousRules
        -- This entry was empty before
        MV.write solutionRulesM x freeRule

-- This function takes a subset of the freeVaraibles and returns the
-- corresponding solution. No checks
convertToSolution :: S.IntSet -> V.Vector S.IntSet -> S.IntSet
convertToSolution subset solutionRules = V.foldl convertToIntSet S.empty indexedCurrentSolution
    where
        convertToIntSet previous (index, is) = case (odd (S.size is)) of
            True  -> index `S.insert` previous
            False -> previous
        indexedCurrentSolution = V.zip (V.generate s id) currentSolution
        currentSolution = V.map (subset `S.intersection`) solutionRules
        s = V.length solutionRules

testGauss :: [[Int]] -> ([Int], [[Int]])
testGauss listMatrix = runST $ do
    let s = length listMatrix
        matrix = V.fromList (map S.fromList listMatrix)
        leadingEntries = V.singleton S.empty
        rowNumbers = V.singleton []
        freeVariables =  V.singleton (S.fromList [0..(s - 1)])
        solutionRules = V.replicate s S.empty
    matrixM <- V.thaw matrix
    leadingEntriesM <- V.thaw leadingEntries
    rowNumbersM <- V.thaw rowNumbers
    gaussianElimination matrixM leadingEntriesM rowNumbersM
    matrixF <- V.unsafeFreeze matrixM
    rowNumbersF <- V.unsafeFreeze rowNumbersM
    freeVariablesM <- V.thaw freeVariables
    solutionRulesM <- V.thaw solutionRules
    linearSolve freeVariablesM solutionRulesM matrixF (rowNumbersF V.! 0)
    freeVariablesF <- V.unsafeFreeze freeVariablesM
    solutionRulesF <- V.unsafeFreeze solutionRulesM
    pure (S.toList (freeVariablesF V.! 0), map S.toList (V.toList solutionRulesF))

findFirstSquare :: S.IntSet -> V.Vector Int -> Integer -> Integer
findFirstSquare solution indices n = S.fold construct 1 solution
    where
        construct current previous = (((intToInteger (indices V.! current)) + squareRoot) * previous) `mod` n
        squareRoot = (integerSquareRoot n) + 1

findSecondSquare :: S.IntSet -> V.Vector S.IntSet -> Integer -> Integer
findSecondSquare solution factorisations n = I.foldrWithKey computeRoot 1 countSquares
    where
        computeRoot key power previous = ((intToInteger key) ^ (power `div` 2 :: Int) * previous) `mod` n
        countSquares = V.foldl countPowers I.empty squares
        countPowers previous factorisation = S.fold (\key im -> I.insertWith (+) key 1 im) previous factorisation
        squares = V.map (\(_, factorisation) -> factorisation) indexedSquares
        indexedSquares = V.filter (\(index, _) -> (index `S.member` solution)) indexedFactorisations
        indexedFactorisations = V.zip (V.generate s id) factorisations
        s = V.length factorisations
