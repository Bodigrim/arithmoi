{-# LANGUAGE ViewPatterns #-}

module Math.NumberTheory.Primes.Factorisation.QuadraticSieve
  ( quadraticSieve
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
import Math.NumberTheory.Primes.IntSet as SP
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
        --I.IntMap (Int)
        sievingInterval = V.generate t (\index -> (reduce index, SP.empty))
        reduce i = integerToInt ((squareRoot + (intToInteger i)) ^ (2 :: Int) - n)
    sievingIntervalM <- V.thaw sievingInterval
    smoothSieveM sievingIntervalM factorBase n
    sievingIntervalF <- V.unsafeFreeze sievingIntervalM
    let indexedFactorisations = findSmoothNumbers sievingIntervalF
        -- This is to acces indexedSmooth aftwerwards. There is
        -- probably a better way to remember vector
        -- Consider writing a vector whose non-smooth number entries
        -- are empty and the smooth ones are signaled
        factorisations = fmap snd indexedFactorisations
    factorisationsM <- V.thaw factorisations
    pivots <- gaussianEliminationM factorisationsM
    factorisationsF <- V.unsafeFreeze factorisationsM
    let solutions = linearSolve factorisationsF pivots
        -- -- Pick smallest solution. Later on, all solutions will be looped through
        -- -- Must throw error when free is empty.
        -- solution = convertToSolution (S.singleton (S.findMax free)) solutionRulesF --convertToSolution free solutionRulesF
        -- x = findFirstSquare solution (fmap fst indexedFactorisations) n
        -- y = findSecondSquare solution (fmap snd indexedFactorisations) n
    pure solutions --(gcd (x - y) n)

-- This algorithm takes the sievingInterval, the factorBase and the
-- modularSquareRoots and returns the smooth numbers in the interval with
-- respect to the factorBase together with their factorisations.
smoothSieveM :: MV.MVector s (Int, SP.PrimeIntSet) -> [Prime Int] -> Integer -> ST s ()
smoothSieveM sievingIntervalM factorBase n = do
    let t = MV.length sievingIntervalM
        squareRoot = (integerSquareRoot n) + 1
    forM_ factorBase $ \prime -> do
        let modularSquareRoots = map integerToInt (sqrtsModPrime n ((fromJust . toPrimeIntegral) prime))
        forM_ modularSquareRoots $ \modularSquareRoot -> do
            let startingIndex = integerToInt ((intToInteger modularSquareRoot - squareRoot) `mod` (intToInteger . unPrime) prime)
            forM_ [startingIndex, startingIndex + (unPrime prime)..(t - 1)] $ \entry -> do
                let change (y, is) = (y `div` unPrime prime, SP.insert prime is)
                MV.modify sievingIntervalM change entry

-- This function returns the smooth numbers together with their index in order
-- to retrieve later the value of x and x^2 - n.
-- This function should be better written.
-- Change it to V.Vector (Int, S.IntSet)
findSmoothNumbers :: V.Vector (Int, SP.PrimeIntSet) -> V.Vector (Int, SP.PrimeIntSet)
findSmoothNumbers = V.imapMaybe selectSmooth
     where
         selectSmooth index (residue, factorisation)
          | residue == 1 = Just (index, factorisation)
          | otherwise    = Nothing

-- This algorithm takes the factorisations of the smooth numbers reduced
-- modulo 2 and rperforms row reduction.
-- There is one thing that ideally should change. At the moment, two extra
-- arguments are passed into the functions in order to keep track of
-- what primes appear in the leading diagonal of the matrix. The first one
-- is an IntSet, the second one is a list. The second one contains more
-- information, however it may be expensive to continously convert
-- between one and the other in the course of the algorithm.
-- Make one mutable vector out of rownumbersM and factorisationM
gaussianEliminationM :: MV.MVector s SP.PrimeIntSet -> ST s (I.IntMap (Prime Int))
gaussianEliminationM factorisationsM = do
    let s = MV.length factorisationsM
    -- Pivots remembers the matrix entry where the pivots are
    let go pivots column
            | column >= s = pure pivots
            | otherwise = do
                primeFactorisation <- MV.read factorisationsM column
                let setOfPivotRows = (SP.fromList . I.elems) pivots
                    difference = primeFactorisation SP.\\ unPrimeIntSet setOfPivotRows
                -- If column is linearly dependent go to the next one
                if (difference == SP.empty)
                    then go pivots (column + 1)
                    else do
                        -- otherwise find the new pivot
                        let (rowPivot, nonZeroRows) = SP.deleteFindMin difference
                        -- Delete entries in same column
                        MV.write factorisationsM column (primeFactorisation SP.\\ unPrimeIntSet nonZeroRows)
                        -- Delete entries in further columns
                        forM_ [(column + 1)..(s - 1)] $ \index -> do
                            nextFactorisation <- MV.read factorisationsM index
                            when (rowPivot `SP.member` nextFactorisation) $ do
                                let xor a b = (a SP.\\ unPrimeIntSet b) <> (b SP.\\ unPrimeIntSet a)
                                MV.modify factorisationsM (xor nonZeroRows) index
                        -- Go to the next column remembering pivot
                        go (I.insert column rowPivot pivots) (column + 1)

    go I.empty 0

linearSolve :: V.Vector SP.PrimeIntSet -> I.IntMap (Prime Int) -> V.Vector S.IntSet
linearSolve rowReducedMatrix im = Prelude.foldr accumulate (V.replicate s S.empty) [0..(s -1)]
    where
        s = V.length rowReducedMatrix
        accumulate column oldSolutions = case I.lookup column im of
            -- When there is a free variable in the given column
            Nothing        -> oldSolutions V.// [(column, S.singleton column)]
            -- When there is a pivot in the given column
            Just rowNumber -> oldSolutions V.// [(column, combination)]
                where
                    combination = Prelude.foldr combine S.empty [(column + 1)..(s - 1)]
                    xor a b = (a S.\\ b) <> (b S.\\ a)
                    combine index oldCombination
                        -- The column index does have a 1 at rowNumber
                        | (rowNumber `SP.member` (rowReducedMatrix V.! index)) = xor oldCombination (oldSolutions V.! index)
                        -- The column index does not have a 1 at rowNumber
                        | otherwise                                            = oldCombination


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

-- testGauss :: [[Int]] -> ([Int], [[Int]])
-- testGauss listMatrix = runST $ do
--     let s = length listMatrix
--         matrix = V.fromList (map S.fromList listMatrix)
--         leadingEntries = V.singleton S.empty
--         rowNumbers = V.singleton []
--         freeVariables =  V.singleton (S.fromList [0..(s - 1)])
--         solutionRules = V.replicate s S.empty
--     matrixM <- V.thaw matrix
--     leadingEntriesM <- V.thaw leadingEntries
--     rowNumbersM <- V.thaw rowNumbers
--     gaussianElimination matrixM leadingEntriesM rowNumbersM
--     matrixF <- V.unsafeFreeze matrixM
--     rowNumbersF <- V.unsafeFreeze rowNumbersM
--     freeVariablesM <- V.thaw freeVariables
--     solutionRulesM <- V.thaw solutionRules
--     linearSolve freeVariablesM solutionRulesM matrixF (rowNumbersF V.! 0)
--     freeVariablesF <- V.unsafeFreeze freeVariablesM
--     solutionRulesF <- V.unsafeFreeze solutionRulesM
--     pure (S.toList (freeVariablesF V.! 0), map S.toList (V.toList solutionRulesF))

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
