{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module Math.NumberTheory.Primes.Factorisation.QuadraticSieve
  ( quadraticSieve
  , gaussianEliminationM
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Maybe
--import Data.Semigroup
--import Data.Foldable
import qualified Data.IntSet as S
import qualified Data.IntMap as I
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Math.NumberTheory.Roots
import Math.NumberTheory.Primes
import qualified Math.NumberTheory.Primes.IntSet as SP
import Math.NumberTheory.Moduli.Sqrt
import Math.NumberTheory.Utils.FromIntegral
import Debug.Trace

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
        --I.IntMap (Int)
        sievingInterval = V.generate t (\index -> (reduce index, SP.empty))
        reduce i = integerToInt ((squareRoot + (intToInteger i)) ^ (2 :: Int) - n)
    sievingIntervalM <- V.thaw sievingInterval
    smoothSieveM sievingIntervalM factorBase n
    sievingIntervalF <- V.unsafeFreeze sievingIntervalM
    let indexedFactorisations = findSmoothNumbers sievingIntervalF
    indexedFactorisationsM <- V.thaw indexedFactorisations
    gaussianEliminationM indexedFactorisationsM
    solutionRules <- V.unsafeFreeze indexedFactorisationsM
    -- 1 is the parametre indexing all solution to the matrix. It is
    -- recursively increased to go through all the solutions.
    let r = length (V.findIndices (/= S.empty) (fmap fst solutionRules))
    pure (trace ("Dimension of kernel: " ++ show r ++ "\nFactoring") $ findFactor n 1 (fmap fst solutionRules) indexedFactorisations)

-- This algorithm takes the sievingInterval, the factorBase and the
-- modularSquareRoots and returns the smooth numbers in the interval with
-- respect to the factorBase together with their factorisations.
smoothSieveM :: MV.MVector s (Int, SP.PrimeIntSet) -> [Prime Int] -> Integer -> ST s ()
smoothSieveM sievingIntervalM factorBase n = trace ("Sieving") $ do
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
findSmoothNumbers :: V.Vector (Int, SP.PrimeIntSet) -> V.Vector (S.IntSet, SP.PrimeIntSet)
findSmoothNumbers = V.imapMaybe selectSmooth
     where
         selectSmooth index (residue, factorisation)
          | residue == 1 = Just (S.singleton index, factorisation)
          | otherwise    = Nothing

-- gaussianElimination :: MV.MVector s (S.IntSet, SP.PrimeIntSet) -> ST s ()
-- gaussianElimination indexedFactorisationsM = do
--     indexedFactorisations <- V.unsafeFreeze indexedFactorisationsM
--     let s = V.length indexedFactorisations
--         findMinimumSet :: V.Vector (S.IntSet, PrimeIntSet) -> Maybe (Key, (S.IntSet, PrimeIntSet))
--         findMinimumSet = fmap getMin . foldMap (\tis@(_, is) -> Min . (, tis) . fst <$> SP.minView is)
--
--         maybeMin = findMinimumSet indexedFactorisations
--
--     when (isJust maybeMin) $ do
--         let Just (smallestPrime, smallestColumn) = maybeMin
--         -- Can I avoid using index?
--         forM_ [0..(s - 1)] $ \columnIndex -> do
--             column <- MV.read indexedFactorisationsM columnIndex
--             let (_, factorisation) = trace (show columnIndex) $ column
--             when (smallestPrime `SP.member` factorisation) $ do
--                 let xor (a, u) (b, v) = ((a S.\\ b) <> (b S.\\ a), (u SP.\\ SP.unPrimeIntSet v) <> (v SP.\\ SP.unPrimeIntSet u))
--                 trace (show smallestPrime) $ MV.modify indexedFactorisationsM (xor smallestColumn) columnIndex
--
--         gaussianElimination indexedFactorisationsM

gaussianEliminationM :: MV.MVector s (S.IntSet, SP.PrimeIntSet) -> ST s ()
gaussianEliminationM indexedFactorisationsM = trace ("Reducing") $ do
    let s = MV.length indexedFactorisationsM
    -- Pivots remembers the matrix entry where the pivots are
    forM_ [0..(s - 1)] $ \column -> do
        x@(_, primeFactorisation) <- MV.read indexedFactorisationsM column
        -- If column is linearly dependent to the previous ones go to the next one
        when (primeFactorisation /= SP.empty) $ do
            -- otherwise find the new pivot
            let pivot = SP.findMin primeFactorisation
            -- Delete entries in columns
            forM_ [column..(s - 1)] $ \index -> do
                (_, nextFactorisation) <- MV.read indexedFactorisationsM index
                when (pivot `SP.member` nextFactorisation) $ do
                    let xor (a, u) (b, v) = ((a S.\\ b) <> (b S.\\ a), (u SP.\\ SP.unPrimeIntSet v) <> (v SP.\\ SP.unPrimeIntSet u))
                    MV.modify indexedFactorisationsM (xor x) index

convertToSolution :: I.IntMap Bool -> V.Vector S.IntSet -> S.IntSet
convertToSolution parametres solutionRules = V.ifoldr add S.empty solutionRules
    where
        add index rule previous = case (I.lookup index parametres) of
            Just True  -> previous
            Just False -> xor rule previous
            Nothing    -> previous
            where
                xor a b = (a S.\\ b) <> (b S.\\ a)

generateParametres :: V.Vector S.IntSet -> Integer -> I.IntMap Bool
generateParametres _ 0 = I.empty
generateParametres solutionRules generator = case (V.findIndex (/= S.empty) solutionRules) of
    Just column -> I.insert column (even generator) (generateParametres (solutionRules V.// [(column, S.empty)]) (generator `div` 2))
    Nothing     -> I.empty

findFirstSquare :: S.IntSet -> Integer -> Integer
findFirstSquare solution n = S.fold construct 1 solution
    where
        construct current previous = ((intToInteger current + squareRoot) * previous) `mod` n
        squareRoot = (integerSquareRoot n) + 1

findSecondSquare :: S.IntSet -> V.Vector (S.IntSet, SP.PrimeIntSet) -> Integer -> Integer
findSecondSquare solution indexedFactorisations n = I.foldrWithKey computeRoot 1 countSquares
    where
        computeRoot key power previous = case (odd power) of
            True  -> error "Linear algebra issue."
            False -> ((intToInteger key) ^ (power `div` 2 :: Int) * previous) `mod` n
        countSquares = V.foldl countPowers I.empty squares
        countPowers acc factorisation = SP.foldr (\key im -> I.insertWith (+) (unPrime key) 1 im) acc factorisation
        squares = V.mapMaybe selectSolution indexedFactorisations
        selectSolution (index, factorisation)
            | (index `S.isSubsetOf` solution) = Just factorisation
            | otherwise                   = Nothing

findFactor :: Integer -> Integer -> V.Vector S.IntSet -> V.Vector (S.IntSet, SP.PrimeIntSet) -> Integer
findFactor n generator solutionRules indexedFactorisations
    | solution == S.empty            = error "Parametres are not large enough."
    | (factor == 1) || (factor == n) = findFactor n (generator + 1) solutionRules indexedFactorisations
    | otherwise                      = factor
        where
            factor = gcd (x - y) n
            x = findFirstSquare solution n
            y = findSecondSquare solution indexedFactorisations n
            -- Only attempts to find solutions by selecting the columns
            -- Does not check linear combinations of them
            -- Is this even necessary?
            solution = convertToSolution parametres solutionRules
            parametres = generateParametres solutionRules generator
