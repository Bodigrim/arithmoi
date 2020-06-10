--{-# LANGUAGE ViewPatterns #-}
--{-# LANGUAGE TupleSections #-}

module Math.NumberTheory.Primes.Factorisation.QuadraticSieve
  ( quadraticSieve
  , gaussianElimination
  , gaussianEliminationM
  ) where

import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.IntMap as I
import qualified Data.IntSet as S
import qualified Math.NumberTheory.Primes.IntSet as SP
import Control.Monad
import Control.Monad.ST
import Debug.Trace
import Data.Maybe
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
        squareRoot = integerSquareRoot n + 1
        -- Generating sieving interval. This consists of tuples whose first
        -- component is x^2 - n as x runs from the square root of n for a
        -- total of length t. The second component stores the factorisation
        -- modulo 2 as an IntSet.
        sievingInterval = V.generate t (\index -> (reduce index, SP.empty))
        reduce i = integerToInt ((squareRoot + intToInteger i) ^ (2 :: Int) - n)
    sievingIntervalM <- V.thaw sievingInterval
    -- Perform sieving
    smoothSieveM sievingIntervalM factorBase n
    sievingIntervalF <- V.unsafeFreeze sievingIntervalM
    -- Filters smooth numbers
    let indexedFactorisations = V.toList (findSmoothNumbers sievingIntervalF)
        solutionBasis = gaussianElimination indexedFactorisations
        r = length solutionBasis
    -- Checks thorugh all possible solutions. That is 2 ^ (dim kernel)
    pure (trace ("Dimension of kernel: " ++ show r) $ findFactor n 1 solutionBasis indexedFactorisations)
    -- -- Filters smooth numbers
    -- let indexedFactorisations = findSmoothNumbers sievingIntervalF
    -- -- indexedFactorisationsM <- V.thaw indexedFactorisations
    -- -- Solves linear equation
    -- gaussianEliminationM indexedFactorisationsM
    -- solutionBasis <- V.unsafeFreeze indexedFactorisationsM
    -- let r = length (V.findIndices (/= S.empty) (fmap fst solutionBasis))
    -- -- Checks thorugh all possible solutions. That is 2 ^ (dim kernel)
    -- pure (trace ("Dimension of kernel: " ++ show r) $ (findFactor n 1 (fmap fst solutionBasis) indexedFactorisations))

-- This algorithm takes the sievingInterval, the factorBase and the
-- modularSquareRoots and divides by all the prime in the factor base
-- storing the factorisations. The smooth numbers correspond to tuples
-- whose first component is 1 and whose second component is their factorisation.
smoothSieveM :: MV.MVector s (Int, SP.PrimeIntSet) -> [Prime Int] -> Integer -> ST s ()
smoothSieveM sievingIntervalM factorBase n = do
    let t = MV.length sievingIntervalM
        squareRoot = integerSquareRoot n + 1
    forM_ factorBase $ \prime -> do
        let modularSquareRoots = map integerToInt (sqrtsModPrime n ((fromJust . toPrimeIntegral) prime))
        forM_ modularSquareRoots $ \modularSquareRoot -> do
            let startingIndex = integerToInt ((intToInteger modularSquareRoot - squareRoot) `mod` (intToInteger . unPrime) prime)
            forM_ [startingIndex, startingIndex + unPrime prime..(t - 1)] $ \entry -> do
                let change (y, is) = (y `div` unPrime prime, SP.insert prime is)
                MV.modify sievingIntervalM change entry

-- This function returns the smooth numbers together with their index. This
-- is in order to retrieve later the value of x and x^2 - n. The value stored
-- in the first component of the tuple is a set whose only component is
-- the index of column before sieving.
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

gaussianElimination :: [(S.IntSet, SP.PrimeIntSet)] -> [S.IntSet]
gaussianElimination [] = []
gaussianElimination (p@(indices ,pivotFact) : xs)
    | SP.null pivotFact = indices : gaussianElimination xs
    | otherwise         = gaussianElimination newXs
    where
        newXs = map change xs
        change q@(_, fact) = if pivot `SP.member` fact then xor p q else q
        pivot = SP.findMin pivotFact
        xor (a, u) (b, v) = ((a S.\\ b) <> (b S.\\ a), (u SP.\\ SP.unPrimeIntSet v) <> (v SP.\\ SP.unPrimeIntSet u))

-- This solves the linear equation. It returns a vector of tuples. Its first
-- component is either empty (when the corresponding column contains a pivot)
-- or stores a basis element of the kernel of the matrix (when it corresponds
-- to a free variable). The second component consits of empty vectors.
gaussianEliminationM :: MV.MVector s (S.IntSet, SP.PrimeIntSet) -> ST s ()
gaussianEliminationM indexedFactorisationsM = do
    let s = MV.length indexedFactorisationsM
    -- Loop through columns
    forM_ [0..(s - 1)] $ \column -> do
        x@(_, primeFactorisation) <- MV.read indexedFactorisationsM column
        -- If column is linearly depends on the previous ones go to the next one
        when (primeFactorisation /= SP.empty) $ do
            -- otherwise find the new pivot
            let pivot = SP.findMin primeFactorisation
            -- Delete entries in columns
            forM_ [column..(s - 1)] $ \index -> do
                (_, nextFactorisation) <- MV.read indexedFactorisationsM index
                when (pivot `SP.member` nextFactorisation) $ do
                    let xor (a, u) (b, v) = ((a S.\\ b) <> (b S.\\ a), (u SP.\\ SP.unPrimeIntSet v) <> (v SP.\\ SP.unPrimeIntSet u))
                    -- Remember combinations of columns summing to zero
                    MV.modify indexedFactorisationsM (xor x) index

-- -- This function attempts to find factorisation. This loops through all the
-- -- vector elements in the kernel of the matrix until a factorisation is found.
-- -- The number @generator@ determines which combination of basis elements is
-- -- used when cooking up a solution. It does so via its representation base 2.
-- -- This solution is chosen and the corresponding @factor@ is computed.
-- findFactor :: Integer -> Integer -> V.Vector S.IntSet -> V.Vector (S.IntSet, SP.PrimeIntSet) -> Integer
-- findFactor n generator solutionBasis indexedFactorisations
--     | solution == S.empty            = error "Parametres are not large enough."
--     | (factor == 1) || (factor == n) = findFactor n (generator + 1) solutionBasis indexedFactorisations
--     | otherwise                      = factor
--         where
--             factor = gcd (x - y) n
--             x = findFirstSquare solution n
--             y = findSecondSquare solution indexedFactorisations n
--             solution = convertToSolution parametres solutionBasis
--             parametres = generateParametres solutionBasis generator
--
-- -- Given a solution, the value of x^2 - n is recomputed. Note that, by contruction,
-- -- the solution IntSet consists of values which correspond to columns in the
-- -- original sieving interval (before sieving). Thus, in order to compute x^2 - n,
-- -- it is sufficient to add the square root.
-- findFirstSquare :: S.IntSet -> Integer -> Integer
-- findFirstSquare solution n = S.fold construct 1 solution
--     where
--         construct current previous = ((intToInteger current + squareRoot) * previous) `mod` n
--         squareRoot = integerSquareRoot n + 1
--
-- -- Finds the factorisations corresponding to the selected solutions and computes
-- -- the total number of times a given prime occurs in the selected factorisations.
-- -- By construction, for any given prime, this number is even. From here, a
-- -- square root is computed.
-- findSecondSquare :: S.IntSet -> V.Vector (S.IntSet, SP.PrimeIntSet) -> Integer -> Integer
-- findSecondSquare solution indexedFactorisations n = I.foldrWithKey computeRoot 1 countSquares
--     where
--         computeRoot key power previous = (intToInteger key ^ (power `div` 2 :: Int) * previous) `mod` n
--         countSquares = V.foldl countPowers I.empty squares
--         countPowers = SP.foldr (\key im -> I.insertWith (+) (unPrime key) 1 im)
--         squares = V.mapMaybe selectSolution indexedFactorisations
--         selectSolution (index, factorisation)
--             | index `S.isSubsetOf` solution = Just factorisation
--             | otherwise                     = Nothing
--
-- -- Given the IntMap computed in generateParametres, a solution is constructed
-- -- summing through all those columns whose value in IntMap is False.
-- convertToSolution :: I.IntMap Bool -> V.Vector S.IntSet -> S.IntSet
-- convertToSolution parametres = V.ifoldr add S.empty
--     where
--         add index rule previous = case I.lookup index parametres of
--             Just True  -> previous
--             Just False -> xor rule previous
--             Nothing    -> previous
--             where
--                 xor a b = (a S.\\ b) <> (b S.\\ a)
--
-- -- This function converts a seed @generator@ into a binary vector which
-- -- determines which basis elements are summed. The key in IntMap is the
-- -- number of the column (before sieving)
-- generateParametres :: V.Vector S.IntSet -> Integer -> I.IntMap Bool
-- generateParametres _ 0 = I.empty
-- generateParametres solutionBasis generator = case V.findIndex (/= S.empty) solutionBasis of
--     Just column -> I.insert column (even generator) (generateParametres (solutionBasis V.// [(column, S.empty)]) (generator `div` 2))
--     Nothing     -> I.empty

-- This function attempts to find factorisation. This loops through all the
-- vector elements in the kernel of the matrix until a factorisation is found.
-- The number @generator@ determines which combination of basis elements is
-- used when cooking up a solution. It does so via its representation base 2.
-- This solution is chosen and the corresponding @factor@ is computed.
findFactor :: Integer -> Integer -> [S.IntSet] -> [(S.IntSet, SP.PrimeIntSet)] -> Integer
findFactor n generator solutionBasis indexedFactorisations
    | solution == S.empty            = error "Parametres are not large enough."
    | (factor == 1) || (factor == n) = findFactor n (generator + 1) solutionBasis indexedFactorisations
    | otherwise                      = factor
        where
            factor = gcd (x - y) n
            x = findFirstSquare solution n
            y = findSecondSquare solution indexedFactorisations n
            solution = convertToSolution (baseTwo generator) solutionBasis
            baseTwo = L.unfoldr (\t -> if t == 0 then Nothing else Just (even t, t `div` 2))

-- Given a solution, the value of x^2 - n is recomputed. Note that, by contruction,
-- the solution IntSet consists of values which correspond to columns in the
-- original sieving interval (before sieving). Thus, in order to compute x^2 - n,
-- it is sufficient to add the square root.
findFirstSquare :: S.IntSet -> Integer -> Integer
findFirstSquare solution n = S.fold construct 1 solution
    where
        construct current previous = ((intToInteger current + squareRoot) * previous) `mod` n
        squareRoot = integerSquareRoot n + 1

-- Finds the factorisations corresponding to the selected solutions and computes
-- the total number of times a given prime occurs in the selected factorisations.
-- By construction, for any given prime, this number is even. From here, a
-- square root is computed.
findSecondSquare :: S.IntSet -> [(S.IntSet, SP.PrimeIntSet)] -> Integer -> Integer
findSecondSquare solution indexedFactorisations n = I.foldrWithKey computeRoot 1 countSquares
    where
        computeRoot key power previous = (intToInteger key ^ (power `div` 2 :: Int) * previous) `mod` n
        countSquares = foldl countPowers I.empty squares
        countPowers = SP.foldr (\key im -> I.insertWith (+) (unPrime key) 1 im)
        squares = fmap snd (filter (\(index, _) -> index `S.isSubsetOf` solution) indexedFactorisations)

-- Given a list of Boolean values, a solution is constructed summing all the
-- basis elements whose corresponding value in the list is False.
convertToSolution :: [Bool] -> [S.IntSet] -> S.IntSet
convertToSolution [] _ = S.empty
convertToSolution _ [] = S.empty
convertToSolution (True : xsb) (_ : xsis) = convertToSolution xsb xsis
convertToSolution (False : xsb) (xis : xsis) = xor xis (convertToSolution xsb xsis)
    where
        xor a b = (a S.\\ b) <> (b S.\\ a)
