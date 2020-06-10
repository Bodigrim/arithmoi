module Math.NumberTheory.Primes.Factorisation.QuadraticSieve
  ( quadraticSieve
  , gaussianElimination
  ) where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.IntMap as I
import qualified Data.IntSet as S
import qualified Math.NumberTheory.Primes.IntSet as SP
import Control.Monad
import Control.Monad.ST
import Data.Maybe
import Debug.Trace
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
        solutionBasis = gaussianElimination (deleteRows indexedFactorisations)
    -- Checks thorugh all basis elements of kernel
    pure $ findFactor n solutionBasis indexedFactorisations

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

-- This function deletes the rows of the matrix which only contain one
-- non-zero entry.
deleteRows :: [(S.IntSet, SP.PrimeIntSet)] -> [(S.IntSet, SP.PrimeIntSet)]
deleteRows matrix = map (\(index, fact) -> (index, fact SP.\\ badPrimes)) matrix
    where
        badPrimes = I.keysSet (I.filter (== 1) countPowers)
        countPowers = foldl count I.empty (fmap snd matrix)
        count = SP.foldr (\key im -> I.insertWith (+) (unPrime key) (1 :: Int) im)

-- This solves the linear equation. It returns a list of tuples. Its first
-- component is either empty (when the corresponding column contains a pivot)
-- or stores a basis element of the kernel of the matrix (when it corresponds
-- to a free variable). The second component is discarded of.
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

-- This function attempts to find factorisation. This loops through all the
-- vector elements in the kernel of the matrix until a factorisation is found.
-- The number @generator@ determines which combination of basis elements is
-- used when cooking up a solution. It does so via its representation base 2.
-- This solution is chosen and the corresponding @factor@ is computed.
findFactor :: Integer -> [S.IntSet] -> [(S.IntSet, SP.PrimeIntSet)] -> Integer
findFactor _ [] _ = error "Parametres are not large enough."
findFactor n (sol : otherSols) indexedFactorisations
    | (factor == 1) || (factor == n) = findFactor n otherSols indexedFactorisations
    | otherwise                      = factor
        where
            factor = gcd (x - y) n
            x = findFirstSquare sol n
            y = findSecondSquare sol indexedFactorisations n

-- Given a solution, the value of x^2 - n is recomputed. Note that, by contruction,
-- the solution IntSet consists of values which correspond to columns in the
-- original sieving interval (before sieving). Thus, in order to compute x^2 - n,
-- it is sufficient to add the square root.
findFirstSquare :: S.IntSet -> Integer -> Integer
findFirstSquare solution n = S.foldr construct 1 solution
    where
        construct current previous = ((intToInteger current + squareRoot) * previous) `mod` n
        squareRoot = integerSquareRoot n + 1

-- Finds the factorisations corresponding to the selected solutions and computes
-- the total number of times a given prime occurs in the selected factorisations.
-- By construction, for any given prime, this number is even. From here, a
-- square root is computed.
findSecondSquare :: S.IntSet -> [(S.IntSet, SP.PrimeIntSet)] -> Integer -> Integer
findSecondSquare solution indexedFactorisations n = I.foldrWithKey computeRoot 1 countPowers
    where
        computeRoot key power previous = (intToInteger key ^ (power `div` 2 :: Int) * previous) `mod` n
        countPowers = foldl count I.empty squares
        count = SP.foldr (\key im -> I.insertWith (+) (unPrime key) (1 :: Int) im)
        squares = fmap snd (filter (\(index, _) -> index `S.isSubsetOf` solution) indexedFactorisations)
