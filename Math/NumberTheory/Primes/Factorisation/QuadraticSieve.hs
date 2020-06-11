module Math.NumberTheory.Primes.Factorisation.QuadraticSieve
  ( quadraticSieve
  , gaussianElimination
  , findFactor
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
quadraticSieve :: Integer -> Int -> Int -> [(Integer, Integer)]
quadraticSieve n b t = runST $ do
    let factorBase = [nextPrime 2..precPrime b]
        squareRoot = integerSquareRoot n
        -- Generating sieving interval. This consists of tuples whose first
        -- component is x^2 - n as x runs from the square root of n for a
        -- total of length t. The second component stores the factorisation
        -- modulo 2 as an IntSet.
        sievingFunction = \j -> integerToInt (j ^ (2 :: Int) - n)
        startingPoint = squareRoot - intToInteger t `div` 2
        sievingInterval = generateInterval sievingFunction startingPoint t b
    sievingIntervalM <- V.thaw sievingInterval
    -- Perform sieving
    smoothSieveM sievingIntervalM factorBase n startingPoint
    sievingIntervalF <- V.unsafeFreeze sievingIntervalM
    -- Filters smooth numbers
    let indexedFactorisations = V.toList (findSmoothNumbers sievingIntervalF)
        solutionBasis = gaussianElimination indexedFactorisations
    -- Checks thorugh all basis elements of kernel
    pure $ trace (show (length solutionBasis)) $ map (\sol -> (findFirstSquare n startingPoint sol, findSecondSquare n b indexedFactorisations sol)) solutionBasis

generateInterval :: (Integer -> Int) -> Integer -> Int -> Int -> V.Vector (Int, SP.PrimeIntSet)
-- Very bad way to take -1 into account
generateInterval f startingPoint dim b = V.map (\x -> (x, isNegative x)) vectorOfValues
    where
        vectorOfValues = V.generate dim (\i -> f (intToInteger i + startingPoint))
        isNegative j = case j < 0 of
            True  -> SP.singleton newPrime
            False -> mempty
        newPrime = nextPrime (b + 1)

-- This algorithm takes the sievingInterval, the factorBase and the
-- modularSquareRoots and divides by all the prime in the factor base
-- storing the factorisations. The smooth numbers correspond to tuples
-- whose first component is 1 and whose second component is their factorisation.
smoothSieveM :: MV.MVector s (Int, SP.PrimeIntSet) -> [Prime Int] -> Integer -> Integer -> ST s ()
smoothSieveM sievingIntervalM factorBase n startingPoint = do
    let t = MV.length sievingIntervalM
    forM_ factorBase $ \prime -> do
        let modularSquareRoots = sqrtsModPrime n ((fromJust . toPrimeIntegral) prime)
        forM_ modularSquareRoots $ \modularSquareRoot -> do
            -- This part needs to be clearer. That is the relation between startingIndex, i, t and modularSquareRoot.
            -- let startingIndex = integerToInt ((intToInteger modularSquareRoot - squareRoot) `mod` (intToInteger . unPrime) prime)
            let startingIndex = integerToInt ((modularSquareRoot - startingPoint) `mod` (intToInteger . unPrime) prime)
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

-- This solves the linear equation. It returns a list of tuples. Its first
-- component is either empty (when the corresponding column contains a pivot)
-- or stores a basis element of the kernel of the matrix (when it corresponds
-- to a free variable). The second component is discarded of.
gaussianElimination :: [(S.IntSet, SP.PrimeIntSet)] -> [S.IntSet]
gaussianElimination [] = []
gaussianElimination (p@(indices ,pivotFact) : xs) = case SP.minView pivotFact of
    Just (pivot, _) -> gaussianElimination (map (\q@(_, fact) -> if pivot `SP.member` fact then xor p q else q) xs )
    Nothing         -> indices : gaussianElimination xs
    where
        xor (a, u) (b, v) = ((a S.\\ b) <> (b S.\\ a), (u SP.\\ SP.unPrimeIntSet v) <> (v SP.\\ SP.unPrimeIntSet u))

findFactor :: Integer -> [(Integer, Integer)] -> Integer
findFactor _ [] = error "Parametres are not large enough."
findFactor n ((x, y) : otherPairs) = if factor == 1 || factor == n then findFactor n otherPairs else factor
    where
        factor = gcd (x - y) n

-- Given a solution, the value of x^2 - n is recomputed. Note that, by contruction,
-- the solution IntSet consists of values which correspond to columns in the
-- original sieving interval (before sieving). Thus, in order to compute x^2 - n,
-- it is sufficient to add the square root.
findFirstSquare ::Integer -> Integer -> S.IntSet -> Integer
findFirstSquare n startingPoint = S.foldr construct 1
    where
        construct index previous = ((intToInteger index + startingPoint) * previous) `mod` n

-- Finds the factorisations corresponding to the selected solutions and computes
-- the total number of times a given prime occurs in the selected factorisations.
-- By construction, for any given prime, this number is even. From here, a
-- square root is computed.
findSecondSquare :: Integer -> Int -> [(S.IntSet, SP.PrimeIntSet)] -> S.IntSet -> Integer
findSecondSquare n b indexedFactorisations solution = I.foldrWithKey computeRoot 1 countPowers
    where
        computeRoot key power previous = (intToInteger key ^ (power `div` 2 :: Int) * previous) `mod` n
        countPowers = foldl count I.empty squares
        -- Do not count Prime representing -1
        count = SP.foldr (\prime im -> if prime /= newPrime then I.insertWith (+) (unPrime prime) (1 :: Int) im else im)
        squares = fmap snd (filter (\(index, _) -> index `S.isSubsetOf` solution) indexedFactorisations)
        newPrime = nextPrime (b + 1)
