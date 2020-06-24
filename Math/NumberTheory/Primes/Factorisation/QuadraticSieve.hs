{-# LANGUAGE CPP #-}

module Math.NumberTheory.Primes.Factorisation.QuadraticSieve
  ( quadraticSieve
  , findSquares
  ) where

#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup
#endif
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.IntMap as I
import qualified Data.IntSet as S
import qualified Math.NumberTheory.Primes.IntSet as PS
import Control.Monad
import Control.Monad.ST
import Data.Maybe
import Data.Bifunctor
import Math.NumberTheory.Roots
import Math.NumberTheory.Primes
import Math.NumberTheory.Moduli.Sqrt
import Math.NumberTheory.Utils.FromIntegral

data SignedPrimeIntSet = SignedPrimeIntSet
  { sign :: !Bool
  , primeSet :: !PS.PrimeIntSet
  } deriving (Show)

data BoolOrPrime = Bool !Bool | PrimeInt !(Prime Int)

insert :: Prime Int -> SignedPrimeIntSet -> SignedPrimeIntSet
insert prime (SignedPrimeIntSet s ps) = SignedPrimeIntSet s (prime `PS.insert` ps)

nonZero :: SignedPrimeIntSet -> Maybe BoolOrPrime
nonZero (SignedPrimeIntSet s ps) = case PS.minView ps of
  Just (prime, _)  -> Just (PrimeInt prime)
  Nothing          -> if s then Just (Bool True) else Nothing

member :: BoolOrPrime -> SignedPrimeIntSet -> Bool
member value (SignedPrimeIntSet s ps) = case value of
  Bool b     -> s == b
  PrimeInt p -> p `PS.member` ps

xor :: SignedPrimeIntSet -> SignedPrimeIntSet -> SignedPrimeIntSet
xor (SignedPrimeIntSet s1 ps1) (SignedPrimeIntSet s2 ps2) = SignedPrimeIntSet (s1 /= s2) ((ps1 PS.\\ PS.unPrimeIntSet ps2) <> (ps2 PS.\\ PS.unPrimeIntSet ps1))

-- | Given an odd positive composite Integer @n@ and Int parameters @b@ and @t@,
-- the Quadratic Sieve attempts to output @factor@, a factor of @n@. If it fails,
-- it throws an exception. The parameter @b@ controls the size of the factor base.
-- This consists of all the relevant primes which are less than or equal to @b@.
-- The parameter @t@ controls the length of the sieving interval.
quadraticSieve :: Integer -> Int -> Int -> Integer
quadraticSieve n b t = findFactor n $ findSquares n b t

findFactor :: Integer -> [(Integer, Integer)] -> Integer
findFactor n pairs = case L.find (\(x, y) -> gcd (x - y) n /= 1 && gcd (x - y) n /= n) pairs of
  Just (x, y) -> gcd (x - y) n
  Nothing     -> error "Parameters are not large enough."

-- | This algorithm returns pairs from whose a factor of @n@ will later be inferred.
-- It is exported only for testing.
findSquares :: Integer -> Int -> Int -> [(Integer, Integer)]
findSquares n b t = runST $ do
  let
    factorBase = [nextPrime 2..precPrime b]
    squareRoot = integerSquareRoot n
    sievingFunction j = j * j - n
    startingPoint = squareRoot - intToInteger t `div` 2
    sievingInterval = generateInterval sievingFunction startingPoint t
  sievingIntervalM <- V.thaw sievingInterval
  smoothSieveM sievingIntervalM factorBase n startingPoint
  sievingIntervalF <- V.unsafeFreeze sievingIntervalM
  let
    indexedFactorisations = removeRows $ V.toList (findSmoothNumbers sievingIntervalF)
    solutionBasis = gaussianElimination indexedFactorisations
    unsignedFactorisations = map (second primeSet) indexedFactorisations

  pure $ map (\sol -> (findFirstSquare n startingPoint sol, findSecondSquare n unsignedFactorisations sol)) solutionBasis

-- This routine generates the sieving interval. It takes a function @f@,
-- @startingPoint@, and a dimension @dim@. It returns tuples whose
-- first component is @f@ applied to @x@, as @x@ runs from @startingPoint@
-- for a toal length of @dim@. The second component stores the factorisation
-- modulo 2 as an SignedPrimeIntSet. It is initialised to store whether the
-- @x@ is positive and negative. Its factorisation is computed in the sieve.
generateInterval :: (Integer -> Integer) -> Integer -> Int -> V.Vector (Integer, SignedPrimeIntSet)
generateInterval f startingPoint dim = V.map (\x -> (x, isNegative x)) vectorOfValues
  where
    vectorOfValues = V.generate dim (\i -> f (intToInteger i + startingPoint))
    isNegative j = SignedPrimeIntSet (j < 0) mempty

-- This algorithm takes @sievingIntervalM@, @factorBase@, the integer @n@ to be
-- factored and the @startingPoint@. It divides by all the primes in
-- @factorBase@ storing the factorisations of the numbers in the
-- @sievingIntervalM@. When, a division occurs, the value in the interval is
-- divided. At the end of the process, the smooth numbers correspond to tuples
-- whose first component is 1. The second component is their factorisation.
smoothSieveM :: MV.MVector s (Integer, SignedPrimeIntSet) -> [Prime Int] -> Integer -> Integer -> ST s ()
smoothSieveM sievingIntervalM factorBase n startingPoint = do
  let t = MV.length sievingIntervalM
  forM_ factorBase $ \prime -> do
    let modularSquareRoots = sqrtsModPrime n ((fromJust . toPrimeIntegral) prime)
    forM_ modularSquareRoots $ \modularSquareRoot -> do
      let startingIndex = integerToInt ((modularSquareRoot - startingPoint) `mod` (intToInteger . unPrime) prime)
      forM_ [startingIndex, startingIndex + unPrime prime..(t - 1)] $ \entry -> do
        let change (y, set) = (y `div` (intToInteger . unPrime) prime, prime `insert` set)
        MV.modify sievingIntervalM change entry

-- This algorithm filters the @sievingIntervalF@ for smooth numbers. It returns
-- the smooth numbers together with their index. This is needed in order to
-- later retrieve the value of @x@ and therefore @f(x)@. This index is stored
-- as a singleton IntSet to prepare the data for Gaussian elimination.
findSmoothNumbers :: V.Vector (Integer, SignedPrimeIntSet) -> V.Vector (S.IntSet, SignedPrimeIntSet)
findSmoothNumbers = V.imapMaybe selectSmooth
  where
    selectSmooth index (residue, factorisation)
      | residue == 1 = Just (S.singleton index, factorisation)
      | otherwise    = Nothing

-- Find all primes, which appear only once in the input list.
appearsOnlyOnce :: [PS.PrimeIntSet] -> PS.PrimeIntSet
appearsOnlyOnce = fst . L.foldl' go (mempty, mempty)
  where
    go (onlyOnce, atLeastOnce) x =
      ((onlyOnce PS.\\ PS.unPrimeIntSet x) <> (x PS.\\ PS.unPrimeIntSet atLeastOnce), atLeastOnce <> x)

-- Removes all columns of the matrix which contain primes appearing only once.
-- These columns cannot be part of the solution.
removeRows :: [(S.IntSet, SignedPrimeIntSet)] -> [(S.IntSet, SignedPrimeIntSet)]
removeRows indexedFactorisations
  | onlyOnce == mempty = indexedFactorisations
  | otherwise          = removeRows $ filter (\(_, SignedPrimeIntSet _ xs) -> PS.disjoint xs onlyOnce) indexedFactorisations
  where
    onlyOnce = appearsOnlyOnce $ map (primeSet . snd) indexedFactorisations

-- This solves the linear equation. It returns a basis for the kernel
-- of the matrix as a list of IntSet.
gaussianElimination :: [(S.IntSet, SignedPrimeIntSet)] -> [S.IntSet]
gaussianElimination [] = []
gaussianElimination (p@(indices, pivotFact) : xs) = case nonZero pivotFact of
  Just pivot -> gaussianElimination (map (\q@(_, fact) -> if pivot `member` fact then add p q else q) xs)
  Nothing    -> indices : gaussianElimination xs
  where
    add (a, u) (b, v) = ((a S.\\ b) <> (b S.\\ a), xor u v)

-- Given a solution, the value of @f(x)@ is computed again. By contruction,
-- the solution IntSet consists of values which correspond to columns in the
-- original sieving interval.
findFirstSquare ::Integer -> Integer -> S.IntSet -> Integer
findFirstSquare n startingPoint = S.foldr construct 1
  where
    construct index previous = ((intToInteger index + startingPoint) * previous) `mod` n

-- Finds the factorisations corresponding to the selected solutions and computes
-- the total number of times a given prime occurs in the selected factorisations.
-- By construction, for any given prime, this number is even. From here, a
-- square root is computed.
findSecondSquare :: Integer -> [(S.IntSet, PS.PrimeIntSet)] -> S.IntSet -> Integer
findSecondSquare n indexedFactorisations solution = I.foldrWithKey computeRoot 1 countPowers
  where
    computeRoot key power previous = (intToInteger key ^ (power `div` 2 :: Int) * previous) `mod` n
    countPowers = foldl count I.empty squares
    count = PS.foldr (\prime im -> I.insertWith (+) (unPrime prime) (1 :: Int) im)
    squares = fmap snd (filter (\(index, _) -> index `S.isSubsetOf` solution) indexedFactorisations)
