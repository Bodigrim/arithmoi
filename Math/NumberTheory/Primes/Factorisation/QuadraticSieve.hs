{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.Primes.Factorisation.QuadraticSieve
  ( quadraticSieve
  , findSquares
  ) where

#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup
#endif
import qualified Data.List as L
import qualified Data.IntMap as I
import qualified Data.IntSet as S
import qualified Data.Vector as V
import qualified Data.Vector.Sized as SV
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Sized as SU
import qualified Data.Mod as M
import qualified Data.Mod.Word as MW
import Math.NumberTheory.Primes
import Math.NumberTheory.Moduli.Sqrt
import Math.NumberTheory.Utils.FromIntegral
import Math.NumberTheory.Primes.Factorisation.LinearAlgebra
import Control.Monad
import Control.Monad.ST
import GHC.TypeNats
import Data.Proxy
import Data.Kind
import Data.Foldable
import Data.Maybe
import Data.Bit
import Data.Bifunctor
import Debug.Trace

import Math.NumberTheory.Roots
-- | Given an odd positive composite Integer @n@ and Int parameters @b@, @t@ and
-- @k@, the Quadratic Sieve attempts to output @factor@, a factor of @n@. The
-- parameter @b@ controls the size of the factor base. This consists of all
-- the relevant primes which are less than or equal to @b@. The parameter @t@
-- controls the length of the sieving interval. The parameter @k@ controls the
-- maximum number of blocks for the sieve to go through (@2 ^ (k - 1)@).
quadraticSieve :: Integer -> Int -> Int -> Int -> Integer
quadraticSieve n t m k = findFactor n $ findSquares n t m k

findFactor :: Integer -> [(Integer, Integer)] -> Integer
findFactor _ [] = error "Quadratic Sieve failed."
findFactor n ((x, y) : otherSquares)
  | factor /= 1 && factor /= n   = factor
  | (x * x - y * y) `mod` n /= 0 = error ("Algorithm incorrect." ++ show (x, y))
  | otherwise                    = findFactor n otherSquares
  where
    factor = gcd (x - y) n

-- This routine outputs an infinite list of tuples @(x, y)@ such that
-- @x ^ 2 - y ^ 2 `mod` n = 0@. A factorisation can be infered from this data
-- in at least a half of the cases.
findSquares :: Integer -> Int -> Int -> Int -> [(Integer, Integer)]
-- k indicates maximum number of sieve blocks to go thorugh 2 ^ (k - 1)
findSquares n t m k = runST $ do
  let
    factorBase = [nextPrime 2..precPrime t]
    -- Make sure rootOfA is an Int
    rootOfA = traceShowId $ ((2 * fromInteger n) ** (1 / (4 * fromIntegral k))) / (fromIntegral m ** (1 / (2 * fromIntegral k))) :: Double
    listOfFactors = generatePrimes n rootOfA k
    decompositionOfA = zip (map (fromJust . toPrimeIntegral) listOfFactors) (repeat 2)
    a = foldr (\(p, i) acc -> acc * unPrime p ^ i) 1 decompositionOfA
    valuesOfB = trace (show ((fromInteger (integerSquareRoot (2*n))) / (fromInteger (a * fromIntegral m)) :: Double)) $ filter (<= a `div` 2) $ sqrtsModFactorisation n decompositionOfA
    valuesOfC = map (\x -> (x * x - n) `div` a) valuesOfB
    mappingFunctions = zipWith (curry (\ (b, c) x -> a * x * x + 2 * b * x + c)) valuesOfB valuesOfC
    squareRoots = map (findSquareRoots n m) factorBase

    goSieving :: [(Integer, I.IntMap Int)] -> [Integer -> Integer] -> [Integer] -> [Integer] -> [(Integer, I.IntMap Int)]
    goSieving _ [] _ _ = trace ("Parameters are not large enough: " ++ show (n, t, m, k)) [] --error "Parameters are not large enough."
    goSieving _ _ [] _ = trace ("Parameters are not large enough: " ++ show (n, t, m, k)) [] --error "Parameters are not large enough."
    goSieving _ _ _ [] = trace ("Parameters are not large enough: " ++ show (n, t, m, k)) [] --error "Parameters are not large enough."
    goSieving previousFactorisations (f : fs) (b : bs) (c : cs) = runST $ do
      let
        sievingInterval = generateInterval f m -- trace ("a: " ++ show a ++ "\nb: " ++ show b ++ "\nc: " ++ show c ++ "\nm: " ++ show m) $
      sievingIntervalM <- V.thaw sievingInterval
      smoothSieveM sievingIntervalM (zip factorBase squareRoots) a b c m
      sievingIntervalF <- V.unsafeFreeze sievingIntervalM
      let
        smoothNumbers = previousFactorisations ++ V.toList (findSmoothNumbers m a b sievingIntervalF)-- Consider removing duplicates
        matrixSmoothNumbers
          -- Also takes the sign into account.
          | numberOfConstraints < length mat = trace (show (length mat, numberOfConstraints)) $ smoothNumbers -- trace (show (numberOfConstraints, length mat))
          | otherwise                        = trace (show (length mat, numberOfConstraints)) $ goSieving smoothNumbers fs bs cs -- trace (show (numberOfConstraints, length mat)) $
          where
            numberOfConstraints = S.size $ foldr (\col acc -> acc <> I.keysSet col) mempty mat
            mat = fmap snd smoothNumbers
      pure matrixSmoothNumbers

    indexedSmoothNumbers = goSieving [] mappingFunctions valuesOfB valuesOfC-- trace ("Factor Base: " ++ show (length (filter (\x -> head x /= []) squareRoots))) $

    goSolving :: Int -> [(Integer, I.IntMap Int)] -> [(Integer, Integer)]
    goSolving _ [] = [(0,0)] -- Only to facilitate testing
    goSolving seed sievingData = firstSquare `seq` secondSquare `seq` (firstSquare, secondSquare) : goSolving (seed + 1) sievingData
      where
        firstSquare = findFirstSquare n (fmap fst squaresData)
        secondSquare = findSecondSquare n (fmap snd squaresData)
        -- Add factorisation of a and
        squaresData = map (second (I.unionWith (+) (I.fromList (zip (map unPrime listOfFactors) (repeat 2)))) . (usefulSievingData !!)) solution
        solution = convertToList $ linearSolve' seed $ translate $ fmap (convertToSet . snd) usefulSievingData
        usefulSievingData = removeRows sievingData

  pure $ goSolving (integerToInt n) indexedSmoothNumbers

generatePrimes :: Integer -> Double -> Int -> [Prime Int]
generatePrimes n midPoint len = lowerPrimes ++ higherPrimes
  where
    higherPrimes = take (len - length lowerPrimes) $ filter positiveResidue $ generatePrimesForwards $ floor midPoint + 1
    -- The length of @lowerPrimes@ may not be @len `div` 2@
    lowerPrimes = take (len `div` 2) $ filter positiveResidue $ generatePrimesBackwards $ floor midPoint
    positiveResidue p = jacobi n ((intToInteger . unPrime) p) == One

generatePrimesForwards :: Int -> [Prime Int]
generatePrimesForwards from
  -- 2 cannot be a factor of a
  | from <= 2 = generatePrimesForwards 3
  | otherwise = [nextPrime from..]

generatePrimesBackwards :: Int -> [Prime Int]
generatePrimesBackwards to
  -- 2 cannot be a factor of a
  | to <= 2   = []
  | otherwise = precPrime to : generatePrimesBackwards (unPrime (precPrime to) - 1)

findSquareRoots :: Integer -> Int -> Prime Int -> [[Integer]]
findSquareRoots n m = go 1
  where
    go :: Word -> Prime Int -> [[Integer]]
    go power prime
      | unPrime prime ^ power > 2 * m + 1 = []
      -- If @null roots@, no further roots will be found so this can be optimised.
      -- Nonetheless, it may be important for all to have the same length.
      | null roots                        = [[], []]
      | otherwise                         = roots : go (power + 1) prime
      where
        roots = sqrtsModFactorisation n [((fromJust . toPrimeIntegral) prime, power)]

-- This routine generates the sieving interval. It takes a function @f@,
-- @startingPoint@, and a dimension @dim@. It returns tuples whose
-- first component is @f@ applied to @x@, as @x@ runs from @startingPoint@
-- for a toal length of @dim@. The second component stores the factorisation
-- modulo 2 as an SignedPrimeIntSet. It is initialised to store whether
-- @x@ is negative. Its factorisation is computed in the sieve.
generateInterval :: (Integer -> Integer) -> Int -> V.Vector (Integer, I.IntMap Int)
generateInterval f m = V.generate (2 * m + 1) go
  where
    go i = x `seq` sps `seq` (x, sps)
      where
        x = f $ intToInteger $ i - m
        sps = if x < 0 then I.singleton 0 1 else mempty

-- This algorithm takes @sievingIntervalM@, @factorBase@, the integer @n@ to be
-- factored and the @startingPoint@. It divides by all the primes in
-- @factorBase@ storing the factorisations of the numbers in the
-- @sievingIntervalM@. When, a division occurs, the value in the interval is
-- divided. At the end of the process, the smooth numbers correspond to tuples
-- whose first component is 1. The second component is their factorisation.
smoothSieveM :: MV.MVector s (Integer, I.IntMap Int) -> [(Prime Int, [[Integer]])] -> Integer -> Integer -> Integer -> Int -> ST s ()
smoothSieveM sievingIntervalM base a b c m =
  forM_ base $ \(prime, roots) ->
    -- After this bound there is no certainty that at least one elemnt in the sieve will be divisible. It may however be worth increasing.
    -- Consider filtering indices here with (takeWhile (\r -> (unPrime prime) ^ r <=  2 * m + 1) [1,2..]). Somehow, it does not terminate.
    forM_ (zip roots [1,2..]) $ \(squareRootsOfPower, power) -> case someNatVal (intToNatural (unPrime prime ^ power)) of
      SomeNat (Proxy :: Proxy primePower) -> do
        let
          startingIndices = case MW.invertMod (fromInteger a :: MW.Mod primePower) of
            Just inverseOfA -> map (\root -> (wordToInt . MW.unMod) (fromIntegral m + fromInteger (- b + root) * inverseOfA :: MW.Mod primePower)) squareRootsOfPower
            Nothing         -> case MW.invertMod (fromInteger (2 * b) :: MW.Mod primePower) of
              -- Temporary fix. If power > 2, something better has to be found.
              Just inverseOf2B -> [(wordToInt . MW.unMod) (fromIntegral m - fromInteger c * inverseOf2B :: MW.Mod primePower) | power <= 2]
              -- For this to be true 2 cannot be a factor of a.
              -- Better to ouput @unPrime prime@ as a factor.
              Nothing          -> error ("Found an illegal factor: " ++ show prime)
        forM_ startingIndices $ \startingIndex -> do
          let change (y, im) = (y `div` (intToInteger . unPrime) prime, I.insert (unPrime prime) power im)
          forM_ [startingIndex, startingIndex + (unPrime prime ^ power)..(2 * m)] $ \entry ->
            -- let change (y, im) = (if y `mod` (intToInteger . unPrime) prime == 0 then y `div` (intToInteger . unPrime) prime else error ("Sieve fail: " ++ show (prime, power, entry)), I.insert (unPrime prime) power im)
            MV.modify sievingIntervalM change entry

-- This algorithm filters @sievingIntervalF@ for smooth numbers. It returns
-- the smooth numbers together with their @index@ and @startingPoint@.
-- This is needed in order to later compute @firstSquare@.
findSmoothNumbers :: Int -> Integer -> Integer -> V.Vector (Integer, I.IntMap Int) -> V.Vector (Integer, I.IntMap Int)
findSmoothNumbers m a b = V.imapMaybe selectSmooth
  where
    selectSmooth index (residue, factorisation)
      | residue == 1 = Just (a * intToInteger (index - m) + b, factorisation)
      | otherwise    = Nothing

-- Removes all columns of the matrix which contain primes appearing only once.
-- These columns cannot be part of the solution.
-- This is a very lazy way to do it. There is probably a better one.
removeRows :: [(Integer, I.IntMap Int)] -> [(Integer, I.IntMap Int)]
removeRows indexedFactorisations
  | onlyOnce == mempty = indexedFactorisations
  | otherwise          = removeRows $ filter (\(_, im) -> S.null (S.intersection (convertToSet im) onlyOnce)) indexedFactorisations
  where
    onlyOnce = appearsOnlyOnce $ map (convertToSet . snd) indexedFactorisations

-- Find all primes, which appear only once in the input list.
appearsOnlyOnce :: [S.IntSet] -> S.IntSet
appearsOnlyOnce = fst . L.foldl' go (mempty, mempty)
  where
    go (onlyOnce, atLeastOnce) x =
      ((onlyOnce S.\\ x) <> (x S.\\ atLeastOnce), atLeastOnce <> x)

convertToSet :: I.IntMap Int -> S.IntSet
convertToSet = I.foldrWithKey (\key pow set -> if odd pow then key `S.insert` set else set) mempty

-- This routine translates the list of smooth factorisations into a matrix.
-- The prime numbers need to mapped to ints based on their order (Prime 2 -> 1,
-- Prime 3 -> 2,...). The int 0 is reserved for storing whether the number is
-- negative. This is needed since @linearSolve@ takes powers of a matrix, so
-- the indices of columns and rows must match up.
translate :: [S.IntSet] -> SomeKnown SBMatrix
translate listOfFactorisations = translateHelper listOfFactorisations (length listOfFactorisations)
  where
    translateHelper :: [S.IntSet] -> Int -> SomeKnown SBMatrix
    translateHelper columns dim = case someNatVal (fromIntegral dim) of
      SomeNat (_ :: Proxy dim) -> let result :: SBMatrix dim = SBMatrix (fromJust (SV.fromList (map toIndices columns))) in
        SomeKnown result
          where
            toIndices :: KnownNat dim => S.IntSet -> SBVector dim
            toIndices x = SBVector $ U.fromList $ map fromIntegral primeTranslation
                  where
                    primeTranslation = binarySearch (S.toAscList x) indexedPrimes
                    -- How to fold a list of monoids?
                    indexedPrimes = U.fromList . S.toAscList $ fold columns

-- When translating, it becomes necessary to see the index of a certain Prime.
-- @binarySearch@ does so efficiently.
binarySearch :: (Eq a, Ord a, U.Unbox a) => [a] -> U.Vector a -> [Int]
binarySearch list v = go 0 (len - 1) list v
  where
    len = U.length v
    go :: (Eq a, Ord a, U.Unbox a) => Int -> Int -> [a] -> U.Vector a -> [Int]
    go _ _ [] _ = []
    go lowerIndex upperIndex allItems@(item : otherItems) vector = case item `compare` entry of
      LT -> go lowerIndex (currentIndex - 1) allItems vector
      -- @(currentIndex + 1)@ makes sure no prime number's index is 0.
      -- 0 is used to mark negative numbers.
      EQ -> (currentIndex + 1) : go (currentIndex + 1) (len - 1) otherItems vector
      GT -> go (currentIndex + 1) upperIndex allItems vector
      where
        entry = vector U.! currentIndex
        currentIndex = (upperIndex + lowerIndex) `div` 2

-- This datatype facilitates calling @linearSolve@.
data SomeKnown (f :: Nat -> Type) where
  SomeKnown :: KnownNat k => f k -> SomeKnown f

linearSolve' :: Int -> SomeKnown SBMatrix -> SomeKnown DBVector
linearSolve' seed (SomeKnown m) = SomeKnown (linearSolve seed m)

convertToList :: SomeKnown DBVector -> [Int]
convertToList (SomeKnown solution) = listBits $ SU.fromSized $ unDBVector solution

-- Given a solution, it computes the product of the numbers in the first
-- component of the tuple.
-- Consider using Mod for clarity.
findFirstSquare :: Integer -> [Integer] -> Integer
findFirstSquare n squaresData = case someNatVal (integerToNatural n) of
  SomeNat (Proxy :: Proxy n) ->
    naturalToInteger . M.unMod $ foldr (\x acc -> (fromInteger x :: M.Mod n) * acc) (1 :: M.Mod n) squaresData

-- Finds the factorisations corresponding to the selected solutions and computes
-- the total number of times a given prime occurs in the selected factorisations.
-- By construction, for any given prime, this number is even. From here, a
-- square root is computed.
-- Use Mod.
findSecondSquare :: Integer -> [I.IntMap Int] -> Integer
findSecondSquare n factorisations = case someNatVal (integerToNatural n) of
  SomeNat (Proxy :: Proxy n) ->
    -- I would like to move part of this line further down but the compiler cannot deduce what n is.
    naturalToInteger . M.unMod $ I.foldrWithKey (\key power acc -> (fromInteger (fromIntegral key) :: M.Mod n) ^ (power `div` 2 :: Int) * acc) (1 :: M.Mod n) $ I.unionsWith (+) factorisations
  -- where
  --   computeRoot :: KnownNat n => Int -> Int -> M.Mod n -> M.Mod n
  --   computeRoot key power acc = (fromInteger (fromIntegral key) :: M.Mod n) ^ (if even power then (power `div` 2 :: Int) else error "Wrong second square") * acc
