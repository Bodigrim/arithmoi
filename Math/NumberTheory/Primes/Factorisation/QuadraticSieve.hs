-- |
-- Module:      Math.NumberTheory.Primes.Factorisation.QuadraticSieve
-- Copyright:   (c) 2020 Federico Bongiorno
-- Licence:     MIT
-- Maintainer:  Federico Bongiorno <federicobongiorno97@gmail.com>
--
-- <https://en.wikipedia.org/wiki/Quadratic_sieve Quadratic Sieve> algorithm
-- employing multiple polynomials and large prime variation.

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.Primes.Factorisation.QuadraticSieve
  ( QuadraticSieveConfig(..)
  , quadraticSieve
  , quadraticSieveManual
  , autoConfig
  , findRoots
  ) where

#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup
#endif
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import qualified Data.Vector.Sized as SV
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.Mod as M
import qualified Data.Mod.Word as MW
import Math.NumberTheory.Utils
import Math.NumberTheory.Roots
import Math.NumberTheory.Primes
import Math.NumberTheory.Logarithms
import Math.NumberTheory.Moduli.Sqrt
import Math.NumberTheory.Utils.FromIntegral
import Math.NumberTheory.Primes.Factorisation.LinearAlgebra
import Math.NumberTheory.Primes.Factorisation.TrialDivision
import Control.Monad
import Control.Monad.ST
import GHC.TypeNats
import Data.Proxy
import Data.Foldable
import Data.Maybe
import Data.Bits
import Data.Bifunctor
import qualified Debug.Trace

trace :: String -> a -> a
trace = if tuning then Debug.Trace.trace else const id

-- This variable can be set to @True@ when tuning parameters. It prints
-- information about the sieving and solving processes.
tuning :: Bool
tuning = False

-- | This data type is used to configure running ot the quadratic sieve. It
-- comprises four parameters.
-- 1. @qscFactorBase@ controls the size of the factor base. More precisely the
-- factor base consists of all primes up to the given bound such that @n@, the
-- integer to be factored, is a square modulo @p@.
-- 2. @qscSievingInterval@ controls the length of the sieving interval. This is
-- given by @[i | i <- [-qscSievingInterval..qscSievingInterval]]@.
-- 3. @qscNumberOfBlocks@ controls the number of sieving blocks to run with a
-- fixed initialisation. Inputting @0@ results in running the standard quadratic
-- sieve. Inputting a larger number results in running the multiple polynomials
-- variant. In this case, the number of blocks employed with a fixed
--  initialisation is @2 ^ qscNumberOfBlocks - 1@.
-- 4. @qscSmoothThreshold@ is the threshold to select smooth numbers. After
-- approximate log sieving, the values which are smaller than the threshold are
-- checked for smoothness.
data QuadraticSieveConfig = QuadraticSieveConfig
  { qscFactorBase :: Int
  , qscSievingInterval :: Int
  , qscNumberOfBlocks :: Int
  , qscSmoothThreshold :: Int
  } deriving (Show)

-- | Given an integer @n@ to factor, this routine produces a configuaration
-- to run the quadratic sieve. Significantly better results may be
-- obtained by tuning the algorithm manually.
autoConfig :: Integer -> QuadraticSieveConfig
autoConfig n = QuadraticSieveConfig t m k h
  where
    h = intLog2 t + 4
    k = max 0 (l `div` 10)
    m = t
    t
      | l < 3     = integerToInt $ n `div` 2
      | l < 8     = integerToInt $ integerSquareRoot n
      | otherwise = max (41 - l) 1 * floor (exp (sqrt (le * log le) / 2) :: Double)
    -- number of digits of n
    l = integerLog10 n
    le = intToDouble l * log 10

-- | Given an odd positive composite Integer @n@, @quadraticSieve@ outputs a
-- factor of @n@. These conditions are not checked.
--
-- >>> quadraticSieve 15
-- 5
quadraticSieve :: Integer -> Integer
quadraticSieve n = quadraticSieveManual n $ autoConfig n

-- | This is a more general version of the algorithm where the user can input
--  a manual configuration.
--
-- >>> quadraticSieveManual 15 $ QuadraticSieveConfig 0 0 0 0
-- Exception: Math.NumberTheory.Primes.Factorisation.QuadraticSieve: Parameters are not large enough.
quadraticSieveManual :: Integer -> QuadraticSieveConfig -> Integer
quadraticSieveManual n qsc = findFactor n $ findRoots n qsc

-- This routine attempts to infer a factorisation from a pair of numbers
-- @(x, y)@ such that @(x ^ 2 - y ^ 2) `mod` n = 0@. If it fails, it calls
-- the linear algebra routine again to provide another pair.
findFactor :: Integer -> [(Integer, Integer)] -> Integer
findFactor _ [] = error "Math.NumberTheory.Primes.Factorisation.QuadraticSieve: Parameters are not large enough."
findFactor n ((x, y) : otherRoots)
  | factor /= 1 && factor /= n   = factor
  | otherwise                    = findFactor n otherRoots
  where
    factor = gcd (x - y) n

-- | This routine outputs an infinite list of tuples @(x, y)@ such that
-- @(x ^ 2 - y ^ 2) `mod` n = 0@. A factorisation can be infered from this data
-- in at least a half of the cases. The algorithm employs multiple polynomials
-- with self-initialisation, approximate log sieving and the large prime variation.
-- The algorithm has four steps:
-- 1. Data Initialisation: a factor base and respective roots are computed.
-- 2. Data Collection: sieving is performed to find enough smooth numbers.
-- 3. Data Processing: a linear algebra routine is called to find dependencies.
-- 4. Factor Inference: a factor is inferred from the processed data.
findRoots :: Integer -> QuadraticSieveConfig -> [(Integer, Integer)]
findRoots n qsc@(QuadraticSieveConfig t m k h) = trace ("Parameters: " ++ show qsc) $ runST $ do
  let
    -- 1. In the first part of the algorithm, the data is initialised.
    initialFactorBase = if t < 3 then error "Math.NumberTheory.Primes.Factorisation.QuadraticSieve: Parameters are not large enough."
      else [nextPrime 2..precPrime t]
    factorBaseWithSquareRoots = map (\p -> (p, (sqrtsModPrime n . fromJust . toPrimeIntegral) p)) initialFactorBase
    -- Prime numbers such that @n@ is a square modulo @p@.
    factorBase = fst <$> filter (not . null . snd) factorBaseWithSquareRoots
    -- @a@ is the leading coefficient of the multiple polynomials to be used in
    -- one initialisation. To maximise efficiency, its k^th root needs to be
    -- around the number computed below. This is explained in page 274 of
    -- Crandall and Pomerance's book.
    kRootOfA
      | k <= 0    = 1
      | otherwise = integerRoot (4 * k) ((2 * n) `div` (intToInteger m ^ (2 :: Int)))
    factorsOfA = trace ("Size of Factor Base: " ++ show (length factorBase)) $
      -- The factors of @a@ are chosen as closely as possible to @kRootOfA@.
      map isPrimeInt $ generatePrimes n kRootOfA k
    initialDecompositionOfA = map (,2) factorsOfA

    -- 2. In the second part of the algorithm, the goal is to find enough smooth numbers.
    goSieving :: M.Map Integer (IM.IntMap Int) -> [(Prime Integer, Word)] -> M.Map Integer (IM.IntMap Int)
    goSieving previousDiffSmoothNumbers decompositionOfA = goSelfInitSieving previousDiffSmoothNumbers $ zip valuesOfB valuesOfC
      where
        a = factorBack decompositionOfA
        -- The @Ratio@ traced below needs to be close to @1@ for maximal efficiency.
        valuesOfB = trace ("Ratio: " ++ show ((fromInteger (a * intToInteger m) :: Double) / fromInteger (integerSquareRoot (2*n)))) $
          -- Only one root is picked since they produce equivalent data.
          filter (<= a `div` 2) $ sqrtsModFactorisation n decompositionOfA
        -- @b@ and @c@ are chosen so that @b ^ 2 - a * c = n@.
        valuesOfC = map (\x -> (x * x - n) `div` a) valuesOfB

        -- Multiple polynomials are used with the same leading coefficient.
        goSelfInitSieving :: M.Map Integer (IM.IntMap Int) -> [(Integer, Integer)] -> M.Map Integer (IM.IntMap Int)
        -- If there are no more polynomials, then choose a different @a@.
        goSelfInitSieving previousSmoothNumbers [] = goSieving previousSmoothNumbers nextDecompositionOfA
          where
            nextDecompositionOfA
              -- Change from @k = 0@ to @k = 1@.
              | null decompositionOfA = map ((, 2) . isPrimeInt) (generatePrimes n 1 1)
              -- @k@ remains the same. The smallest prime is dropped and the next suitable prime is picked.
              | otherwise             = tail decompositionOfA ++ [(isPrimeInt nextFactor, 2)]
              where
                nextFactor = head $ generatePrimes n highestPrime 1
                highestPrime = unPrime . fst . maximum $ decompositionOfA

        goSelfInitSieving previousSmoothNumbers ((b, c) : otherCoeffs) = runST $ do
          let
            -- @f@ is the polynomial used during sieving.
            f x = a * x * x + 2 * b * x + c
            sievingInterval = V.generate (2 * m + 1) (f . intToInteger . (+ (-m)))
            -- @0@ is an element of @sievingInterval@ only if @n@ is a square number.
            -- This throws an exception when applying @integerLog2@.
            sievingLogInterval = V.convert (V.map (integerLog2 . abs) sievingInterval) :: U.Vector Int
          sievingLogIntervalM <- U.unsafeThaw sievingLogInterval
          smoothLogSieveM sievingLogIntervalM factorBaseWithSquareRoots a b c m
          sievedLogInterval <- U.unsafeFreeze sievingLogIntervalM
          let
            newSmoothNumbers = findLogSmoothNumbers factorBase m h decompositionOfA b sievingInterval sievedLogInterval
            currentSmoothNumbers = previousSmoothNumbers <> newSmoothNumbers
            smoothNumbers
              -- This traces the number of smooth numbers and primes found in the previous sieving block.
              | trace ("Smooth Numbers: " ++ show (M.size currentSmoothNumbers) ++ "\nPrimes: " ++ show numberOfConstraints) False = undefined
              -- Enough smooth numbers are found. Not too many smooth numbers are
              -- taken to ensure the dimension of the matrix is not too large.
              | numberOfConstraints < M.size currentSmoothNumbers = M.take (numberOfConstraints + 5 * (k + 1)) currentSmoothNumbers
              -- More smooth numbers are needed.
              | otherwise                                         = goSelfInitSieving currentSmoothNumbers otherCoeffs
              where
                -- The tracing prints how many numbers satisfy the given threshold and how many of them are actually smoooth.
                numberOfConstraints = trace ("Log Filtering: " ++ show (U.length (U.filter (<= h) sievedLogInterval)) ++ " -> " ++ show (M.size newSmoothNumbers)) $
                  IS.size $ foldMap setOddPowers currentSmoothNumbers
          -- Removes columns which could never be part of a solution.
          pure $ removeColumns 0 smoothNumbers

    sievingData = goSieving mempty initialDecompositionOfA
    matrix = translate . map setOddPowers $ M.elems sievingData

    -- 3. In the third part of the algorithm, the linear algebra routine is called
    goSolving :: Int -> Int -> [(Integer, Integer)]
    goSolving seed counter
      -- Solve the matrix with a different seed.
      | counter < 5 = firstRoot `seq` secondRoot `seq` (firstRoot, secondRoot) : goSolving (seed + 1) (counter + 1)
      -- In this case, the solutions output by @linearSolve@ are probably identical.
      -- Start again with different parameters.
      | otherwise   = findRoots n $ QuadraticSieveConfig (t + 10 * (k + 1)) (m + 50 * (k + 1) * (k + 1)) k h
      where
        -- 4. In the final part, the two roots are inferred.
        firstRoot = findFirstRoot n (fmap fst rootsData)
        secondRoot = findSecondRoot n (fmap snd rootsData)
        rootsData = map (`M.elemAt` sievingData) solution
        solution = withSomeKnown (convertToList . linearSolve seed) matrix

  -- Prints the size of the matrix.
  pure $ trace ("Size of Matrix: " ++ show (withSomeKnown intVal matrix)) $
    goSolving (integerToInt n) 0

-- This routine is used to pick primes in the factorisation of @a@.
generatePrimes :: Integer -> Integer -> Int -> [Prime Integer]
generatePrimes n midPoint len
  | len <= 0  = []
  | otherwise = lowerPrimes ++ higherPrimes
  where
    higherPrimes = take (len - length lowerPrimes) $ filter positiveResidue $ generatePrimesForwards $ midPoint + 1
    -- The length of @lowerPrimes@ may be smaller than @len `div` 2@
    lowerPrimes = take (len `div` 2) $ filter positiveResidue $ generatePrimesBackwards midPoint
    positiveResidue p = jacobi n (unPrime p) == One

generatePrimesForwards :: Integer -> [Prime Integer]
generatePrimesForwards from = [nextPrime (max 3 from)..]

generatePrimesBackwards :: Integer -> [Prime Integer]
generatePrimesBackwards to
  -- 2 cannot be a factor of a
  | to <= 2   = []
  | otherwise = precPrime to : generatePrimesBackwards (unPrime (precPrime to) - 1)

-- Checks if a prime picked for the factorisation of @a@ is small enough.
isPrimeInt :: Prime Integer -> Prime Integer
isPrimeInt x = fromJust . toPrimeIntegral $ primeInt
  where
    primeInt = fromMaybe (error "Math.NumberTheory.Primes.Factorisation.QuadraticSieve: Parameters are not large enough.")
      (toPrimeIntegral x :: Maybe (Prime Int))

-- This routine takes @sievingIntervalM@, and performs log division by all the
-- primes in the @factorBase@. When a division occurs, the logarithm of the
-- prime is subtracted from the value in the interval.
smoothLogSieveM :: MU.MVector s Int -> [(Prime Int, [Integer])] -> Integer -> Integer -> Integer -> Int -> ST s ()
smoothLogSieveM sievingIntervalM factorBaseWithSquareRoots a b c m =
  forM_ factorBaseWithSquareRoots $ \(prime, roots) -> case someNatVal (intToNatural (unPrime prime)) of
    SomeNat (Proxy :: Proxy prime) -> do
      let
        startingIndices = case MW.invertMod (fromInteger a :: MW.Mod prime) of
          -- For the primes not dividing @a@.
          Just inverseOfA -> map (\root -> (wordToInt . MW.unMod) (fromIntegral m + fromInteger (- b + root) * inverseOfA :: MW.Mod prime)) roots
          -- When a prime belongs to the factorisation of @a@.
          Nothing         -> case MW.invertMod (fromInteger (2 * b) :: MW.Mod prime) of
            Just inverseOf2B -> [(wordToInt . MW.unMod) (fromIntegral m - fromInteger c * inverseOf2B :: MW.Mod prime)]
            -- In this case @p@ divides @n@. This is not possible since any @p@
            -- dividing @a@ has positive quadratic residue with respect to @n@.
            Nothing          -> error "Math.NumberTheory.Primes.Factorisation.QuadraticSieve: Algorithm incorrect."
      forM_ startingIndices $ \startingIndex -> do
        let change y = y - (intLog2 . unPrime) prime
        forM_ [startingIndex, startingIndex + unPrime prime..(2 * m)] $ \entry ->
          MU.modify sievingIntervalM change entry

-- This routine takes the @sievedInterval@ as input and filters for potential
-- smooth numbers using the given threshold. It then checks which of the
-- filtered numbers are smooth by computing their factorisation by trial division.
-- It also adds extra smooth numbers whenever there are numbers which are almost
-- smooth except for one large prime. The output is a map whose keys are integers
-- @x_i@ and whose values are, by construction, the full factorisations of
-- @x_i ^ 2 - n@. The keys are used to compute @firstRoot@ and the values
-- are used to compute @secondRoot@.
findLogSmoothNumbers :: [Prime Int] -> Int -> Int -> [(Prime Integer, Word)] -> Integer -> V.Vector Integer -> U.Vector Int -> M.Map Integer (IM.IntMap Int)
findLogSmoothNumbers factorBase m h decompositionOfA b sievingInterval sievedLogInterval =
  -- Firstly, the filtered interval is converted to a map to remove duplicates.
  -- Then, smooth numbers which do not share a @highestFactor@ with any other
  -- smooth numbers are removed.
  removeColumns highestPrime . M.fromList . V.toList $ V.imapMaybe isSmooth sievingInterval
  where
    highestPrime = unPrime $ maximum factorBase
    isSmooth :: Int -> Integer -> Maybe (Integer, IM.IntMap Int)
    isSmooth index value
      | logResidue <= h && isJust highestFactor = Just (complete index, IM.unionWith (+) intMapA facMap)
      | otherwise                               = Nothing
      where
        logResidue = sievedLogInterval U.! index
        -- This returns the highest factor in a factorisation whenever this
        -- is of Int type and prime. Otherwise returns @Nothing@.
        -- The maximum in @preFac@ is the number that is left after dividing by
        -- all the primes in @factorBase@. Note that @preFac@ is empty whenever
        -- @value = 1@, hence the need to prepend @1@.
        highestFactor = isPrime =<< (toIntegralSized (maximum (1 : fmap fst preFac)) :: Maybe Int)
        -- The conversion to Int is safe since @facMap@ is only used whenever
        -- @highestFactor@ is itself an Int.
        facMap = IM.fromDistinctAscList $ map (bimap integerToInt wordToInt) fullFac
        -- Add negative unit.
        fullFac = if value < 0 then (-1, 1) : preFac else preFac
        -- This performs trial division with prime numbers in the @factorBase@.
        -- It returns the factorisation with respect to the primes in the
        -- @factorBase@ and it appends the remainder of the number after all the
        -- divisions, if this is larger than one. Note that the resulting list
        -- is necessarily in ascending order since the remainder cannot be
        -- smaller than @highestPrime@.
        preFac = trialDivisionWith (map (intToInteger . unPrime) factorBase) value

    -- This function completes the square in @f@.
    complete i = a * intToInteger (i - m) + b
    a = factorBack decompositionOfA
    intMapA = IM.fromDistinctAscList $ map (bimap (integerToInt . unPrime) wordToInt) decompositionOfA

-- Removes all columns of the matrix which contain primes appearing only once
-- and are greater than @lowerBound@. When @lowerBound = 0@, this routine
-- removes columns which cannot be part of the solution.
removeColumns :: Int -> M.Map Integer (IM.IntMap Int) -> M.Map Integer (IM.IntMap Int)
removeColumns lowerBound smoothData
  | onlyOnce == mempty = smoothData
  | otherwise          = removeColumns lowerBound $ M.filter (disjoint onlyOnce . setOddPowers) smoothData
  where
    onlyOnce = IS.filter (> lowerBound) . appearsOnlyOnce $ M.map setOddPowers smoothData

-- This routine finds all primes, which appear only once in the input map.
appearsOnlyOnce :: M.Map Integer IS.IntSet -> IS.IntSet
appearsOnlyOnce = fst . M.foldl' go (mempty, mempty)
  where
    go (onlyOnce, atLeastOnce) x =
      ((onlyOnce IS.\\ x) <> (x IS.\\ atLeastOnce), atLeastOnce <> x)

#if MIN_VERSION_containers(0,5,11)
disjoint :: IS.IntSet -> IS.IntSet -> Bool
disjoint = IS.disjoint
#else

disjoint :: IS.IntSet -> IS.IntSet -> Bool
disjoint x y = IS.null (IS.intersection x y)
#endif

setOddPowers :: IM.IntMap Int -> IS.IntSet
setOddPowers = IM.keysSet . IM.filter odd

-- This routine translates the list of smooth factorisations into a matrix.
-- The prime numbers (and -1) are mapped to Ints based on their order (-1 -> 0,
-- 2 -> 1, 3 -> 2,...). If a prime (or -1) is missing, then their Int values
-- change accordingly (-1 -> 0, 3 -> 1, 17 -> 2,...).
-- This is needed since @linearSolve@ computes powers of a matrix, hence the
-- indices of columns and rows must match up.
translate :: [IS.IntSet] -> SomeKnown SBMatrix
translate listOfFactorisations = translateHelper listOfFactorisations (length listOfFactorisations)
  where
    translateHelper :: [IS.IntSet] -> Int -> SomeKnown SBMatrix
    translateHelper columns dim = case someNatVal (intToNatural dim) of
      SomeNat (_ :: Proxy dim) -> let result :: SBMatrix dim = SBMatrix (fromJust (SV.fromList (map toIndices columns))) in
        SomeKnown result
          where
            indexedPrimes = U.fromList . IS.toAscList $ fold columns
            toIndices :: KnownNat dim => IS.IntSet -> SBVector dim
            toIndices x = SBVector $ U.fromList $ map fromIntegral primeTranslation
                  where
                    primeTranslation = binarySearch (IS.toAscList x) indexedPrimes

-- When translating, it becomes necessary to access the index of a given prime.
-- @binarySearch@ does so efficiently.
binarySearch :: (Eq a, Ord a, U.Unbox a) => [a] -> U.Vector a -> [Int]
binarySearch list v = go 0 (len - 1) list v
  where
    len = U.length v
    go :: (Eq a, Ord a, U.Unbox a) => Int -> Int -> [a] -> U.Vector a -> [Int]
    go _ _ [] _ = []
    go lowerIndex upperIndex allItems@(item : otherItems) vector = case item `compare` entry of
      LT -> go lowerIndex (currentIndex - 1) allItems vector
      EQ -> currentIndex : go (currentIndex + 1) (len - 1) otherItems vector
      GT -> go (currentIndex + 1) upperIndex allItems vector
      where
        entry = vector U.! currentIndex
        currentIndex = (upperIndex + lowerIndex) `div` 2

findFirstRoot :: Integer -> [Integer] -> Integer
findFirstRoot n rootsData = case someNatVal (integerToNatural n) of
  SomeNat (Proxy :: Proxy n) ->
    naturalToInteger . M.unMod $ foldr (\x acc -> (fromInteger x :: M.Mod n) * acc) (1 :: M.Mod n) rootsData

findSecondRoot :: Integer -> [IM.IntMap Int] -> Integer
findSecondRoot n factorisations = case someNatVal (integerToNatural n) of
  SomeNat (Proxy :: Proxy n) ->
    naturalToInteger . M.unMod $
      -- By contruction, the number obtained by adding the relevant factorisations
      -- in @rootsData@ is a square number. Its square root is computed by
      -- dividing each prime power by @2@.
      IM.foldrWithKey (\key power acc -> (fromIntegral key :: M.Mod n) ^ (power `div` 2 :: Int) * acc) (1 :: M.Mod n) $
        IM.unionsWith (+) factorisations
