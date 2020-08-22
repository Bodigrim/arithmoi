-- |
-- Module:      Math.NumberTheory.Primes.Factorisation.LinearAlgebra
-- Copyright:   (c) 2020 Federico Bongiorno
-- Licence:     MIT
-- Maintainer:  Federico Bongiorno <federicobongiorno97@gmail.com>
--
-- <http://www.enseignement.polytechnique.fr/profs/informatique/Francois.Morain/Master1/Crypto/projects/Wiedemann86.pdf Weidemann algorithm>
-- to find a non-zero element in the kernel of a singular sparse binary matrix.

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.Primes.Factorisation.LinearAlgebra
  ( SBVector(..)
  , DBVector(..)
  , SBMatrix(..)
  , dot
  , mult
  , fromList
  , convertToList
  , linearSolve
  ) where

#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup
#endif
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Sized as SV
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Sized as SU
import qualified Data.Vector.Unboxed.Mutable.Sized as SMU
import qualified Data.Vector.Generic.Sized.Internal as GSI
import qualified Data.Vector.Generic.Mutable.Sized.Internal as GMSI
import qualified Data.Mod.Word as MW
import Math.NumberTheory.Utils
import Math.NumberTheory.Utils.FromIntegral
import Control.Monad.ST
import GHC.TypeNats
import Data.Proxy
import System.Random
import Data.Foldable
import Data.Maybe
import Data.Bit
import Data.Bits

-- | Sparse Binary Vector of size @k@.
newtype SBVector (k :: Nat) = SBVector { unSBVector :: U.Vector (MW.Mod k) }

-- | Dense Binary Vector of size @k@.
newtype DBVector (k :: Nat) = DBVector { unDBVector :: SU.Vector k Bit }
  deriving (Eq, Ord, Show)

-- | Sparse Binary square Matrix of size @k@. It is formed of columns
-- of sparse binary vectors.
newtype SBMatrix (k :: Nat) = SBMatrix { unSBMatrix :: SV.Vector k (SBVector k) }
  deriving (Show)

instance KnownNat k => Show (SBVector k) where
  show (SBVector sbVec) = show $ U.map MW.unMod sbVec

-- | Addition of two dense Binary Vectors.
instance KnownNat k => Semigroup (DBVector k) where
  DBVector v1 <> DBVector v2 = DBVector $ SU.withVectorUnsafe (zipBits xor (SU.fromSized v1)) v2

-- | Dense Binary Vectors of given length form a group under addition.
instance KnownNat k => Monoid (DBVector k) where
  mempty = DBVector $ SU.replicate (Bit False)
  mappend = (<>)

-- | Dot product of two dense Binary Vectors of the same size.
dot :: KnownNat k => DBVector k -> DBVector k -> Bit
dot (DBVector v1) (DBVector v2) = Bit $ odd . countBits $ zipBits (.&.) (SU.fromSized v1) (SU.fromSized v2)

-- | Multiplication of a sparse binary matrix and a dense binary vector.
mult :: KnownNat k => SBMatrix k -> DBVector k -> DBVector k
mult matrix vector = runST $ do
  vs <- SMU.new
  traverse_ (U.mapM_ (flipBit' vs . wordToInt . MW.unMod) . unSBVector . (matrix `index'`)) $ listBits' vector
  ws <- SU.unsafeFreeze vs
  pure $ DBVector ws

-- | Converts a list of lists of Ints into a @SomeKnown SBMatrix@. It is interpreted
-- as a list of columns. Each column is a list of Ints corresponding to the position
-- of the non-zero entries of the column.
fromList :: [[Int]] -> SomeKnown SBMatrix
fromList columns = case someNatVal (fromIntegral (length columns)) of
  SomeNat (Proxy :: Proxy k) -> SomeKnown (mat :: SBMatrix k)
  where
    mat :: KnownNat k => SBMatrix k
    mat = SBMatrix $ fromJust . SV.fromList $ listOfVectors
    listOfVectors :: KnownNat k => [SBVector k]
    listOfVectors = map toSBVector columns
    toSBVector :: KnownNat k => [Int] -> SBVector k
    toSBVector vec = SBVector $ U.fromList $ map fromIntegral vec

-- | Converts a DBVector to a list of ints. Each int corresponds to the position
-- of the non-zero entries of the vector.
convertToList :: KnownNat k => DBVector k -> [Int]
convertToList = listBits . SU.fromSized . unDBVector

-- These are helper functions used in @mult@.
listBits' :: KnownNat k => DBVector k -> [Int]
listBits' (DBVector (GSI.Vector v)) = listBits v

flipBit' :: KnownNat k => SMU.MVector k s Bit -> Int -> ST s ()
flipBit' (GMSI.MVector v) = unsafeFlipBit v

index' :: KnownNat k => SBMatrix k -> Int -> SBVector k
index' (SBMatrix (GSI.Vector v)) = V.unsafeIndex v

-- | It takes a random seed and a square singular sparse binary matrix and it returns
-- a non-zero elemnent of its kernel. It does not check if the matrix is singular.
--
-- >>> import qualified Math.NumberTheory.Primes.Factorisation.LinearAlgebra as LA
-- >>> mat = LA.fromList [[1,2],[2,3],[1,3]]
-- >>> withSomeKnown (convertToList . linearSolve 0) mat
-- [0,1,2]
linearSolve :: KnownNat k => Int -> SBMatrix k -> DBVector k
linearSolve seed matrix = linearSolveHelper 1 matrix randomVectors 1
  where
    -- The floating point number is the density of the random vectors.
    randomVectors = getRandomDBVectors 0.5 $ mkStdGen seed

-- The idea of the algorithm is to estimate the minimal polynomial of the matrix
-- and to infer a non-zero solution from there. Indeed the miniaml polynomial of
-- a singular matrix in the variable @t@ is divisble by a power of @t@. Dividing
-- this the highest power of @t@ and applying the polynomial to a random vector
-- yields @almostZeroVector@. This vector is non-zero and repeatedly applying
-- @matrix@ yields zero. This must give a non-zero solution.
linearSolveHelper :: KnownNat k => F2Poly -> SBMatrix k -> [DBVector k] -> Int -> DBVector k
linearSolveHelper _ _ [] _ = error "Math.NumberTheory.Primes.Factorisation.LinearAlgebra: Not enough random vectors"
linearSolveHelper _ _ [_] _ = error "Math.NumberTheory.Primes.Factorisation.LinearAlgebra: Not enough random vectors"
linearSolveHelper previousPoly matrix (z : x : otherVecs) counter = case potentialSolution of
  -- This is a good solution.
  Just solution -> solution
  Nothing
    -- If the algorithm does not find a solution, it tries another random vector @x@.
    | counter <= 5 -> linearSolveHelper potentialMinPoly matrix (z : otherVecs) (counter + 1)
    -- If the algorithm does not find a solution after five iterations, then
    -- most likely (>90%), it means that a bad random vector @z@ was chosen at
    -- the start. More precisely @z@ is in the image of the matrix.
    -- In this case, change @z@.
    | otherwise    -> linearSolveHelper 1 matrix otherVecs 1
  where
    potentialSolution = findSolution countOfSingularities matrix almostZeroVector
    almostZeroVector = evaluate matrix z $ toF2Poly $ U.drop countOfSingularities vectorMinPoly
    -- This number is the largest int @i@ such that @t ^ i@ divided the potential
    -- minimal polynomial
    countOfSingularities = fromMaybe (U.length vectorMinPoly) $ bitIndex (Bit True) vectorMinPoly
    vectorMinPoly = unF2Poly potentialMinPoly
    -- Information gathered from the previous random vector @x@ is used in
    -- the subsequent iteration.
    potentialMinPoly = lcm previousPoly candidateMinPoly
    candidateMinPoly = findCandidatePoly matrix z x

-- Infinite lists of random DBVectors.
getRandomDBVectors :: forall k. KnownNat k => Double -> StdGen -> [DBVector k]
getRandomDBVectors density gen = go $ randomRs (0, 1) gen
  where
    numberOfColumns = intVal (Proxy :: Proxy k)
    go :: KnownNat k => [Double] -> [DBVector k]
    go list = newVector `seq` newVector : go backOfList
      where
        newVector = DBVector $ fromJust $ SU.fromList $ map (\d -> Bit (d < density)) frontOfList
        (frontOfList, backOfList) = L.splitAt numberOfColumns list

-- The input is a @matrix@ and random vectors @z@ and @x@. It returns the
-- sequence @x A ^ k z@ as k runs from 0 to the size of the matrix.
generateData :: KnownNat k => SBMatrix k -> DBVector k -> DBVector k -> F2Poly
generateData matrix z x = toF2Poly $ U.fromList $ reverse $ map (x `dot`) matrixPowers
  where
    matrixPowers = take (((*2) . intVal) matrix) $ L.iterate (matrix `mult`) z

-- This routine estimates the minimal polynomial of @matrix@ from the generating
-- polynomial of the random sequence.
findCandidatePoly :: KnownNat k => SBMatrix k -> DBVector k -> DBVector k -> F2Poly
findCandidatePoly matrix z x = berlekampMassey dim errorPoly randomSequence
  where
    randomSequence = generateData matrix z x
    errorPoly = fromInteger (1 `shiftL` (2 * dim)) :: F2Poly
    dim = intVal matrix

-- This routine finds the generating polynomial of the random sequence computed
-- in @generateData@.
berlekampMassey :: Int -> F2Poly -> F2Poly -> F2Poly
berlekampMassey dim = go 1 0
  where
    go :: F2Poly -> F2Poly -> F2Poly ->F2Poly -> F2Poly
    go oneBefore twoBefore a b
      | U.length (unF2Poly b) <= dim = oneBefore
      | otherwise                    = go (twoBefore - oneBefore * q) oneBefore b r
        where
          (q, r) = quotRem a b

-- This routine takes a polynomial @poly@, a @matrix@ and a vector @w@ and
-- returns @p(A)w@.
evaluate :: KnownNat k => SBMatrix k -> DBVector k -> F2Poly -> DBVector k
evaluate matrix w poly = U.foldr (\coeff acc -> (matrix `mult` acc) <> (if unBit coeff then w else mempty)) mempty $ unF2Poly poly

-- This routine maybe finds a solution. If it does not succeed, it returns Nothing.
findSolution :: KnownNat k => Int -> SBMatrix k -> DBVector k -> Maybe (DBVector k)
findSolution len matrix vector = fst <$> find ((== mempty) . snd) (zip vectors (tail vectors))
  where
    vectors = take (len + 1) $ L.iterate (matrix `mult`) vector
