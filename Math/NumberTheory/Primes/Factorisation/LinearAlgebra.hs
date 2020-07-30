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
  , linearSolve
  ) where

#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup
#endif
import qualified Data.List as L
import qualified Data.Vector.Sized as SV
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Sized as SU
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.Vector.Unboxed.Mutable.Sized as SMU
import Math.NumberTheory.Utils.FromIntegral
import Control.Monad.ST
import GHC.TypeNats (Nat, KnownNat, natVal)
import Data.Proxy
import System.Random
import Data.Foldable
import Data.Mod.Word
import Unsafe.Coerce
import Data.Maybe
import Data.Bit
import Data.Bits

-- | Sparse Binary Vector of size @k.
newtype SBVector (k :: Nat) = SBVector { unSBVector :: U.Vector (Mod k) }

-- | Dense Binary Vector of size @k@.
newtype DBVector (k :: Nat) = DBVector { unDBVector :: SU.Vector k Bit }
  deriving (Eq, Show)

-- | Sparse Binary square Matrix of size @k@. It is formed of columns
-- of sparse binary vectors.
newtype SBMatrix (k :: Nat) = SBMatrix { unSBMatrix :: SV.Vector k (SBVector k) }

-- | Addition of two dense Binary Vectors.
instance KnownNat k => Semigroup (DBVector k) where
  DBVector v1 <> DBVector v2 = DBVector $ SU.withVectorUnsafe (zipBits xor (SU.fromSized v1)) v2

-- | Dense Binary Vectors of given length form a group under addition.
instance KnownNat k => Monoid (DBVector k) where
  mempty = DBVector $ SU.replicate (Bit False)
  mappend = (<>)

listBits' :: KnownNat k => DBVector k -> [Mod k]
listBits' = unsafeCoerce listBits

flipBit' :: KnownNat k => SMU.MVector k s Bit -> Mod k -> ST s ()
flipBit' = unsafeCoerce (unsafeFlipBit :: MU.MVector s Bit -> Int -> ST s ())

index' :: KnownNat k => SBMatrix k -> Mod k -> SBVector k
index' = unsafeCoerce SV.index

-- | Dot product of two dense Binary Vectors of the same size.
dot :: KnownNat k => DBVector k -> DBVector k -> Bit
dot (DBVector v1) (DBVector v2) = Bit $ odd . countBits $ zipBits (.&.) (SU.fromSized v1) (SU.fromSized v2)

-- | Multiplication of a square matrix and a dense vector.
mult :: KnownNat k => SBMatrix k -> DBVector k -> DBVector k
mult matrix vector = runST $ do
  vs <- SMU.new
  traverse_ (U.mapM_ (flipBit' vs) . unSBVector . (matrix `index'`)) $ listBits' vector
  ws <- SU.unsafeFreeze vs
  pure $ DBVector ws

-- | It takes a random seed and a square singular matrix and it returns an
-- elemnent of its kernel. It does not check if the matrix is singular.
linearSolve :: KnownNat k => Int -> SBMatrix k -> DBVector k
linearSolve seed matrix = linearSolveHelper 1 matrix randomVectors 1
  where
    -- The floating point number is the density of the random vectors.
    randomVectors = getRandomDBVectors 0.4 $ mkStdGen seed

linearSolveHelper :: KnownNat k => F2Poly -> SBMatrix k -> [DBVector k] -> Int -> DBVector k
linearSolveHelper _ _ [] _ = error "Not enough random vectors"
linearSolveHelper _ _ [_] _ = error "Not enough random vectors"
linearSolveHelper previousPoly matrix (z : x : otherVecs) counter
  -- If the algorithm does not find a solution, try another random vector @x@.
  | potentialSolution == mempty && counter <= 5 = linearSolveHelper potentialMinPoly matrix (z : otherVecs) (counter + 1)
  -- If the algorithm does not find a solution after five iterations,
  -- most likely (>90%), it means that it picked a bad random vector @z@
  -- in the image of the matrix. This changes @z@.
  | potentialSolution == mempty && counter > 5  = linearSolveHelper 1 matrix otherVecs 1
  -- This is a good solution.
  | otherwise                                    = potentialSolution
  where
    potentialSolution = findSolution singularities matrix almostZeroVector
    almostZeroVector = evaluate matrix z reducedMinPoly
    (singularities, reducedMinPoly) = L.break unBit (U.toList $ unF2Poly potentialMinPoly)
    -- Information gathered from the previous random vector @x@ is used in
    -- the subsequent iteration.
    potentialMinPoly = lcm previousPoly candidateMinPoly
    candidateMinPoly = findCandidatePoly matrix z x

-- Infinite lists of random DBVectors.
getRandomDBVectors :: forall k. KnownNat k => Double -> StdGen -> [DBVector k]
getRandomDBVectors density gen = go $ randomRs (0, 1) gen
  where
    numberOfColumns = naturalToInt $ natVal (Proxy :: Proxy k)
    go :: KnownNat k => [Double] -> [DBVector k]
    go list = newVector `seq` newVector : go backOfList
      where
        newVector = DBVector $ fromJust $ SU.fromList $ map (\d -> Bit (d < density)) frontOfList
        (frontOfList, backOfList) = L.splitAt numberOfColumns list

-- The input is a matrix @B@ and random vectors @z@ and @x@. It returns the
-- sequence @x A ^ k z@ as k runs from 0 to the size of the matrix.
generateData :: KnownNat k => SBMatrix k -> DBVector k -> DBVector k -> F2Poly
generateData matrix z x = toF2Poly $ U.fromList $ reverse $ map (x `dot`) matrixPowers
  where
    matrixPowers = take (((*2) . naturalToInt . natVal) matrix) $ L.iterate (matrix `mult`) z

-- This routine finds the generating polynomial of the random sequence computed
-- in @generateData@.
berlekampMassey :: Int -> F2Poly -> F2Poly -> F2Poly
berlekampMassey dim = go 1 0
  where
    go :: F2Poly -> F2Poly -> F2Poly ->F2Poly -> F2Poly
    go oneBefore twoBefore a b
      | U.length (unF2Poly b) <= dim = oneBefore
      -- Updated value is given by @twoBefore - oneBefore * q@
      | otherwise                    = go (twoBefore - oneBefore * q) oneBefore b r
        where
          (q, r) = quotRem a b

findCandidatePoly :: KnownNat k => SBMatrix k -> DBVector k -> DBVector k -> F2Poly
findCandidatePoly matrix z x = berlekampMassey dim errorPoly randomSequence
  where
    randomSequence = generateData matrix z x
    errorPoly = fromInteger (1 `shiftL` (2 * dim)) :: F2Poly
    dim = naturalToInt $ natVal matrix

-- This routine takes a polynomial @p@, a @matrix@ and a vector @w@ and
-- returns @p(A)w@.
evaluate :: KnownNat k => SBMatrix k -> DBVector k -> [Bit] -> DBVector k
evaluate matrix w = foldr (\coeff acc -> (matrix `mult` acc) <> (if unBit coeff then w else mempty)) mempty

-- Tries to infer a solution. If it does not succeed, it returns an empty solution.
findSolution :: KnownNat k => [Bit] -> SBMatrix k -> DBVector k -> DBVector k
findSolution [] _ _ = mempty
findSolution (_ : xs) matrix vector
  | result == mempty = vector
  | otherwise        = findSolution xs matrix result
  where
    result = matrix `mult` vector
