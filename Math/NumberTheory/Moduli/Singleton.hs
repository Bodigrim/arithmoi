-- |
-- Module:      Math.NumberTheory.Moduli.Singleton
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Singleton data types.
--

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

module Math.NumberTheory.Moduli.Singleton
  ( -- * SFactors singleton
    SFactors
  , sfactors
  , someSFactors
  , unSFactors
  , proofFromSFactors
    -- * CyclicGroup singleton
  , CyclicGroup
  , cyclicGroup
  , cyclicGroupFromFactors
  , cyclicGroupFromModulo
  , proofFromCyclicGroup
  , pattern CG2
  , pattern CG4
  , pattern CGOddPrimePower
  , pattern CGDoubleOddPrimePower
    -- * SFactors \<=\> CyclicGroup
  , cyclicGroupToSFactors
  , sfactorsToCyclicGroup
    -- * Some wrapper
  , Some(..)
  ) where

import Control.DeepSeq
import Data.Constraint
import Data.Kind
import Data.List (sort)
import qualified Data.Map as M
import Data.Proxy
#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup
#endif
import GHC.Generics
import GHC.TypeNats (KnownNat, Nat, natVal)
import Numeric.Natural
import Unsafe.Coerce

import Math.NumberTheory.Roots (highestPower)
import Math.NumberTheory.Primes
import Math.NumberTheory.Primes.Types

-- | Wrapper to hide an unknown type-level natural.
data Some (a :: Nat -> Type) where
  Some :: a m -> Some a

-- | From "Data.Constraint.Nat".
newtype Magic n = Magic (KnownNat n => Dict (KnownNat n))

-- | This singleton data type establishes a correspondence
-- between a modulo @m@ on type level
-- and its factorisation on term level.
newtype SFactors a (m :: Nat) = SFactors
  { unSFactors :: [(Prime a, Word)]
  -- ^ Factors of @m@.
  } deriving (Show, Generic)

instance Eq (SFactors a m) where
  _ == _ = True

instance Ord (SFactors a m) where
  _ `compare` _ = EQ

instance NFData a => NFData (SFactors a m)

instance Ord a => Eq (Some (SFactors a)) where
  Some (SFactors xs) == Some (SFactors ys) =
    xs == ys

instance Ord a => Ord (Some (SFactors a)) where
  Some (SFactors xs) `compare` Some (SFactors ys) =
    xs `compare` ys

instance Show a => Show (Some (SFactors a)) where
  showsPrec p (Some x) = showsPrec p x

instance NFData a => NFData (Some (SFactors a)) where
  rnf (Some x) = rnf x

-- | Create a singleton from a type-level positive modulo @m@,
-- passed in a constraint.
--
-- >>> :set -XDataKinds
-- >>> sfactors :: SFactors Integer 13
-- SFactors {sfactorsFactors = [(Prime 13,1)]}
sfactors :: forall a m. (Ord a, UniqueFactorisation a, KnownNat m) => SFactors a m
sfactors = if m == 0
  then error "sfactors: modulo must be positive"
  else SFactors (sort (factorise m))
  where
    m = fromIntegral (natVal (Proxy :: Proxy m))

-- | Create a singleton from factors of @m@.
-- Factors must be distinct, as in output of 'factorise'.
--
-- >>> import Math.NumberTheory.Primes
-- >>> someSFactors (factorise 98)
-- SFactors {sfactorsFactors = [(Prime 2,1),(Prime 7,2)]}
someSFactors :: (Ord a, Num a) => [(Prime a, Word)] -> Some (SFactors a)
someSFactors
  = Some
  . SFactors
  -- Just a precaution against ill-formed lists of factors
  . M.assocs
  . M.fromListWith (+)

-- | Convert a singleton to a proof that @m@ is known. Usage example:
--
-- > toModulo :: SFactors Integer m -> Natural
-- > toModulo t = case proofFromSFactors t of Sub Dict -> natVal t
proofFromSFactors :: Integral a => SFactors a m -> (() :- KnownNat m)
proofFromSFactors (SFactors fs) = Sub $ unsafeCoerce (Magic Dict) (fromIntegral (factorBack fs) :: Natural)

-- | This singleton data type establishes a correspondence
-- between a modulo @m@ on type level
-- and a cyclic group of the same order on term level.
data CyclicGroup a (m :: Nat)
  = CG2' -- ^ Residues modulo 2.
  | CG4' -- ^ Residues modulo 4.
  | CGOddPrimePower'       (Prime a) Word
  -- ^ Residues modulo @p@^@k@ for __odd__ prime @p@.
  | CGDoubleOddPrimePower' (Prime a) Word
  -- ^ Residues modulo 2@p@^@k@ for __odd__ prime @p@.
  deriving (Show, Generic)

instance Eq (CyclicGroup a m) where
  _ == _ = True

instance Ord (CyclicGroup a m) where
  _ `compare` _ = EQ

instance NFData a => NFData (CyclicGroup a m)

instance Eq a => Eq (Some (CyclicGroup a)) where
  Some CG2' == Some CG2' = True
  Some CG4' == Some CG4' = True
  Some (CGOddPrimePower' p1 k1) == Some (CGOddPrimePower' p2 k2) =
    p1 == p2 && k1 == k2
  Some (CGDoubleOddPrimePower' p1 k1) == Some (CGDoubleOddPrimePower' p2 k2) =
    p1 == p2 && k1 == k2
  _ == _ = False

instance Ord a => Ord (Some (CyclicGroup a)) where
  compare (Some x) (Some y) = case x of
    CG2' -> case y of
      CG2' -> EQ
      _    -> LT
    CG4' -> case y of
      CG2' -> GT
      CG4' -> EQ
      _    -> LT
    CGOddPrimePower' p1 k1 -> case y of
      CGDoubleOddPrimePower'{} -> LT
      CGOddPrimePower' p2 k2 ->
        p1 `compare` p2 <> k1 `compare` k2
      _ -> GT
    CGDoubleOddPrimePower' p1 k1 -> case y of
      CGDoubleOddPrimePower' p2 k2 ->
        p1 `compare` p2 <> k1 `compare` k2
      _ -> GT

instance Show a => Show (Some (CyclicGroup a)) where
  showsPrec p (Some x) = showsPrec p x

instance NFData a => NFData (Some (CyclicGroup a)) where
  rnf (Some x) = rnf x

-- | Create a singleton from a type-level positive modulo @m@,
-- passed in a constraint.
--
-- >>> :set -XDataKinds
-- >>> import Data.Maybe
-- >>> cyclicGroup :: CyclicGroup Integer 169
-- CGOddPrimePower' (Prime 13) 2
--
-- >>> sfactorsToCyclicGroup (fromModulo 4)
-- Just CG4'
-- >>> sfactorsToCyclicGroup (fromModulo (2 * 13 ^ 3))
-- Just (CGDoubleOddPrimePower' (Prime 13) 3)
-- >>> sfactorsToCyclicGroup (fromModulo (4 * 13))
-- Nothing
cyclicGroup
  :: forall a m.
     (Integral a, UniqueFactorisation a, KnownNat m)
  => Maybe (CyclicGroup a m)
cyclicGroup = fromModuloInternal m
  where
    m = fromIntegral (natVal (Proxy :: Proxy m))

-- | Create a singleton from factors.
-- Factors must be distinct, as in output of 'factorise'.
cyclicGroupFromFactors
  :: (Eq a, Num a)
  => [(Prime a, Word)]
  -> Maybe (Some (CyclicGroup a))
cyclicGroupFromFactors = \case
  [(unPrime -> 2, 1)] -> Just $ Some CG2'
  [(unPrime -> 2, 2)] -> Just $ Some CG4'
  [(unPrime -> 2, _)] -> Nothing
  [(p, k)] -> Just $ Some $ CGOddPrimePower' p k
  [(unPrime -> 2, 1), (p, k)] -> Just $ Some $ CGDoubleOddPrimePower' p k
  [(p, k), (unPrime -> 2, 1)] -> Just $ Some $ CGDoubleOddPrimePower' p k
  _ -> Nothing

-- | Similar to 'cyclicGroupFromFactors' . 'factorise',
-- but much faster, because it
-- but performes only one primality test instead of full
-- factorisation.
cyclicGroupFromModulo
  :: (Integral a, UniqueFactorisation a)
  => a
  -> Maybe (Some (CyclicGroup a))
cyclicGroupFromModulo = fmap Some . fromModuloInternal

fromModuloInternal
  :: (Integral a, UniqueFactorisation a)
  => a
  -> Maybe (CyclicGroup a m)
fromModuloInternal = \case
  2 -> Just CG2'
  4 -> Just CG4'
  n
    | even n -> uncurry CGDoubleOddPrimePower' <$> isOddPrimePower (n `div` 2)
    | otherwise -> uncurry CGOddPrimePower' <$> isOddPrimePower n

isOddPrimePower
  :: (Integral a, UniqueFactorisation a)
  => a
  -> Maybe (Prime a, Word)
isOddPrimePower n
  | even n    = Nothing
  | otherwise = (, k) <$> isPrime p
  where
    (p, k) = highestPower n

-- | Convert a cyclic group to a proof that @m@ is known. Usage example:
--
-- > toModulo :: CyclicGroup Integer m -> Natural
-- > toModulo t = case proofFromCyclicGroup t of Sub Dict -> natVal t
proofFromCyclicGroup :: Integral a => CyclicGroup a m -> (() :- KnownNat m)
proofFromCyclicGroup = proofFromSFactors . cyclicGroupToSFactors

-- | Check whether a multiplicative group of residues,
-- characterized by its modulo, is cyclic and, if yes, return its form.
--
-- >>> sfactorsToCyclicGroup (fromModulo 4)
-- Just CG4'
-- >>> sfactorsToCyclicGroup (fromModulo (2 * 13 ^ 3))
-- Just (CGDoubleOddPrimePower' (Prime 13) 3)
-- >>> sfactorsToCyclicGroup (fromModulo (4 * 13))
-- Nothing
sfactorsToCyclicGroup :: (Eq a, Num a) => SFactors a m -> Maybe (CyclicGroup a m)
sfactorsToCyclicGroup (SFactors fs) = case fs of
  [(unPrime -> 2, 1)]         -> Just CG2'
  [(unPrime -> 2, 2)]         -> Just CG4'
  [(unPrime -> 2, _)]         -> Nothing
  [(p, k)]                    -> Just $ CGOddPrimePower' p k
  [(p, k), (unPrime -> 2, 1)] -> Just $ CGDoubleOddPrimePower' p k
  [(unPrime -> 2, 1), (p, k)] -> Just $ CGDoubleOddPrimePower' p k
  _ -> Nothing

-- | Invert 'sfactorsToCyclicGroup'.
--
-- >>> import Data.Maybe
-- >>> cyclicGroupToSFactors (fromJust (sfactorsToCyclicGroup (fromModulo 4)))
-- SFactors {sfactorsModulo = 4, sfactorsFactors = [(Prime 2,2)]}
cyclicGroupToSFactors :: Num a => CyclicGroup a m -> SFactors a m
cyclicGroupToSFactors = SFactors . \case
  CG2' -> [(Prime 2, 1)]
  CG4' -> [(Prime 2, 2)]
  CGOddPrimePower' p k -> [(p, k)]
  CGDoubleOddPrimePower' p k -> [(Prime 2, 1), (p, k)]

-- | Unidirectional pattern for residues modulo 2.
pattern CG2 :: CyclicGroup a m
pattern CG2 <- CG2'

-- | Unidirectional pattern for residues modulo 4.
pattern CG4 :: CyclicGroup a m
pattern CG4 <- CG4'

-- | Unidirectional pattern for residues modulo @p@^@k@ for __odd__ prime @p@.
pattern CGOddPrimePower :: Prime a -> Word -> CyclicGroup a m
pattern CGOddPrimePower p k <- CGOddPrimePower' p k

-- | Unidirectional pattern for residues modulo 2@p@^@k@ for __odd__ prime @p@.
pattern CGDoubleOddPrimePower :: Prime a -> Word -> CyclicGroup a m
pattern CGDoubleOddPrimePower p k <- CGDoubleOddPrimePower' p k

#if __GLASGOW_HASKELL__ > 801
{-# COMPLETE CG2, CG4, CGOddPrimePower, CGDoubleOddPrimePower #-}
#endif
