-- |
-- Module:      Math.NumberTheory.Moduli.Multiplicative
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Multiplicative groups of integers modulo m.
--

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Math.NumberTheory.Moduli.Multiplicative
  ( -- * Multiplicative group
    MultMod
  , multElement
  , isMultElement
  , invertGroup
  -- * Primitive roots
  , PrimitiveRoot
  , unPrimitiveRoot
  , isPrimitiveRoot
  , discreteLogarithm
  ) where

import Control.Monad
import Data.Constraint
import Data.Mod
import Data.Semigroup
import GHC.TypeNats.Compat
import Numeric.Natural

import Math.NumberTheory.Moduli.Internal
import Math.NumberTheory.Moduli.Singleton
import Math.NumberTheory.Primes

-- | This type represents elements of the multiplicative group mod m, i.e.
-- those elements which are coprime to m. Use @toMultElement@ to construct.
newtype MultMod m = MultMod {
  multElement :: Mod m -- ^ Unwrap a residue.
  } deriving (Eq, Ord, Show)

instance KnownNat m => Semigroup (MultMod m) where
  MultMod a <> MultMod b = MultMod (a * b)
  stimes k a@(MultMod a')
    | k >= 0 = MultMod (a' ^% k)
    | otherwise = invertGroup $ stimes (-k) a
  -- ^ This Semigroup is in fact a group, so @stimes@ can be called with a negative first argument.

instance KnownNat m => Monoid (MultMod m) where
  mempty = MultMod 1
  mappend = (<>)

instance KnownNat m => Bounded (MultMod m) where
  minBound = MultMod 1
  maxBound = MultMod (-1)

-- | Attempt to construct a multiplicative group element.
isMultElement :: KnownNat m => Mod m -> Maybe (MultMod m)
isMultElement a = if unMod a `gcd` natVal a == 1
                     then Just $ MultMod a
                     else Nothing

-- | For elements of the multiplicative group, we can safely perform the inverse
-- without needing to worry about failure.
invertGroup :: KnownNat m => MultMod m -> MultMod m
invertGroup (MultMod a) = case invertMod a of
                            Just b -> MultMod b
                            Nothing -> error "Math.NumberTheory.Moduli.invertGroup: failed to invert element"

-- | 'PrimitiveRoot' m is a type which is only inhabited
-- by <https://en.wikipedia.org/wiki/Primitive_root_modulo_n primitive roots> of m.
newtype PrimitiveRoot m = PrimitiveRoot
  { unPrimitiveRoot :: MultMod m -- ^ Extract primitive root value.
  }
  deriving (Eq, Show)

-- | Check whether a given modular residue is
-- a <https://en.wikipedia.org/wiki/Primitive_root_modulo_n primitive root>.
--
-- >>> :set -XDataKinds
-- >>> import Data.Maybe
-- >>> isPrimitiveRoot (fromJust cyclicGroup) (1 :: Mod 13)
-- Nothing
-- >>> isPrimitiveRoot (fromJust cyclicGroup) (2 :: Mod 13)
-- Just (PrimitiveRoot {unPrimitiveRoot = MultMod {multElement = (2 `modulo` 13)}})
isPrimitiveRoot
  :: (Integral a, UniqueFactorisation a)
  => CyclicGroup a m
  -> Mod m
  -> Maybe (PrimitiveRoot m)
isPrimitiveRoot cg r = case proofFromCyclicGroup cg of
  Sub Dict -> do
    r' <- isMultElement r
    guard $ isPrimitiveRoot' cg (fromIntegral (unMod r))
    return $ PrimitiveRoot r'

-- | Computes the discrete logarithm. Currently uses a combination of the baby-step
-- giant-step method and Pollard's rho algorithm, with Bach reduction.
--
-- >>> :set -XDataKinds
-- >>> import Data.Maybe
-- >>> let cg = fromJust cyclicGroup :: CyclicGroup Integer 13
-- >>> let rt = fromJust (isPrimitiveRoot cg 2)
-- >>> let x  = fromJust (isMultElement 11)
-- >>> discreteLogarithm cg rt x
-- 7
discreteLogarithm :: CyclicGroup Integer m -> PrimitiveRoot m -> MultMod m -> Natural
discreteLogarithm cg (multElement . unPrimitiveRoot -> a) (multElement -> b) = case cg of
  CG2
    -> 0
    -- the only valid input was a=1, b=1
  CG4
    -> if unMod b == 1 then 0 else 1
    -- the only possible input here is a=3 with b = 1 or 3
  CGOddPrimePower (unPrime -> p) k
    -> discreteLogarithmPP p k (toInteger (unMod a)) (toInteger (unMod b))
  CGDoubleOddPrimePower (unPrime -> p) k
    -> discreteLogarithmPP p k (toInteger (unMod a) `rem` p^k) (toInteger (unMod b) `rem` p^k)
    -- we have the isomorphism t -> t `rem` p^k from (Z/2p^kZ)* -> (Z/p^kZ)*
