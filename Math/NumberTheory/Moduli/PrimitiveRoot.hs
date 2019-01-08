-- |
-- Module:      Math.NumberTheory.Moduli.PrimitiveRoot
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Primitive roots and cyclic groups.
--

{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TupleSections        #-}

module Math.NumberTheory.Moduli.PrimitiveRoot
  ( -- * Cyclic groups
    CyclicGroup(..)
  , cyclicGroupFromModulo
  , cyclicGroupToModulo
  , groupSize
    -- * Primitive roots
  , PrimitiveRoot
  , unPrimitiveRoot
  , getGroup
  , isPrimitiveRoot
  , isPrimitiveRoot'
  ) where

#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup
#endif

import Math.NumberTheory.ArithmeticFunctions (totient)
import qualified Math.NumberTheory.Euclidean as E
import Math.NumberTheory.Euclidean.Coprimes as Coprimes (singleton)
import Math.NumberTheory.Moduli.Class (getNatMod, getNatVal, KnownNat, Mod, MultMod, isMultElement, multElement)
import Math.NumberTheory.Powers.General (highestPower)
import Math.NumberTheory.Powers.Modular
import Math.NumberTheory.Prefactored
import Math.NumberTheory.UniqueFactorisation

import Control.DeepSeq
import Control.Monad (guard)
import GHC.Generics
import Numeric.Natural

import Data.Proxy
import GHC.TypeNats.Compat

-- | A multiplicative group of residues is called cyclic,
-- if there is a primitive root @g@,
-- whose powers generates all elements.
-- Any cyclic multiplicative group of residues
-- falls into one of the following cases.
data CyclicGroup a
  = CG2 -- ^ Residues modulo 2.
  | CG4 -- ^ Residues modulo 4.
  | CGOddPrimePower       (Prime a) Word
  -- ^ Residues modulo @p@^@k@ for __odd__ prime @p@.
  | CGDoubleOddPrimePower (Prime a) Word
  -- ^ Residues modulo 2@p@^@k@ for __odd__ prime @p@.
  deriving (Eq, Show, Generic)

data CyclicGroup' (n :: Nat)
  = CG2'
  | CG4'
  | CGOddPrimePower'       (Prime Natural) Word
  | CGDoubleOddPrimePower' (Prime Natural) Word
  deriving (Eq, Show, Generic)

instance NFData a => NFData (CyclicGroup a)
instance NFData (CyclicGroup' n)

-- | Check whether a multiplicative group of residues,
-- characterized by its modulo, is cyclic and, if yes, return its form.
--
-- >>> cyclicGroupFromModulo 4
-- Just CG4
-- >>> cyclicGroupFromModulo (2 * 13 ^ 3)
-- Just (CGDoubleOddPrimePower (PrimeNat 13) 3)
-- >>> cyclicGroupFromModulo (4 * 13)
-- Nothing
cyclicGroupFromModulo
  :: (Ord a, Integral a, UniqueFactorisation a)
  => a
  -> Maybe (CyclicGroup a)
cyclicGroupFromModulo = \case
  2 -> Just CG2
  4 -> Just CG4
  n
    | n <= 1    -> Nothing
    | odd n     -> uncurry CGOddPrimePower       <$> isPrimePower n
    | odd halfN -> uncurry CGDoubleOddPrimePower <$> isPrimePower halfN
    | otherwise -> Nothing
    where
      halfN = n `quot` 2

cyclicGroup'FromModulo :: forall n. KnownNat n => Maybe (CyclicGroup' n)
cyclicGroup'FromModulo = case natVal (Proxy :: Proxy n) of
                           2 -> Just CG2'
                           4 -> Just CG4'
                           n | n <= 1    -> Nothing
                             | odd n     -> uncurry CGOddPrimePower' <$> isPrimePower n
                             | odd halfN -> uncurry CGDoubleOddPrimePower' <$> isPrimePower halfN
                             | otherwise -> Nothing
                             where halfN = n `quot` 2

isPrimePower
  :: (Integral a, UniqueFactorisation a)
  => a
  -> Maybe (Prime a, Word)
isPrimePower n = (, k) <$> isPrime m
  where
    (m, k) = highestPower n

-- | Extract modulo and its factorisation from
-- a cyclic multiplicative group of residues.
--
-- >>> cyclicGroupToModulo CG4
-- Prefactored {prefValue = 4, prefFactors = Coprimes {unCoprimes = fromList [(2,2)]}}
--
-- >>> :set -XTypeFamilies
-- >>> cyclicGroupToModulo (CGDoubleOddPrimePower (PrimeNat 13) 3)
-- Prefactored {prefValue = 4394, prefFactors = Coprimes {unCoprimes = fromList [(2,1),(13,3)]}}
cyclicGroupToModulo
  :: (E.Euclidean a, Ord a, UniqueFactorisation a)
  => CyclicGroup a
  -> Prefactored a
cyclicGroupToModulo = fromFactors . \case
  CG2                       -> Coprimes.singleton 2 1
  CG4                       -> Coprimes.singleton 2 2
  CGOddPrimePower p k       -> Coprimes.singleton (unPrime p) k
  CGDoubleOddPrimePower p k -> Coprimes.singleton 2 1
                            <> Coprimes.singleton (unPrime p) k

-- | 'PrimitiveRoot' m is a type which is only inhabited
-- by <https://en.wikipedia.org/wiki/Primitive_root_modulo_n primitive roots> of m.
data PrimitiveRoot m = PrimitiveRoot
  { unPrimitiveRoot :: MultMod m -- ^ Extract primitive root value.
  , getGroup        :: CyclicGroup Natural -- ^ Get cyclic group structure.
  }
  deriving (Eq, Show)

data PrimitiveRoot' m = PrimitiveRoot'
  { unPrimitiveRoot' :: MultMod m -- ^ Extract primitive root value.
  , getGroup'        :: CyclicGroup' m -- ^ Get cyclic group structure.
  }
  deriving (Eq, Show)

-- | 'isPrimitiveRoot'' @cg@ @a@ checks whether @a@ is
-- a <https://en.wikipedia.org/wiki/Primitive_root_modulo_n primitive root>
-- of a given cyclic multiplicative group of residues @cg@.
--
-- >>> let Just cg = cyclicGroupFromModulo 13
-- >>> isPrimitiveRoot' cg 1
-- False
-- >>> isPrimitiveRoot' cg 2
-- True
isPrimitiveRoot'
  :: (Integral a, UniqueFactorisation a)
  => CyclicGroup a
  -> a
  -> Bool
-- https://en.wikipedia.org/wiki/Primitive_root_modulo_n#Finding_primitive_roots
isPrimitiveRoot' cg r =
  case cg of
    CG2                       -> r == 1
    CG4                       -> r == 3
    CGOddPrimePower p k       -> oddPrimePowerTest (unPrime p) k r
    CGDoubleOddPrimePower p k -> doubleOddPrimePowerTest (unPrime p) k r
  where
    oddPrimeTest p g              = let phi  = totient p
                                        pows = map (\pk -> phi `quot` unPrime (fst pk)) (factorise phi)
                                        exps = map (\x -> powMod g x p) pows
                                     in g /= 0 && gcd g p == 1 && all (/= 1) exps
    oddPrimePowerTest p 1 g       = oddPrimeTest p (g `mod` p)
    oddPrimePowerTest p _ g       = oddPrimeTest p (g `mod` p) && powMod g (p-1) (p*p) /= 1
    doubleOddPrimePowerTest p k g = odd g && oddPrimePowerTest p k g

isPrimitiveRoot'2 :: CyclicGroup' n -> MultMod n -> Maybe (PrimitiveRoot' n)
isPrimitiveRoot'2 cg r = if test
                            then Just (PrimitiveRoot' r cg)
                            else Nothing
  where test = case cg of
                 CG2' -> r' == 1
                 CG4' -> r' == 3
                 CGOddPrimePower' p k -> oddPrimePowerTest (unPrime p) k r'
                 CGDoubleOddPrimePower' p k -> doubleOddPrimePowerTest (unPrime p) k r'
        r' = getNatVal $ multElement r
        oddPrimeTest p g              = let phi  = totient p
                                            pows = map (\pk -> phi `quot` unPrime (fst pk)) (factorise phi)
                                            exps = map (\x -> powMod g x p) pows
                                         in g /= 0 && gcd g p == 1 && all (/= 1) exps
        oddPrimePowerTest p 1 g       = oddPrimeTest p (g `mod` p)
        oddPrimePowerTest p _ g       = oddPrimeTest p (g `mod` p) && powMod g (p-1) (p*p) /= 1
        doubleOddPrimePowerTest p k g = odd g && oddPrimePowerTest p k g

isPrimitiveRoot2 :: KnownNat n => Mod n -> Maybe (PrimitiveRoot' n)
isPrimitiveRoot2 r = do
  r' <- isMultElement r
  cg <- cyclicGroup'FromModulo
  isPrimitiveRoot'2 cg r'

-- | Check whether a given modular residue is
-- a <https://en.wikipedia.org/wiki/Primitive_root_modulo_n primitive root>.
--
-- >>> :set -XDataKinds
-- >>> isPrimitiveRoot (1 :: Mod 13)
-- False
-- >>> isPrimitiveRoot (2 :: Mod 13)
-- True
--
-- Here is how to list all primitive roots:
--
-- >>> mapMaybe isPrimitiveRoot [minBound .. maxBound] :: [Mod 13]
-- [(2 `modulo` 13), (6 `modulo` 13), (7 `modulo` 13), (11 `modulo` 13)]
--
-- This function is a convenient wrapper around 'isPrimitiveRoot''. The latter
-- provides better control and performance, if you need them.
isPrimitiveRoot
  :: KnownNat n
  => Mod n
  -> Maybe (PrimitiveRoot n)
isPrimitiveRoot r = do
  r' <- isMultElement r
  cg <- cyclicGroupFromModulo (getNatMod r)
  guard $ isPrimitiveRoot' cg (getNatVal r)
  return $ PrimitiveRoot r' cg

-- | Calculate the size of a given cyclic group.
groupSize :: (E.Euclidean a, Ord a, UniqueFactorisation a) => CyclicGroup a -> Prefactored a
groupSize = totient . cyclicGroupToModulo
