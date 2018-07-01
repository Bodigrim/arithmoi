-- |
-- Module:      Math.NumberTheory.Moduli.PrimitiveRoot
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Primitive roots and cyclic groups.
--

{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE UndecidableInstances #-}

module Math.NumberTheory.Moduli.PrimitiveRoot
  ( PrimitiveRoot
  , isPrimitiveRoot
  , primitiveRootGetMod
    -- * Cyclic groups
  , CyclicGroup(..)
  , cyclicGroupFromModulo
  , cyclicGroupToModulo
  , isPrimitiveRoot'
  ) where

import Math.NumberTheory.ArithmeticFunctions (totient)
import Math.NumberTheory.Moduli (Mod, getNatMod, getNatVal, KnownNat)
import Math.NumberTheory.Powers.General (highestPower)
import Math.NumberTheory.Powers.Modular
import Math.NumberTheory.Prefactored
import Math.NumberTheory.UniqueFactorisation
import Math.NumberTheory.Utils.FromIntegral

import Control.Monad (guard)

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

deriving instance Show (Prime a) => Show (CyclicGroup a)

-- | Check whether a multiplicative group of residues,
-- characterized by its modulo, is cyclic and, if yes, return its form.
--
-- > > cyclicGroupFromModulo 4
-- > Just CG4
-- > > cyclicGroupFromModulo (2 * 13 ^ 3)
-- > Just (CGDoubleOddPrimePower (PrimeNat 13) 3)
-- > > cyclicGroupFromModulo (4 * 13)
-- > Nothing
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

isPrimePower
  :: (Integral a, UniqueFactorisation a)
  => a
  -> Maybe (Prime a, Word)
isPrimePower n = (, intToWord k) <$> isPrime m
  where
    (m, k) = highestPower n

-- | Extract modulo and its factorisation from
-- a cyclic multiplicative group of residues.
--
-- > > cyclicGroupToModulo CG4
-- > Prefactored {prefValue = 4, prefFactors = [(2, 2)]}
-- > > cyclicGroupToModulo (CGDoubleOddPrimePower (PrimeNat 13) 3)
-- > Prefactored {prefValue = 4394, prefFactors = [(2, 1), (13, 3)]}
cyclicGroupToModulo
  :: (Integral a, UniqueFactorisation a)
  => CyclicGroup a
  -> Prefactored a
cyclicGroupToModulo = fromFactors . \case
  CG2                       -> [(2, 1)]
  CG4                       -> [(2, 2)]
  CGOddPrimePower p k       -> [(unPrime p, k)]
  CGDoubleOddPrimePower p k -> [(2, 1), (unPrime p, k)]

-- | 'PrimitiveRoot n' is a type which is only inhabited by primitive roots of
-- n.
newtype PrimitiveRoot n = PrimitiveRoot (Mod n)
  deriving (Eq, Ord, Show)

-- | Extract the value from a 'PrimitiveRoot'.
--
-- > > fmap primitiveRootGetMod (isPrimitiveRoot (2 :: Mod 13))
-- > 2 `modulo` 13
primitiveRootGetMod :: PrimitiveRoot n -> Mod n
primitiveRootGetMod (PrimitiveRoot m) = m

-- | 'isPrimitiveRoot'' @cg@ @a@ checks whether @a@ is
-- a <https://en.wikipedia.org/wiki/Primitive_root_modulo_n primitive root>
-- of a given cyclic multiplicative group of residues @cg@.
--
-- > > let Just cg = cyclicGroupFromModulo 13
-- > > isPrimitiveRoot cg 1
-- > False
-- > > isPrimitiveRoot cg 2
-- > True
isPrimitiveRoot'
  :: (Integral a, UniqueFactorisation a)
  => CyclicGroup a
  -> a
  -> Bool
isPrimitiveRoot' cg r = r /= 0 && gcd r (prefValue m) == 1 && all (/= 1) exps
  where
    -- https://en.wikipedia.org/wiki/Primitive_root_modulo_n#Finding_primitive_roots
    m    = cyclicGroupToModulo cg
    phi  = totient m
    pows = map (\pk -> prefValue phi `quot` unPrime (fst pk)) (factorise phi)
    exps = map (\p -> powMod r p (prefValue m)) pows

-- | Check whether a given modular residue is
-- a <https://en.wikipedia.org/wiki/Primitive_root_modulo_n primitive root>.
--
-- > > isPrimitiveRoot (1 :: Mod 13)
-- > False
-- > > isPrimitiveRoot (2 :: Mod 13)
-- > True
--
-- Here is how to list all primitive roots:
--
-- > > mapMaybe isPrimitiveRoot [minBound .. maxBound] :: [Mod 13]
-- > [(2 `modulo` 13), (6 `modulo` 13), (7 `modulo` 13), (11 `modulo` 13)]
--
-- This function is a convenient wrapper around 'isPrimitiveRoot''. The latter
-- provides better control and performance, if you need them.
isPrimitiveRoot
  :: KnownNat n
  => Mod n
  -> Maybe (PrimitiveRoot n)
isPrimitiveRoot r = do
  cg <- cyclicGroupFromModulo (getNatMod r)
  guard (isPrimitiveRoot' cg (getNatVal r))
  Just (PrimitiveRoot r)
