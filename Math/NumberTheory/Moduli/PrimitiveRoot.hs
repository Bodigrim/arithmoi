-- |
-- Module:      Math.NumberTheory.Moduli.PrimitiveRoot
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Primitive roots and cyclic groups.
--

{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

#if __GLASGOW_HASKELL__ < 801
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#endif

module Math.NumberTheory.Moduli.PrimitiveRoot
  ( -- * Primitive roots
    PrimitiveRoot
  , unPrimitiveRoot
  , isPrimitiveRoot
  ) where

import Control.Monad (guard)
import Data.Constraint

import Math.NumberTheory.ArithmeticFunctions (totient)
import Math.NumberTheory.Moduli.Class (MultMod(..), isMultElement, Mod, getNatVal)
import Math.NumberTheory.Moduli.Singleton
import Math.NumberTheory.Powers.Modular
import Math.NumberTheory.Primes

-- | 'PrimitiveRoot' m is a type which is only inhabited
-- by <https://en.wikipedia.org/wiki/Primitive_root_modulo_n primitive roots> of m.
newtype PrimitiveRoot m = PrimitiveRoot
  { unPrimitiveRoot :: MultMod m -- ^ Extract primitive root value.
  }
  deriving (Eq, Show)

-- https://en.wikipedia.org/wiki/Primitive_root_modulo_n#Finding_primitive_roots
isPrimitiveRoot'
  :: (Integral a, UniqueFactorisation a)
  => CyclicGroup a m
  -> a
  -> Bool
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
    guard $ isPrimitiveRoot' cg (fromIntegral (getNatVal r))
    return $ PrimitiveRoot r'
