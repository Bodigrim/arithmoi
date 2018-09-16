-- |
-- Module:      Math.NumberTheory.DirichletCharacters
-- Copyright:   (c) 2018 Bhavik Mehta
-- Licence:     MIT
-- Maintainer:  Bhavik Mehta <bhavikmehta8@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Implementation and enumeration of Dirichlet characters.
--

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS -fno-warn-unused-imports #-}

module Math.NumberTheory.DirichletCharacters
  ( DirichletCharacter
  , generators
  ) where

import qualified Data.Map as M
import Data.Map                              (Map, (!))
import GHC.TypeNats.Compat                   (Nat)
import Numeric.Natural                       (Natural)

import Math.NumberTheory.Moduli              (PrimitiveRoot, CyclicGroup(..), isPrimitiveRoot', chineseRemainder2)
import Math.NumberTheory.UniqueFactorisation (UniqueFactorisation, unPrime, Prime, factorise)
import Math.NumberTheory.Powers              (powMod)

data DirichletCharacter (n :: Nat) = Generated (Map Natural Natural)
  deriving (Eq)

canonGenHelp :: (Integral a, UniqueFactorisation a) => (Prime a, Word) -> [a]
canonGenHelp (p, k)
  | p' == 2, k == 1 = []
  | p' == 2, k == 2 = [3]
  | p' == 2         = [5, p'^k - 1]
  | k == 1          = [modP]
  | otherwise       = [if powMod modP (p'-1) (p'*p') == 1 then modP + p' else modP]
  where p'   = unPrime p
        modP = head $ filter (isPrimitiveRoot' (CGOddPrimePower p 1)) [2..p' - 1]

generators :: (Integral a, UniqueFactorisation a) => a -> [a]
generators 1 = [1]
generators 2 = [1] -- special cases of trivial group
generators n = do
  (p,k) <- factorise n
  let factor = unPrime p ^ k
      rest = n `div` factor
  g <- canonGenHelp (p,k)
  return $ chineseRemainder2 (g,factor) (1,rest)
