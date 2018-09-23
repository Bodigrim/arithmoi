-- |
-- Module:      Math.NumberTheory.Moduli.Jacobi
-- Copyright:   (c) 2011 Daniel Fischer, 2017-2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- <https://en.wikipedia.org/wiki/Jacobi_symbol Jacobi symbol>
-- is a generalization of the Legendre symbol, useful for primality
-- testing and integer factorization.
--

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE LambdaCase   #-}

{-# OPTIONS_GHC -fno-warn-deprecations  #-}

module Math.NumberTheory.Moduli.Jacobi
  ( JacobiSymbol(..)
  , jacobi
  ) where

import Data.Bits
#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup
#endif
import Numeric.Natural

import Math.NumberTheory.Utils

-- | Represents three possible values of
-- <https://en.wikipedia.org/wiki/Jacobi_symbol Jacobi symbol>.
data JacobiSymbol = MinusOne | Zero | One
  deriving (Eq, Ord, Show)

instance Semigroup JacobiSymbol where
  (<>) = \case
    MinusOne -> negJS
    Zero     -> const Zero
    One      -> id

instance Monoid JacobiSymbol where
  mempty = One
  mappend = (<>)

negJS :: JacobiSymbol -> JacobiSymbol
negJS = \case
  MinusOne -> One
  Zero     -> Zero
  One      -> MinusOne

-- | <https://en.wikipedia.org/wiki/Jacobi_symbol Jacobi symbol> of two arguments.
-- The lower argument (\"denominator\") must be odd and positive,
-- this condition is checked.
--
-- If arguments have a common factor, the result
-- is 'Zero', otherwise it is 'MinusOne' or 'One'.
--
-- >>> jacobi 1001 9911
-- Zero -- arguments have a common factor 11
-- >>> jacobi 1001 9907
-- MinusOne
{-# SPECIALISE jacobi :: Integer -> Integer -> JacobiSymbol,
                         Natural -> Natural -> JacobiSymbol,
                         Int -> Int -> JacobiSymbol,
                         Word -> Word -> JacobiSymbol
  #-}
jacobi :: (Integral a, Bits a) => a -> a -> JacobiSymbol
jacobi _ 1 = One
jacobi a b
  | b < 0     = error "Math.NumberTheory.Moduli.jacobi: negative denominator"
  | evenI b   = error "Math.NumberTheory.Moduli.jacobi: even denominator"
  | otherwise = jacobi' a b   -- b odd, > 1

jacobi' :: (Integral a, Bits a) => a -> a -> JacobiSymbol
jacobi' 0 _ = Zero
jacobi' 1 _ = One
jacobi' a b
  | a < 0     = let n = if rem4is3 b then MinusOne else One
                    (z, o) = shiftToOddCount (negate a)
                    s = if evenI z || rem8is1or7 b then n else negJS n
                in s <> jacobi' o b
  | a >= b    = case a `rem` b of
                  0 -> Zero
                  r -> jacPS One r b
  | evenI a   = case shiftToOddCount a of
                  (z, o) -> let r = if rem4is3 o && rem4is3 b then MinusOne else One
                                s = if evenI z || rem8is1or7 b then r else negJS r
                            in jacOL s b o
  | otherwise = jacOL (if rem4is3 a && rem4is3 b then MinusOne else One) b a

-- numerator positive and smaller than denominator
jacPS :: (Integral a, Bits a) => JacobiSymbol -> a -> a -> JacobiSymbol
jacPS !acc a b
  | evenI a = case shiftToOddCount a of
    (z, o)
      | evenI z || rem8is1or7 b -> jacOL (if rem4is3 o && rem4is3 b then negJS acc else acc) b o
      | otherwise               -> jacOL (if rem4is3 o && rem4is3 b then acc else negJS acc) b o
  | otherwise = jacOL (if rem4is3 a && rem4is3 b then negJS acc else acc) b a

-- numerator odd, positive and larger than denominator
jacOL :: (Integral a, Bits a) => JacobiSymbol -> a -> a -> JacobiSymbol
jacOL !acc _ 1 = acc
jacOL !acc a b = case a `rem` b of
  0 -> Zero
  r -> jacPS acc r b

-- Utilities

-- Sadly, GHC do not optimise `Prelude.even` to a bit test automatically.
evenI :: Bits a => a -> Bool
evenI n = not (n `testBit` 0)

-- For an odd input @n@ test whether n `rem` 4 == 1
rem4is3 :: Bits a => a -> Bool
rem4is3 n = n `testBit` 1

-- For an odd input @n@ test whether (n `rem` 8) `elem` [1, 7]
rem8is1or7 :: Bits a => a -> Bool
rem8is1or7 n = n `testBit` 1 == n `testBit` 2
