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

{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

module Math.NumberTheory.Moduli.Jacobi
  ( JacobiSymbol(..)
  , jacobi
  , jacobi'
  ) where

import Data.Array.Unboxed
import Data.Bits
#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup
#endif

import Math.NumberTheory.Unsafe
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
-- > > jacobi 1001 9911
-- > Zero -- arguments have a common factor 11
-- > > jacobi 1001 9907
-- > MinusOne
{-# SPECIALISE jacobi :: Integer -> Integer -> JacobiSymbol,
                         Int -> Int -> JacobiSymbol,
                         Word -> Word -> JacobiSymbol
  #-}
jacobi :: (Integral a, Bits a) => a -> a -> JacobiSymbol
jacobi a b
  | b < 0       = error "Math.NumberTheory.Moduli.jacobi: negative denominator"
  | evenI b     = error "Math.NumberTheory.Moduli.jacobi: even denominator"
  | b == 1      = One
  | otherwise   = jacobi' a b   -- b odd, > 1

-- | Similar to 'jacobi', but the condition on the lower argument
-- (\"denominator\") is __not__ checked.
{-# SPECIALISE jacobi' :: Integer -> Integer -> JacobiSymbol,
                          Int -> Int -> JacobiSymbol,
                          Word -> Word -> JacobiSymbol
  #-}
jacobi' :: (Integral a, Bits a) => a -> a -> JacobiSymbol
jacobi' a b
  | a == 0      = Zero
  | a == 1      = One
  | a < 0       = let n | rem4 b == 1 = One
                        | otherwise   = MinusOne
                      -- Blech, minBound may pose problems
                      (z,o) = shiftToOddCount (abs $ toInteger a)
                      s | evenI z || unsafeAt jac2 (rem8 b) == 1 = n
                        | otherwise                              = negJS n
                  in s <> jacobi' (fromInteger o) b
  | a >= b      = case a `rem` b of
                    0 -> Zero
                    r -> jacPS One r b
  | evenI a     = case shiftToOddCount a of
                    (z,o) -> let r | rem4 o .&. rem4 b == 1 = One
                                   | otherwise              = MinusOne
                                 s | evenI z || unsafeAt jac2 (rem8 b) == 1 = r
                                   | otherwise                              = negJS r
                             in jacOL s b o
  | otherwise   = case rem4 a .&. rem4 b of
                    3 -> jacOL MinusOne b a
                    _ -> jacOL One      b a

-- numerator positive and smaller than denominator
{-# SPECIALISE jacPS :: JacobiSymbol -> Integer -> Integer -> JacobiSymbol,
                        JacobiSymbol -> Int -> Int -> JacobiSymbol,
                        JacobiSymbol -> Word -> Word -> JacobiSymbol
  #-}
jacPS :: (Integral a, Bits a) => JacobiSymbol -> a -> a -> JacobiSymbol
jacPS j a b
  | evenI a     = case shiftToOddCount a of
                    (z,o) | evenI z || unsafeAt jac2 (rem8 b) == 1 ->
                              jacOL (if rem4 o .&. rem4 b == 3 then (negJS j) else j) b o
                          | otherwise ->
                              jacOL (if rem4 o .&. rem4 b == 3 then j else (negJS j)) b o
  | otherwise   = jacOL (if rem4 a .&. rem4 b == 3 then (negJS j) else j) b a

-- numerator odd, positive and larger than denominator
{-# SPECIALISE jacOL :: JacobiSymbol -> Integer -> Integer -> JacobiSymbol,
                        JacobiSymbol -> Int -> Int -> JacobiSymbol,
                        JacobiSymbol -> Word -> Word -> JacobiSymbol
  #-}
jacOL :: (Integral a, Bits a) => JacobiSymbol -> a -> a -> JacobiSymbol
jacOL j a b
  | b == 1    = j
  | otherwise = case a `rem` b of
                 0 -> Zero
                 r -> jacPS j r b

-- Utilities

-- For large Integers, going via Int is much faster than bit-fiddling
-- on the Integer, so we do that.
{-# SPECIALISE evenI :: Integer -> Bool,
                        Int -> Bool,
                        Word -> Bool
  #-}
evenI :: Integral a => a -> Bool
evenI n = fromIntegral n .&. 1 == (0 :: Int)

{-# SPECIALISE rem4 :: Integer -> Int,
                       Int -> Int,
                       Word -> Int
  #-}
rem4 :: Integral a => a -> Int
rem4 n = fromIntegral n .&. 3

{-# SPECIALISE rem8 :: Integer -> Int,
                       Int -> Int,
                       Word -> Int
  #-}
rem8 :: Integral a => a -> Int
rem8 n = fromIntegral n .&. 7

jac2 :: UArray Int Int
jac2 = array (0,7) [(0,0),(1,1),(2,0),(3,-1),(4,0),(5,-1),(6,0),(7,1)]
