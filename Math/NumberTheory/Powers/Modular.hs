-- |
-- Module:      Math.NumberTheory.Powers.Modular
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Modular powers (a. k. a. modular exponentiation).
--

{-# LANGUAGE MagicHash #-}

module Math.NumberTheory.Powers.Modular
  ( powMod
  , powModWord
  , powModInt
  ) where

import GHC.Exts (Word(..))
import GHC.Natural (powModNatural)
import qualified GHC.Integer.GMP.Internals as GMP (powModInteger, powModWord)
import Math.NumberTheory.Utils.FromIntegral

-- | @powMod@ @b@ @e@ @m@ computes (@b^e@) \`mod\` @m@ in effective way.
-- An exponent @e@ must be non-negative, a modulo @m@ must be positive.
-- Otherwise the behaviour of @powMod@ is undefined.
--
-- >>> powMod 2 3 5
-- 3
-- >>> powMod 3 12345678901234567890 1001
-- 1
--
-- See also 'Math.NumberTheory.Moduli.Class.powMod' and 'Math.NumberTheory.Moduli.Class.powSomeMod'.
--
-- For finite numeric types ('Int', 'Word', etc.)
-- modulo @m@ should be such that @m^2@ does not overflow,
-- otherwise the behaviour is undefined. If you
-- need both to fit into machine word and to handle large moduli,
-- take a look at 'powModInt' and 'powModWord'.
--
-- >>> powMod 3 101 (2^60-1 :: Integer)
-- 1018105167100379328 -- correct
-- >>> powMod 3 101 (2^60-1 :: Int)
-- 1115647832265427613 -- incorrect due to overflow
-- >>> powModInt 3 101 (2^60-1 :: Int)
-- 1018105167100379328 -- correct
powMod :: (Integral a, Integral b) => a -> b -> a -> a
powMod x y m
  | m <= 0    = error "powModInt: non-positive modulo"
  | y <  0    = error "powModInt: negative exponent"
  | otherwise = f (x `rem` m) y 1 `mod` m
  where
    f _ 0 acc = acc
    f b e acc = f (b * b `rem` m) (e `quot` 2)
      (if odd e then b * acc `rem` m else acc)

{-# INLINE [1] powMod #-}
{-# RULES
"powMod/Integer" powMod = powModInteger
  #-}

-- Work around https://ghc.haskell.org/trac/ghc/ticket/14085
powModInteger :: Integer -> Integer -> Integer -> Integer
powModInteger b e m = GMP.powModInteger (b `mod` m) e m

{-# RULES
"powMod/Natural" powMod = powModNatural
"powMod/Word"    powMod = powModWord
"powMod/Int"     powMod = powModInt
  #-}

-- | Specialised version of 'powMod', able to handle large moduli correctly.
--
-- >>> powModWord 3 101 (2^60-1)
-- 1018105167100379328
powModWord :: Word -> Word -> Word -> Word
powModWord (W# x) (W# y) (W# m) = W# (GMP.powModWord x y m)

-- | Specialised version of 'powMod', able to handle large moduli correctly.
--
-- >>> powModInt 3 101 (2^60-1)
-- 1018105167100379328
powModInt :: Int -> Int -> Int -> Int
powModInt x y m
  | m <= 0 = error "powModInt: non-positive modulo"
  | y <  0 = error "powModInt: negative exponent"
  | otherwise = wordToInt $ powModWord (intToWord (x `mod` m)) (intToWord y) (intToWord m)
