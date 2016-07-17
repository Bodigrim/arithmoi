-- |
-- Module:      Math.NumberTheory.ArithmeticFunctions.Class
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Generic type for arithmetic functions over arbitrary unique
-- factorisation domains.
--

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}

{-# OPTIONS_HADDOCK hide #-}

module Math.NumberTheory.ArithmeticFunctions.Class
  ( ArithmeticFunction(..)
  , runFunction
  ) where

import Control.Applicative
import Data.Semigroup

#if MIN_VERSION_base(4,8,0)
#else
import Data.Word
#endif

import Math.NumberTheory.UniqueFactorisation

-- | A typical arithmetic function operates on the canonical factorisation of
-- a number into prime's powers and consists of two rules. The first one
-- determines the values of the function on the powers of primes. The second
-- one determines how to combine these values into final result.
--
-- In the following definition the first argument is the function on prime's
-- powers, the monoid instance determines a rule of combination (typically
-- 'Product' or 'Sum'), and the second argument is convenient for unwrapping
-- (typically, 'getProduct' or 'getSum').
data ArithmeticFunction n a where
  ArithmeticFunction
    :: Monoid m
    => (Prime n -> Word -> m)
    -> (m -> a)
    -> ArithmeticFunction n a

-- | Convert to function. The value on 0 is undefined.
runFunction :: UniqueFactorisation n => ArithmeticFunction n a -> n -> a
runFunction (ArithmeticFunction f g)
  = g
  . mconcat
  . map (uncurry f)
  . factorise

instance Functor (ArithmeticFunction n) where
  fmap f (ArithmeticFunction g h) = ArithmeticFunction g (f . h)

instance Applicative (ArithmeticFunction n) where
  pure x
    = ArithmeticFunction (\_ _ -> ()) (const x)
  (ArithmeticFunction f1 g1) <*> (ArithmeticFunction f2 g2)
    = ArithmeticFunction (\p k -> (f1 p k, f2 p k)) (\(a1, a2) -> g1 a1 (g2 a2))

instance Semigroup a => Semigroup (ArithmeticFunction n a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (ArithmeticFunction n a) where
  mempty  = pure mempty
  mappend = liftA2 mappend

-- | Factorisation is expensive, so it is better to avoid doing it twice.
-- Write 'runFunction (f + g) n' instead of 'runFunction f n + runFunction g n'.
instance Num a => Num (ArithmeticFunction n a) where
  fromInteger = pure . fromInteger
  negate = fmap negate
  signum = fmap signum
  abs    = fmap abs
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)

instance Fractional a => Fractional (ArithmeticFunction n a) where
  fromRational = pure . fromRational
  recip = fmap recip
  (/) = liftA2 (/)

instance Floating a => Floating (ArithmeticFunction n a) where
  pi    = pure pi
  exp   = fmap exp
  log   = fmap log
  sin   = fmap sin
  cos   = fmap cos
  asin  = fmap asin
  acos  = fmap acos
  atan  = fmap atan
  sinh  = fmap sinh
  cosh  = fmap cosh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh
