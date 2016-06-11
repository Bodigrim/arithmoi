-- |
-- Module:      Math.NumberTheory.ArithmeticFunctions.Class
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Generic type for arithmetic functions over arbitrary unique
-- factorization domains.
--

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}

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

import Math.NumberTheory.UniqueFactorization

data ArithmeticFunction n a where
  ArithmeticFunction :: Monoid m => (Prime n -> Word -> m) -> (m -> a) -> ArithmeticFunction n a

runFunction :: UniqueFactorization n => ArithmeticFunction n a -> n -> a
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
