-- |
-- Module:      Math.NumberTheory.GaussianIntegers
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- This module provides an interface for defining and manipulating
-- arithmetic functions.
--

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}

module Math.NumberTheory.ArithmeticFunctions
  ( Natural
  , Prime
  , unPrime
  , ArithmeticFunction(..)
  , runFunction
  , multiplicative
  , tau
  , sigma
  , totient
  , jordan
  , moebius
  , liouville
  , carmichael
  , additive
  , smallOmega
  , bigOmega
  , mangoldt
  ) where

import Control.Applicative
import Data.Semigroup

#if MIN_VERSION_base(4,8,0)
import Numeric.Natural
#else
import Data.Word
#endif

import Math.NumberTheory.Primes.Factorisation (factorise')

#if MIN_VERSION_base(4,8,0)
#else
type Natural = Integer
#endif

-- | Wrapper for primes.
newtype Prime = Prime { unPrime :: Natural }
  deriving (Eq, Ord, Show)

data ArithmeticFunction a where
  ArithmeticFunction :: Monoid m => (Prime -> Word -> m) -> (m -> a) -> ArithmeticFunction a

runFunction :: ArithmeticFunction a -> Natural -> a
runFunction (ArithmeticFunction f g)
  = g
  . mconcat
  . map (\(p, k) -> f (Prime (fromInteger p)) (fromIntegral k))
  . factorise'
  . toInteger

instance Functor ArithmeticFunction where
  fmap f (ArithmeticFunction g h) = ArithmeticFunction g (f . h)

instance Applicative ArithmeticFunction where
  pure x
    = ArithmeticFunction (\_ _ -> ()) (const x)
  (ArithmeticFunction f1 g1) <*> (ArithmeticFunction f2 g2)
    = ArithmeticFunction (\p k -> (f1 p k, f2 p k)) (\(a1, a2) -> g1 a1 (g2 a2))

instance Semigroup a => Semigroup (ArithmeticFunction a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (ArithmeticFunction a) where
  mempty  = pure mempty
  mappend = liftA2 mappend

instance Num a => Num (ArithmeticFunction a) where
  fromInteger = pure . fromInteger
  negate = fmap negate
  signum = fmap signum
  abs    = fmap abs
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)

instance Fractional a => Fractional (ArithmeticFunction a) where
  fromRational = pure . fromRational
  recip = fmap recip
  (/) = liftA2 (/)

multiplicative :: Num a => (Prime -> Word -> a) -> ArithmeticFunction a
multiplicative f = ArithmeticFunction ((Product .) . f) getProduct

tau :: Num a => ArithmeticFunction a
tau = multiplicative $ \_ k -> fromInteger (toInteger (k + 1))

sigma :: Integral a => Word -> ArithmeticFunction a
sigma 0 = tau
sigma a = multiplicative $ \(Prime p) k -> fromIntegral ((p ^ (a * (k + 1)) - 1) `div` (p ^ a - 1))

totient :: Num a => ArithmeticFunction a
totient = multiplicative $ \(Prime p) k -> fromIntegral ((p - 1) * p ^ (k - 1))

jordan :: Num a => Word -> ArithmeticFunction a
jordan 0 = pure 1
jordan 1 = totient
jordan a = multiplicative $ \(Prime p) k -> fromIntegral ((p ^ a - 1) * p ^ (a * (k - 1)))

moebius :: (Eq a, Num a) => ArithmeticFunction a
moebius = ArithmeticFunction (const (Product0 . f)) getProduct0
  where
    f 0 = 1
    f 1 = (-1)
    f _ = 0

liouville :: Num a => ArithmeticFunction a
liouville = multiplicative $ const ((-1) ^)

carmichael :: Integral a => ArithmeticFunction a
carmichael = ArithmeticFunction (\(Prime p) k -> LCM $ f p k) getLCM
  where
    f 2 1 = 1
    f 2 2 = 2
    f 2 k = fromIntegral $ (2 :: Natural) ^ (k - 2)
    f p k = fromIntegral $ (p - 1) * p ^ (k - 1)

additive :: Num a => (Prime -> Word -> a) -> ArithmeticFunction a
additive f = ArithmeticFunction ((Sum .) . f) getSum

smallOmega :: Num a => ArithmeticFunction a
smallOmega = additive $ (\_ _ -> 1)

bigOmega :: Num a => ArithmeticFunction a
bigOmega = additive $ const (fromInteger . toInteger)

mangoldt :: ArithmeticFunction Double
mangoldt = ArithmeticFunction (\p _ -> Mangoldt (Just p)) (maybe 0 (log . fromIntegral . unPrime) . getMangoldt)

newtype Product0 a = Product0 { getProduct0 :: a }

instance (Eq a, Num a) => Semigroup (Product0 a) where
  (Product0 0) <> _            = Product0 0
  _            <> (Product0 0) = Product0 0
  (Product0 a) <> (Product0 b) = Product0 $ a * b

instance (Eq a, Num a) => Monoid (Product0 a) where
  mempty = Product0 1
  mappend = (<>)

newtype Mangoldt a = Mangoldt { getMangoldt :: a }

instance Semigroup (Mangoldt (Maybe a)) where
  Mangoldt Nothing <> a = a
  a <> Mangoldt Nothing = a
  _ <> _ = Mangoldt Nothing

instance Monoid (Mangoldt (Maybe a)) where
  mempty = Mangoldt Nothing
  mappend = (<>)

newtype LCM a = LCM { getLCM :: a }

instance Integral a => Semigroup (LCM a) where
  (LCM a) <> (LCM b) = LCM $ a `lcm` b

instance Integral a => Monoid (LCM a) where
  mempty = LCM 1
  mappend = (<>)
