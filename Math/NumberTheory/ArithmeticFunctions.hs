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
{-# LANGUAGE LambdaCase          #-}

module Math.NumberTheory.ArithmeticFunctions
  ( Natural
  , Prime
  , unPrime
  , ArithmeticFunction(..)
  , runFunction
  , runFunctionSieve
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
  , expMangoldt
  ) where

import Control.Applicative
import Control.Arrow
import Data.Semigroup

#if MIN_VERSION_base(4,8,0)
import Numeric.Natural
#else
import Data.Word
#endif

import Math.NumberTheory.Primes.Factorisation as F (factorise')

#if MIN_VERSION_base(4,8,0)
#else
type Natural = Integer
#endif

-- | Wrapper for primes.
newtype Prime = Prime { unPrime :: Natural }
  deriving (Eq, Ord, Show)

data ArithmeticFunction a where
  ArithmeticFunction :: Monoid m => (Prime -> Word -> m) -> (m -> a) -> ArithmeticFunction a

factorise :: Natural -> [(Prime, Word)]
factorise = map (Prime . fromInteger *** fromIntegral) . filter (\(x, _) -> x /= 0 && x /= 1) . F.factorise' . toInteger

runFunction :: ArithmeticFunction a -> Natural -> a
runFunction (ArithmeticFunction f g)
  = g
  . mconcat
  . map (uncurry f)
  . factorise

runFunctionSieve :: ArithmeticFunction a -> Natural -> Natural -> [a]
runFunctionSieve = undefined

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

instance Floating a => Floating (ArithmeticFunction a) where
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
    f 0 =  1    -- impossible case
    f 1 = -1
    f _ =  0

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

expMangoldt :: Num a => ArithmeticFunction a
expMangoldt = ArithmeticFunction (\(Prime p) _ -> MangoldtOne p) (fromIntegral . runMangoldt)

newtype Product0 a = Product0 { getProduct0 :: a }

instance (Eq a, Num a) => Semigroup (Product0 a) where
  (Product0 0) <> _            = Product0 0
  _            <> (Product0 0) = Product0 0
  (Product0 a) <> (Product0 b) = Product0 $ a * b

instance (Eq a, Num a) => Monoid (Product0 a) where
  mempty = Product0 1
  mappend = (<>)

data Mangoldt a
  = MangoldtZero
  | MangoldtOne a
  | MangoldtMany

runMangoldt :: Num a => Mangoldt a -> a
runMangoldt = \case
  MangoldtZero  -> 1
  MangoldtOne a -> a
  MangoldtMany  -> 1

instance Semigroup (Mangoldt a) where
  MangoldtZero <> a = a
  a <> MangoldtZero = a
  _ <> _ = MangoldtMany

instance Monoid (Mangoldt a) where
  mempty = MangoldtZero
  mappend = (<>)

newtype LCM a = LCM { getLCM :: a }

instance Integral a => Semigroup (LCM a) where
  (LCM a) <> (LCM b) = LCM $ a `lcm` b

instance Integral a => Monoid (LCM a) where
  mempty = LCM 1
  mappend = (<>)
