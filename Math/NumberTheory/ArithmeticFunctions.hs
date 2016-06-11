-- |
-- Module:      Math.NumberTheory.ArithmeticFunctions
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
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Math.NumberTheory.ArithmeticFunctions
  ( Natural
  , Prime
  , PrimeSequence(..)
  , UniqueFactorization(..)
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

newtype SmallPrime = SmallPrime { unSmallPrime :: Word }
  deriving (Eq, Ord, Show)

newtype BigPrime = BigPrime { unBigPrime :: Natural }
  deriving (Eq, Ord, Show)

type family Prime (f :: *) :: *

type instance Prime Int     = SmallPrime
type instance Prime Word    = SmallPrime
type instance Prime Integer = BigPrime
type instance Prime Natural = BigPrime

class Enum a => PrimeSequence a where
  nextPrime :: a -> Prime a
  precPrime :: a -> Prime a

class UniqueFactorization a where
  unPrime   :: Prime a -> a
  factorise :: a -> [(Prime a, Word)]

factoriseGeneric :: (Eq a, Integral a) => (Integer -> Prime a) -> a -> [(Prime a, Word)]
factoriseGeneric _ 0 = []
factoriseGeneric f n = map (f *** fromIntegral) . filter (\(x, _) -> x /= 0 && x /= 1 && x /= -1) . F.factorise' . toInteger $ n

instance UniqueFactorization Int where
  unPrime = fromIntegral . unSmallPrime
  factorise = factoriseGeneric (SmallPrime . fromIntegral)

instance UniqueFactorization Word where
  unPrime   = unSmallPrime
  factorise = factoriseGeneric (SmallPrime . fromIntegral)

instance UniqueFactorization Integer where
  unPrime   = toInteger . unBigPrime
  factorise = factoriseGeneric (BigPrime . fromInteger)

instance UniqueFactorization Natural where
  unPrime   = unBigPrime
  factorise = factoriseGeneric (BigPrime . fromInteger)

data ArithmeticFunction n a where
  ArithmeticFunction :: Monoid m => (Prime n -> Word -> m) -> (m -> a) -> ArithmeticFunction n a

-- type Convolution m = (Word -> m) -> (Word -> m) -> Word -> m

-- dirichletConvolution :: Num m => Convolution m
-- dirichletConvolution f g k = sum [f i * g (k - i) | i <- [0..k]]

-- exponentialConvolution :: Num m => Convolution m
-- exponentialConvolution f g k = sum [f i * g (k `div` i) | i <- [1..k], k `mod` i == 0]

runFunction :: UniqueFactorization n => ArithmeticFunction n a -> n -> a
runFunction (ArithmeticFunction f g)
  = g
  . mconcat
  . map (uncurry f)
  . factorise

runFunctionSieve :: ArithmeticFunction n a -> n -> n -> [a]
runFunctionSieve = undefined

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

multiplicative :: Num a => (Prime n -> Word -> a) -> ArithmeticFunction n a
multiplicative f = ArithmeticFunction ((Product .) . f) getProduct

tau :: Num a => ArithmeticFunction n a
tau = multiplicative $ \_ k -> fromIntegral (k + 1)

sigma :: forall n. (UniqueFactorization n, Integral n) => Word -> ArithmeticFunction n n
sigma 0 = tau
sigma a = multiplicative $ \((unPrime :: Prime n -> n) -> p) k -> (p ^ (a * (k + 1)) - 1) `div` (p ^ a - 1)

totient :: forall n. (UniqueFactorization n, Integral n) => ArithmeticFunction n n
totient = jordan 1

jordan :: forall n. (UniqueFactorization n, Integral n) => Word -> ArithmeticFunction n n
jordan a = multiplicative $ \((unPrime :: Prime n -> n) -> p) k -> (p ^ a - 1) * p ^ (a * (k - 1))

moebius :: (Eq a, Num a) => ArithmeticFunction n a
moebius = ArithmeticFunction (const (Product0 . f)) getProduct0
  where
    f 0 =  1    -- impossible case
    f 1 = -1
    f _ =  0

liouville :: Num a => ArithmeticFunction n a
liouville = multiplicative $ const ((-1) ^)

carmichael :: forall n. (UniqueFactorization n, Integral n) => ArithmeticFunction n n
carmichael = ArithmeticFunction (\((unPrime :: Prime n -> n) -> p) k -> LCM $ f p k) getLCM
  where
    f 2 1 = 1
    f 2 2 = 2
    f 2 k = 2 ^ (k - 2)
    f p k = (p - 1) * p ^ (k - 1)

additive :: Num a => (Prime n -> Word -> a) -> ArithmeticFunction n a
additive f = ArithmeticFunction ((Sum .) . f) getSum

smallOmega :: Num a => ArithmeticFunction n a
smallOmega = additive $ (\_ _ -> 1)

bigOmega :: ArithmeticFunction n Word
bigOmega = additive $ const id

expMangoldt :: forall n. (UniqueFactorization n, Num n) => ArithmeticFunction n n
expMangoldt = ArithmeticFunction (\((unPrime :: Prime n -> n) -> p) _ -> MangoldtOne p) runMangoldt

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
  mempty  = MangoldtZero
  mappend = (<>)

newtype LCM a = LCM { getLCM :: a }

instance Integral a => Semigroup (LCM a) where
  (LCM a) <> (LCM b) = LCM $ a `lcm` b

instance Integral a => Monoid (LCM a) where
  mempty  = LCM 1
  mappend = (<>)
