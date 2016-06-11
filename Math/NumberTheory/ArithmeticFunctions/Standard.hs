-- |
-- Module:      Math.NumberTheory.ArithmeticFunctions.Standard
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Textbook arithmetic functions.
--

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Math.NumberTheory.ArithmeticFunctions.Standard
  ( multiplicative
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

import Data.Semigroup

import Math.NumberTheory.ArithmeticFunctions.Class
import Math.NumberTheory.UniqueFactorization

#if MIN_VERSION_base(4,8,0)
#else
import Data.Word
#endif

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
smallOmega = additive (\_ _ -> 1)

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
runMangoldt m = case m of
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
