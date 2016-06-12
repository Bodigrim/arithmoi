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
  , tau, tauA
  , sigma, sigmaA
  , totient, totientA
  , jordan, jordanA
  , moebius, moebiusA
  , liouville, liouvilleA
  , carmichael, carmichaelA
  , additive
  , smallOmega, smallOmegaA
  , bigOmega, bigOmegaA
  , expMangoldt, expMangoldtA
  ) where

import Data.Coerce
import Data.Semigroup

import Math.NumberTheory.ArithmeticFunctions.Class
import Math.NumberTheory.UniqueFactorization

#if MIN_VERSION_base(4,8,0)
#else
import Data.Word
#endif

multiplicative :: Num a => (Prime n -> Word -> a) -> ArithmeticFunction n a
multiplicative f = ArithmeticFunction ((Product .) . f) getProduct

tau :: (UniqueFactorization n, Num a) => n -> a
tau = runFunction tauA

tauA :: Num a => ArithmeticFunction n a
tauA = multiplicative $ const (fromIntegral . succ)

sigma :: (UniqueFactorization n, Integral n) => Word -> n -> n
sigma = runFunction . sigmaA

sigmaA :: forall n. (UniqueFactorization n, Integral n) => Word -> ArithmeticFunction n n
sigmaA 0 = tauA
sigmaA 1 = multiplicative $ \((unPrime :: Prime n -> n) -> p) -> sigmaHelper p
sigmaA a = multiplicative $ \((unPrime :: Prime n -> n) -> p) -> sigmaHelper (p ^ a)

sigmaHelper :: Integral n => n -> Word -> n
sigmaHelper pa 1 = pa + 1
sigmaHelper pa 2 = pa * pa + pa + 1
sigmaHelper pa k = (pa ^ (k + 1) - 1) `div` (pa - 1)
{-# INLINE sigmaHelper #-}

totient :: (UniqueFactorization n, Integral n) => n -> n
totient = runFunction totientA

totientA :: forall n. (UniqueFactorization n, Integral n) => ArithmeticFunction n n
totientA = multiplicative $ \((unPrime :: Prime n -> n) -> p) -> jordanHelper p

jordan :: (UniqueFactorization n, Integral n) => Word -> n -> n
jordan = runFunction . jordanA

jordanA :: forall n. (UniqueFactorization n, Integral n) => Word -> ArithmeticFunction n n
jordanA 0 = multiplicative $ \_ _ -> 0
jordanA 1 = totientA
jordanA a = multiplicative $ \((unPrime :: Prime n -> n) -> p) -> jordanHelper (p ^ a)

jordanHelper :: Integral n => n -> Word -> n
jordanHelper pa 1 = pa - 1
jordanHelper pa 2 = (pa - 1) * pa
jordanHelper pa k = (pa - 1) * pa ^ (k - 1)
{-# INLINE jordanHelper #-}

moebius :: (UniqueFactorization n, Eq a, Num a) => n -> a
moebius = runFunction moebiusA

moebiusA :: (Eq a, Num a) => ArithmeticFunction n a
moebiusA = ArithmeticFunction (const f) runMoebius
  where
    f 1 = MoebiusN
    f 0 = MoebiusP
    f _ = MoebiusZ

liouville :: (UniqueFactorization n, Num a) => n -> a
liouville = runFunction liouvilleA

liouvilleA :: Num a => ArithmeticFunction n a
liouvilleA = multiplicative $ const ((-1) ^)

carmichael :: (UniqueFactorization n, Integral n) => n -> n
carmichael = runFunction carmichaelA

carmichaelA :: forall n. (UniqueFactorization n, Integral n) => ArithmeticFunction n n
carmichaelA = ArithmeticFunction (\((unPrime :: Prime n -> n) -> p) k -> LCM $ f p k) getLCM
  where
    f 2 1 = 1
    f 2 2 = 2
    f 2 k = 2 ^ (k - 2)
    f p 1 = p - 1
    f p k = (p - 1) * p ^ (k - 1)

additive :: Num a => (Prime n -> Word -> a) -> ArithmeticFunction n a
additive f = ArithmeticFunction ((Sum .) . f) getSum

smallOmega :: (UniqueFactorization n, Num a) => n -> a
smallOmega = runFunction smallOmegaA

smallOmegaA :: Num a => ArithmeticFunction n a
smallOmegaA = additive (\_ _ -> 1)

bigOmega :: UniqueFactorization n => n -> Word
bigOmega = runFunction bigOmegaA

bigOmegaA :: ArithmeticFunction n Word
bigOmegaA = additive $ const id

expMangoldt :: (UniqueFactorization n, Num n) => n -> n
expMangoldt = runFunction expMangoldtA

expMangoldtA :: forall n. (UniqueFactorization n, Num n) => ArithmeticFunction n n
expMangoldtA = ArithmeticFunction (\((unPrime :: Prime n -> n) -> p) _ -> MangoldtOne p) runMangoldt

data Moebius
  = MoebiusZ
  | MoebiusP
  | MoebiusN

runMoebius :: Num a => Moebius -> a
runMoebius m = case m of
  MoebiusZ ->  0
  MoebiusP ->  1
  MoebiusN -> -1

instance Semigroup Moebius where
  MoebiusZ <> _ = MoebiusZ
  _ <> MoebiusZ = MoebiusZ
  MoebiusP <> a = a
  a <> MoebiusP = a
  _ <> _ = MoebiusP

instance Monoid Moebius where
  mempty = MoebiusP
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
  (<>) = coerce (lcm :: a -> a -> a)

instance Integral a => Monoid (LCM a) where
  mempty  = LCM 1
  mappend = (<>)
