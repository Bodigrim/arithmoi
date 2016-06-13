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
  , divisors, divisorsA
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

import Data.Set (Set)
import qualified Data.Set as S
import Data.Semigroup

import Unsafe.Coerce

import Math.NumberTheory.ArithmeticFunctions.Class
import Math.NumberTheory.UniqueFactorization

#if MIN_VERSION_base(4,8,0)
#else
import Data.Word
#endif

#if MIN_VERSION_base(4,7,0)
import Data.Coerce
#else
coerce :: a -> b
coerce = unsafeCoerce
#endif

-- There are special rewrite rules for (^ Int), but not for (^ Word).
unsafeCoerceWI :: Word -> Int
unsafeCoerceWI = unsafeCoerce

multiplicative :: Num a => (Prime n -> Word -> a) -> ArithmeticFunction n a
multiplicative f = ArithmeticFunction ((Product .) . f) getProduct

divisors :: (UniqueFactorization n, Num n, Ord n) => n -> Set n
divisors = runFunction divisorsA
{-# SPECIALIZE divisors :: Integer -> Set Integer #-}

divisorsA :: forall n. (UniqueFactorization n, Num n, Ord n) => ArithmeticFunction n (Set n)
divisorsA = ArithmeticFunction (\((unPrime :: Prime n -> n) -> p) k -> SetProduct $ divisorsHelper p k) (S.insert 1 . getSetProduct)

divisorsHelper :: Num n => n -> Word -> Set n
divisorsHelper _ 0 = S.empty
divisorsHelper p 1 = S.singleton p
divisorsHelper p a = S.fromDistinctAscList $ p : p * p : map (p ^) [3 .. unsafeCoerceWI a]
{-# INLINE divisorsHelper #-}

tau :: (UniqueFactorization n, Num a) => n -> a
tau = runFunction tauA

tauA :: Num a => ArithmeticFunction n a
tauA = multiplicative $ const (fromIntegral . succ)

sigma :: (UniqueFactorization n, Integral n) => Word -> n -> n
sigma = runFunction . sigmaA

sigmaA :: forall n. (UniqueFactorization n, Integral n) => Word -> ArithmeticFunction n n
sigmaA 0 = tauA
sigmaA 1 = multiplicative $ \((unPrime :: Prime n -> n) -> p) -> sigmaHelper p
sigmaA a = multiplicative $ \((unPrime :: Prime n -> n) -> p) -> sigmaHelper (p ^ unsafeCoerceWI a)

sigmaHelper :: Integral n => n -> Word -> n
sigmaHelper pa 1 = pa + 1
sigmaHelper pa 2 = pa * pa + pa + 1
sigmaHelper pa k = (pa ^ unsafeCoerceWI (k + 1) - 1) `quot` (pa - 1)
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
jordanA a = multiplicative $ \((unPrime :: Prime n -> n) -> p) -> jordanHelper (p ^ unsafeCoerceWI a)

jordanHelper :: Integral n => n -> Word -> n
jordanHelper pa 1 = pa - 1
jordanHelper pa 2 = (pa - 1) * pa
jordanHelper pa k = (pa - 1) * pa ^ unsafeCoerceWI (k - 1)
{-# INLINE jordanHelper #-}

moebius :: (UniqueFactorization n, Eq a, Num a) => n -> a
moebius = runFunction moebiusA

moebiusA :: Num a => ArithmeticFunction n a
moebiusA = ArithmeticFunction (const f) runMoebius
  where
    f 1 = MoebiusN
    f 0 = MoebiusP
    f _ = MoebiusZ

liouville :: (UniqueFactorization n, Num a) => n -> a
liouville = runFunction liouvilleA

liouvilleA :: Num a => ArithmeticFunction n a
liouvilleA = ArithmeticFunction (const $ Xor . odd) runXor

carmichael :: (UniqueFactorization n, Integral n) => n -> n
carmichael = runFunction carmichaelA
{- The specializations reflects available specializations of lcm. -}
{-# SPECIALIZE carmichael :: Int -> Int #-}
{-# SPECIALIZE carmichael :: Integer -> Integer #-}

carmichaelA :: forall n. (UniqueFactorization n, Integral n) => ArithmeticFunction n n
carmichaelA = ArithmeticFunction (\((unPrime :: Prime n -> n) -> p) k -> LCM $ f p k) getLCM
  where
    f 2 1 = 1
    f 2 2 = 2
    f 2 k = 2 ^ unsafeCoerceWI (k - 2)
    f p 1 = p - 1
    f p k = (p - 1) * p ^ unsafeCoerceWI (k - 1)

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

newtype Xor = Xor { _getXor :: Bool }

runXor :: Num a => Xor -> a
runXor m = case m of
  Xor False ->  1
  Xor True  -> -1

instance Semigroup Xor where
   (<>) = coerce ((/=) :: Bool -> Bool -> Bool)

instance Monoid Xor where
  mempty  = Xor False
  mappend = (<>)

newtype SetProduct a = SetProduct { getSetProduct :: Set a }

instance (Num a, Ord a) => Semigroup (SetProduct a) where
  SetProduct s1 <> SetProduct s2 = SetProduct $ s1 <> s2 <> foldMap (\n -> S.mapMonotonic (* n) s2) s1

instance (Num a, Ord a) => Monoid (SetProduct a) where
  mempty  = SetProduct mempty
  mappend = (<>)
