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
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_HADDOCK hide #-}

module Math.NumberTheory.ArithmeticFunctions.Standard
  ( -- * Multiplicative functions
    multiplicative
  , divisors, divisorsA
  , divisorsSmall, divisorsSmallA
  , tau, tauA
  , sigma, sigmaA
  , totient, totientA
  , jordan, jordanA
  , moebius, moebiusA
  , liouville, liouvilleA
    -- * Additive functions
  , additive
  , smallOmega, smallOmegaA
  , bigOmega, bigOmegaA
    -- * Misc
  , carmichael, carmichaelA
  , expMangoldt, expMangoldtA
  ) where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Set (Set)
import qualified Data.Set as S
import Data.Semigroup

import Math.NumberTheory.ArithmeticFunctions.Class
import Math.NumberTheory.UniqueFactorisation

import Numeric.Natural

#if MIN_VERSION_base(4,8,0)
#else
import Data.Foldable
import Data.Word
#endif

#if MIN_VERSION_base(4,7,0)
import Data.Coerce
#else
import Unsafe.Coerce

coerce :: a -> b
coerce = unsafeCoerce
#endif

wordToInt :: Word -> Int
wordToInt = fromIntegral

-- | Create a multiplicative function from the function on prime's powers. See examples below.
multiplicative :: Num a => (Prime n -> Word -> a) -> ArithmeticFunction n a
multiplicative f = ArithmeticFunction ((Product .) . f) getProduct

divisors :: (UniqueFactorisation n, Num n, Ord n) => n -> Set n
divisors = runFunction divisorsA
{-# SPECIALIZE divisors :: Natural -> Set Natural #-}
{-# SPECIALIZE divisors :: Integer -> Set Integer #-}

-- | The set of all (positive) divisors of an argument.
divisorsA :: forall n. (UniqueFactorisation n, Num n, Ord n) => ArithmeticFunction n (Set n)
divisorsA = ArithmeticFunction (\((unPrime :: Prime n -> n) -> p) k -> SetProduct $ divisorsHelper p k) (S.insert 1 . getSetProduct)

divisorsHelper :: Num n => n -> Word -> Set n
divisorsHelper _ 0 = S.empty
divisorsHelper p 1 = S.singleton p
divisorsHelper p a = S.fromDistinctAscList $ p : p * p : map (p ^) [3 .. wordToInt a]
{-# INLINE divisorsHelper #-}

divisorsSmall :: (UniqueFactorisation n, Prime n ~ Prime Int) => n -> IntSet
divisorsSmall = runFunction divisorsSmallA

-- | Same as 'divisors', but with better performance on cost of type restriction.
divisorsSmallA :: forall n. (Prime n ~ Prime Int) => ArithmeticFunction n IntSet
divisorsSmallA = ArithmeticFunction (\p k -> IntSetProduct $ divisorsHelperSmall (unPrime p) k) (IS.insert 1 . getIntSetProduct)

divisorsHelperSmall :: Int -> Word -> IntSet
divisorsHelperSmall _ 0 = IS.empty
divisorsHelperSmall p 1 = IS.singleton p
divisorsHelperSmall p a = IS.fromDistinctAscList $ p : p * p : map (p ^) [3 .. wordToInt a]
{-# INLINE divisorsHelperSmall #-}

tau :: (UniqueFactorisation n, Num a) => n -> a
tau = runFunction tauA

-- | The number of (positive) divisors of an argument.
--
-- > tauA = multiplicative (\_ k -> k + 1)
tauA :: Num a => ArithmeticFunction n a
tauA = multiplicative $ const (fromIntegral . succ)

sigma :: (UniqueFactorisation n, Integral n) => Word -> n -> n
sigma = runFunction . sigmaA

-- | The sum of the @k@-th powers of (positive) divisors of an argument.
--
-- > sigmaA = multiplicative (\p k -> sum $ map (p ^) [0..k])
-- > sigmaA 0 = tauA
sigmaA :: forall n. (UniqueFactorisation n, Integral n) => Word -> ArithmeticFunction n n
sigmaA 0 = tauA
sigmaA 1 = multiplicative $ \((unPrime :: Prime n -> n) -> p) -> sigmaHelper p
sigmaA a = multiplicative $ \((unPrime :: Prime n -> n) -> p) -> sigmaHelper (p ^ wordToInt a)

sigmaHelper :: Integral n => n -> Word -> n
sigmaHelper pa 1 = pa + 1
sigmaHelper pa 2 = pa * pa + pa + 1
sigmaHelper pa k = (pa ^ wordToInt (k + 1) - 1) `quot` (pa - 1)
{-# INLINE sigmaHelper #-}

totient :: (UniqueFactorisation n, Integral n) => n -> n
totient = runFunction totientA

-- | Calculates the totient of a positive number @n@, i.e.
--   the number of @k@ with @1 <= k <= n@ and @'gcd' n k == 1@,
--   in other words, the order of the group of units in @&#8484;/(n)@.
totientA :: forall n. (UniqueFactorisation n, Integral n) => ArithmeticFunction n n
totientA = multiplicative $ \((unPrime :: Prime n -> n) -> p) -> jordanHelper p

jordan :: (UniqueFactorisation n, Integral n) => Word -> n -> n
jordan = runFunction . jordanA

-- | Calculates the k-th Jordan function of an argument.
--
-- > jordanA 1 = totientA
jordanA :: forall n. (UniqueFactorisation n, Integral n) => Word -> ArithmeticFunction n n
jordanA 0 = multiplicative $ \_ _ -> 0
jordanA 1 = totientA
jordanA a = multiplicative $ \((unPrime :: Prime n -> n) -> p) -> jordanHelper (p ^ wordToInt a)

jordanHelper :: Integral n => n -> Word -> n
jordanHelper pa 1 = pa - 1
jordanHelper pa 2 = (pa - 1) * pa
jordanHelper pa k = (pa - 1) * pa ^ wordToInt (k - 1)
{-# INLINE jordanHelper #-}

moebius :: (UniqueFactorisation n, Num a) => n -> a
moebius = runFunction moebiusA

-- | Calculates the Moebius function of an argument.
moebiusA :: Num a => ArithmeticFunction n a
moebiusA = ArithmeticFunction (const f) runMoebius
  where
    f 1 = MoebiusN
    f 0 = MoebiusP
    f _ = MoebiusZ

liouville :: (UniqueFactorisation n, Num a) => n -> a
liouville = runFunction liouvilleA

-- | Calculates the Liouville function of an argument.
liouvilleA :: Num a => ArithmeticFunction n a
liouvilleA = ArithmeticFunction (const $ Xor . odd) runXor

carmichael :: (UniqueFactorisation n, Integral n) => n -> n
carmichael = runFunction carmichaelA
{- The specializations reflects available specializations of lcm. -}
{-# SPECIALIZE carmichael :: Int -> Int #-}
{-# SPECIALIZE carmichael :: Integer -> Integer #-}

-- | Calculates the Carmichael function for a positive integer, that is,
--   the (smallest) exponent of the group of units in @&#8484;/(n)@.
carmichaelA :: forall n. (UniqueFactorisation n, Integral n) => ArithmeticFunction n n
carmichaelA = ArithmeticFunction (\((unPrime :: Prime n -> n) -> p) k -> LCM $ f p k) getLCM
  where
    f 2 1 = 1
    f 2 2 = 2
    f 2 k = 2 ^ wordToInt (k - 2)
    f p 1 = p - 1
    f p 2 = (p - 1) * p
    f p k = (p - 1) * p ^ wordToInt (k - 1)

-- | Create an additive function from the function on prime's powers. See examples below.
additive :: Num a => (Prime n -> Word -> a) -> ArithmeticFunction n a
additive f = ArithmeticFunction ((Sum .) . f) getSum

smallOmega :: (UniqueFactorisation n, Num a) => n -> a
smallOmega = runFunction smallOmegaA

-- | Number of distinct prime factors.
--
-- > smallOmegaA = additive (\_ _ -> 1)
smallOmegaA :: Num a => ArithmeticFunction n a
smallOmegaA = additive (\_ _ -> 1)

bigOmega :: UniqueFactorisation n => n -> Word
bigOmega = runFunction bigOmegaA

-- | Number of prime factors, counted with multiplicity.
--
-- > bigOmegaA = additive (\_ k -> k)
bigOmegaA :: ArithmeticFunction n Word
bigOmegaA = additive $ const id

expMangoldt :: (UniqueFactorisation n, Num n) => n -> n
expMangoldt = runFunction expMangoldtA

-- | The exponent of von Mangoldt function. Use @log expMangoldtA@ to recover von Mangoldt function itself.
expMangoldtA :: forall n. (UniqueFactorisation n, Num n) => ArithmeticFunction n n
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

newtype IntSetProduct = IntSetProduct { getIntSetProduct :: IntSet }

instance Semigroup IntSetProduct where
  IntSetProduct s1 <> IntSetProduct s2 = IntSetProduct $ IS.unions $ s1 : s2 : map (\n -> IS.map (* n) s2) (IS.toAscList s1)

instance Monoid IntSetProduct where
  mempty  = IntSetProduct mempty
  mappend = (<>)
