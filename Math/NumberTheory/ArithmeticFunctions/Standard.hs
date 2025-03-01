-- |
-- Module:      Math.NumberTheory.ArithmeticFunctions.Standard
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Textbook arithmetic functions.
--

{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.ArithmeticFunctions.Standard
  ( -- * List divisors
    divisors, divisorsA
  , divisorsList, divisorsListA
  , divisorsSmall, divisorsSmallA
  , divisorsTo, divisorsToA
    -- * Multiplicative functions
  , multiplicative
  , divisorCount, tau, tauA
  , sigma, sigmaA
  , totient, totientA
  , jordan, jordanA
  , ramanujan, ramanujanA
  , moebius, moebiusA, Moebius(..), runMoebius
  , liouville, liouvilleA
    -- * Additive functions
  , additive
  , smallOmega, smallOmegaA
  , bigOmega, bigOmegaA
    -- * Misc
  , carmichael, carmichaelA
  , expMangoldt, expMangoldtA
  , isNFree, isNFreeA, nFrees, nFreesBlock
  ) where

import Data.Coerce
import Data.Euclidean (GcdDomain(divide))
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Semigroup

import Math.NumberTheory.ArithmeticFunctions.Class
import Math.NumberTheory.ArithmeticFunctions.Moebius
import Math.NumberTheory.ArithmeticFunctions.NFreedom (nFrees, nFreesBlock)
import Math.NumberTheory.Primes
import Math.NumberTheory.Utils.FromIntegral

import Numeric.Natural

-- | Create a multiplicative function from the function on prime's powers. See examples below.
multiplicative :: Num a => (Prime n -> Word -> a) -> ArithmeticFunction n a
multiplicative f = ArithmeticFunction ((Product .) . f) getProduct

-- | See 'divisorsA'.
divisors :: (UniqueFactorisation n, Ord n) => n -> Set n
divisors = runFunction divisorsA
{-# SPECIALIZE divisors :: Natural -> Set Natural #-}
{-# SPECIALIZE divisors :: Integer -> Set Integer #-}

-- | The set of all (positive) divisors of an argument.
divisorsA :: (Ord n, Num n) => ArithmeticFunction n (Set n)
divisorsA = ArithmeticFunction (\p -> SetProduct . divisorsHelper (unPrime p)) (S.insert 1 . getSetProduct)

divisorsHelper :: Num n => n -> Word -> Set n
divisorsHelper _ 0 = S.empty
divisorsHelper p 1 = S.singleton p
divisorsHelper p a = S.fromDistinctAscList $ p : p * p : map (p ^) [3 .. wordToInt a]
{-# INLINE divisorsHelper #-}

-- | See 'divisorsListA'.
divisorsList :: UniqueFactorisation n => n -> [n]
divisorsList = runFunction divisorsListA

-- | The unsorted list of all (positive) divisors of an argument, produced in lazy fashion.
divisorsListA :: Num n => ArithmeticFunction n [n]
divisorsListA = ArithmeticFunction (\p -> ListProduct . divisorsListHelper (unPrime p)) ((1 :) . getListProduct)

divisorsListHelper :: Num n => n -> Word -> [n]
divisorsListHelper _ 0 = []
divisorsListHelper p 1 = [p]
divisorsListHelper p a = p : p * p : map (p ^) [3 .. wordToInt a]
{-# INLINE divisorsListHelper #-}

-- | See 'divisorsSmallA'.
divisorsSmall :: Int -> IntSet
divisorsSmall = runFunction divisorsSmallA

-- | Same as 'divisors', but with better performance on cost of type restriction.
divisorsSmallA :: ArithmeticFunction Int IntSet
divisorsSmallA = ArithmeticFunction (\p -> IntSetProduct . divisorsHelperSmall (unPrime p)) (IS.insert 1 . getIntSetProduct)

divisorsHelperSmall :: Int -> Word -> IntSet
divisorsHelperSmall _ 0 = IS.empty
divisorsHelperSmall p 1 = IS.singleton p
divisorsHelperSmall p a = IS.fromDistinctAscList $ p : p * p : map (p ^) [3 .. wordToInt a]
{-# INLINE divisorsHelperSmall #-}

-- | See 'divisorsToA'.
divisorsTo :: (UniqueFactorisation n, Integral n) => n -> n -> Set n
divisorsTo to = runFunction (divisorsToA to)

-- | The set of all (positive) divisors up to an inclusive bound.
divisorsToA :: (UniqueFactorisation n, Integral n) => n -> ArithmeticFunction n (Set n)
divisorsToA to = ArithmeticFunction f unwrap
  where f p k = BoundedSetProduct (\bound -> divisorsToHelper bound (unPrime p) k)
        unwrap (BoundedSetProduct res) = if 1 <= to then S.insert 1 (res to) else res to

-- | Generate at most @a@ powers of @p@ up to an inclusive bound @b@.
divisorsToHelper :: (Ord n, Num n) => n -> n -> Word -> Set n
divisorsToHelper _ _ 0 = S.empty
divisorsToHelper b p 1 = if p <= b then S.singleton p else S.empty
divisorsToHelper b p a = S.fromDistinctAscList $ take (wordToInt a) $ takeWhile (<=b) $ iterate (p*) p
{-# INLINE divisorsToHelper #-}

-- | Synonym for 'tau'.
--
-- >>> map divisorCount [1..10]
-- [1,2,2,3,2,4,2,4,3,4]
divisorCount :: (UniqueFactorisation n, Num a) => n -> a
divisorCount = tau

-- | See 'tauA'.
tau :: (UniqueFactorisation n, Num a) => n -> a
tau = runFunction tauA

-- | The number of (positive) divisors of an argument.
--
-- > tauA = multiplicative (\_ k -> k + 1)
tauA :: Num a => ArithmeticFunction n a
tauA = multiplicative $ const (fromIntegral . succ)

-- | See 'sigmaA'.
sigma :: (UniqueFactorisation n, Integral n, Num a, GcdDomain a) => Word -> n -> a
sigma = runFunction . sigmaA
{-# INLINABLE sigma #-}

-- | The sum of the @k@-th powers of (positive) divisors of an argument.
--
-- > sigmaA = multiplicative (\p k -> sum $ map (p ^) [0..k])
-- > sigmaA 0 = tauA
sigmaA :: (Integral n, Num a, GcdDomain a) => Word -> ArithmeticFunction n a
sigmaA 0 = tauA
sigmaA 1 = multiplicative $ sigmaHelper . fromIntegral' . unPrime
sigmaA a = multiplicative $ sigmaHelper . (^ wordToInt a) . fromIntegral' . unPrime
{-# INLINABLE sigmaA #-}

sigmaHelper :: (Num a, GcdDomain a) => a -> Word -> a
sigmaHelper pa 1 = pa + 1
sigmaHelper pa 2 = pa * pa + pa + 1
sigmaHelper pa k = fromJust ((pa ^ wordToInt (k + 1) - 1) `divide` (pa - 1))
{-# INLINE sigmaHelper #-}

-- | See 'totientA'.
totient :: UniqueFactorisation n => n -> n
totient = runFunction totientA
{-# INLINABLE totient #-}

-- | Calculates the totient of a positive number @n@, i.e.
--   the number of @k@ with @1 <= k <= n@ and @'gcd' n k == 1@,
--   in other words, the order of the group of units in @&#8484;/(n)@.
totientA :: Num n => ArithmeticFunction n n
totientA = multiplicative $ jordanHelper . unPrime
{-# INLINABLE totientA #-}

-- | See 'jordanA'.
jordan :: UniqueFactorisation n => Word -> n -> n
jordan = runFunction . jordanA

-- | Calculates the k-th Jordan function of an argument.
--
-- > jordanA 1 = totientA
jordanA :: Num n => Word -> ArithmeticFunction n n
jordanA 0 = multiplicative $ \_ _ -> 0
jordanA 1 = totientA
jordanA a = multiplicative $ jordanHelper . (^ wordToInt a) . unPrime

jordanHelper :: Num n => n -> Word -> n
jordanHelper pa 1 = pa - 1
jordanHelper pa 2 = (pa - 1) * pa
jordanHelper pa k = (pa - 1) * pa ^ wordToInt (k - 1)
{-# INLINE jordanHelper #-}

-- | See 'ramanujanA'.
ramanujan :: Integer -> Integer
ramanujan = runFunction ramanujanA

-- | Calculates the <https://en.wikipedia.org/wiki/Ramanujan_tau_function Ramanujan tau function>
--   of a positive number @n@, using formulas given <http://www.numbertheory.org/php/tau.html here>
ramanujanA :: ArithmeticFunction Integer Integer
ramanujanA = multiplicative $ ramanujanHelper . unPrime

ramanujanHelper :: Integer -> Word -> Integer
ramanujanHelper _ 0 = 1
ramanujanHelper 2 1 = -24
ramanujanHelper p 1 = (65 * sigmaHelper (p ^ (11 :: Int)) 1 + 691 * sigmaHelper (p ^ (5 :: Int)) 1 - 691 * 252 * 2 * sum [sigma 5 k * sigma 5 (p-k) | k <- [1..(p `quot` 2)]]) `quot` 756
ramanujanHelper p k = sum $ zipWith3 (\a b c -> a * b * c) paPowers tpPowers binomials
  where pa = p ^ (11 :: Int)
        tp = ramanujanHelper p 1
        paPowers = iterate (* (-pa)) 1
        binomials = scanl (\acc j -> acc * (k' - 2 * j) * (k' - 2 * j - 1) `quot` (k' - j) `quot` (j + 1)) 1 [0 .. k' `quot` 2 - 1]
        k' = wordToInteger k
        tpPowers = reverse $ take (length binomials) $ iterate (* tp^(2::Int)) (if even k then 1 else tp)
{-# INLINE ramanujanHelper #-}

-- | See 'moebiusA'.
moebius :: UniqueFactorisation n => n -> Moebius
moebius = runFunction moebiusA

-- | Calculates the Möbius function of an argument.
moebiusA :: ArithmeticFunction n Moebius
moebiusA = ArithmeticFunction (const f) id
  where
    f 1 = MoebiusN
    f 0 = MoebiusP
    f _ = MoebiusZ

-- | See 'liouvilleA'.
liouville :: (UniqueFactorisation n, Num a) => n -> a
liouville = runFunction liouvilleA

-- | Calculates the Liouville function of an argument.
liouvilleA :: Num a => ArithmeticFunction n a
liouvilleA = ArithmeticFunction (const $ Xor . odd) runXor

-- | See 'carmichaelA'.
carmichael :: (UniqueFactorisation n, Integral n) => n -> n
carmichael = runFunction carmichaelA
{-# SPECIALIZE carmichael :: Int     -> Int #-}
{-# SPECIALIZE carmichael :: Word    -> Word #-}
{-# SPECIALIZE carmichael :: Integer -> Integer #-}
{-# SPECIALIZE carmichael :: Natural -> Natural #-}

-- | Calculates the Carmichael function for a positive integer, that is,
--   the (smallest) exponent of the group of units in @&#8484;/(n)@.
carmichaelA :: Integral n => ArithmeticFunction n n
carmichaelA = ArithmeticFunction (\p -> LCM . f (unPrime p)) getLCM
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

-- | See 'smallOmegaA'.
smallOmega :: (UniqueFactorisation n, Num a) => n -> a
smallOmega = runFunction smallOmegaA

-- | Number of distinct prime factors.
--
-- > smallOmegaA = additive (\_ _ -> 1)
smallOmegaA :: Num a => ArithmeticFunction n a
smallOmegaA = additive $ const $ const 1

-- | See 'bigOmegaA'.
bigOmega :: UniqueFactorisation n => n -> Word
bigOmega = runFunction bigOmegaA

-- | Number of prime factors, counted with multiplicity.
--
-- > bigOmegaA = additive (\_ k -> k)
bigOmegaA :: ArithmeticFunction n Word
bigOmegaA = additive $ const id

-- | See 'expMangoldtA'.
expMangoldt :: UniqueFactorisation n => n -> n
expMangoldt = runFunction expMangoldtA

-- | The exponent of von Mangoldt function. Use @log expMangoldtA@ to recover von Mangoldt function itself.
expMangoldtA :: Num n => ArithmeticFunction n n
expMangoldtA = ArithmeticFunction (const . MangoldtOne . unPrime) runMangoldt

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

-- | See 'isNFreeA'.
isNFree :: UniqueFactorisation n => Word -> n -> Bool
isNFree n = runFunction (isNFreeA n)

-- | Check if an integer is @n@-free. An integer @x@ is @n@-free if in its
-- factorisation into prime factors, no factor has an exponent larger than or
-- equal to @n@.
isNFreeA :: Word -> ArithmeticFunction n Bool
isNFreeA n = ArithmeticFunction (\_ pow -> All $ pow < n) getAll

newtype LCM a = LCM { getLCM :: a }

instance Integral a => Semigroup (LCM a) where
  (<>) = coerce (lcm :: a -> a -> a)

instance Integral a => Monoid (LCM a) where
  mempty  = LCM 1

newtype Xor = Xor { _getXor :: Bool }

runXor :: Num a => Xor -> a
runXor m = case m of
  Xor False ->  1
  Xor True  -> -1

instance Semigroup Xor where
   (<>) = coerce ((/=) :: Bool -> Bool -> Bool)

instance Monoid Xor where
  mempty  = Xor False

newtype SetProduct a = SetProduct { getSetProduct :: Set a }

instance (Num a, Ord a) => Semigroup (SetProduct a) where
  SetProduct s1 <> SetProduct s2 = SetProduct $ s1 <> s2 <> foldMap (\n -> S.mapMonotonic (* n) s2) s1

instance (Num a, Ord a) => Monoid (SetProduct a) where
  mempty  = SetProduct mempty

newtype ListProduct a = ListProduct { getListProduct :: [a] }

instance Num a => Semigroup (ListProduct a) where
  ListProduct s1 <> ListProduct s2 = ListProduct $ s1 <> s2 <> foldMap (\n -> map (* n) s2) s1

instance Num a => Monoid (ListProduct a) where
  mempty  = ListProduct mempty

-- Represent as a Reader monad
newtype BoundedSetProduct a = BoundedSetProduct { _getBoundedSetProduct :: a -> Set a }

takeWhileLE :: Ord a => a -> Set a -> Set a
takeWhileLE b xs = if m then S.insert b ls else ls
  where (ls, m, _) = S.splitMember b xs

instance (Ord a, Num a) => Semigroup (BoundedSetProduct a) where
  BoundedSetProduct f1 <> BoundedSetProduct f2 = BoundedSetProduct f
    where f b = s1 <> s2 <> foldMap (\n -> takeWhileLE b $ S.mapMonotonic (* n) s2) s1
            where s1 = f1 b
                  s2 = f2 b

instance (Ord a, Num a) => Monoid (BoundedSetProduct a) where
  mempty = BoundedSetProduct mempty

newtype IntSetProduct = IntSetProduct { getIntSetProduct :: IntSet }

instance Semigroup IntSetProduct where
  IntSetProduct s1 <> IntSetProduct s2 = IntSetProduct $ IS.unions $ s1 : s2 : map (\n -> IS.map (* n) s2) (IS.toAscList s1)

instance Monoid IntSetProduct where
  mempty  = IntSetProduct mempty
