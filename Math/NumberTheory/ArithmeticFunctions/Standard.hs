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
    divisors
  , divisorsList
  , divisorsSmall
  , divisorsTo
    -- * Multiplicative functions
  , multiplicative
  , tau
  , sigma
  , totient
  , jordan
  , ramanujan
  , moebius, Moebius(..), runMoebius
  , liouville
    -- * Additive functions
  , additive
  , smallOmega
  , bigOmega
    -- * Misc
  , carmichael
  , expMangoldt, runMangoldt
  , isNFree, nFrees, nFreesBlock
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
multiplicative f = ArithmeticFunction 1 f (*)

-- | The set of all (positive) divisors of an argument.
divisors :: (Ord n, Num n) => ArithmeticFunction n (Set n)
divisors = ArithmeticFunction (S.singleton 1) (divisorsHelper . unPrime) undefined
{-# SPECIALIZE divisors :: ArithmeticFunction Natural (Set Natural) #-}
{-# SPECIALIZE divisors :: ArithmeticFunction Integer (Set Integer) #-}

divisorsHelper :: Num n => n -> Word -> Set n
divisorsHelper _ 0 = S.singleton 1
divisorsHelper p 1 = S.fromDistinctAscList [1, p]
divisorsHelper p a = S.fromDistinctAscList $ 1 : p : p * p : map (p ^) [3 .. wordToInt a]
{-# INLINE divisorsHelper #-}

-- | The unsorted list of all (positive) divisors of an argument, produced in lazy fashion.
divisorsList :: Num n => ArithmeticFunction n [n]
divisorsList = ArithmeticFunction [1] (divisorsListHelper . unPrime) undefined

divisorsListHelper :: Num n => n -> Word -> [n]
divisorsListHelper _ 0 = []
divisorsListHelper p 1 = [p]
divisorsListHelper p a = p : p * p : map (p ^) [3 .. wordToInt a]
{-# INLINE divisorsListHelper #-}

-- | Same as 'divisors', but with better performance on cost of type restriction.
divisorsSmall :: ArithmeticFunction Int IntSet
divisorsSmall = ArithmeticFunction (IS.singleton 1) (divisorsHelperSmall . unPrime) undefined

divisorsHelperSmall :: Int -> Word -> IntSet
divisorsHelperSmall _ 0 = IS.singleton 1
divisorsHelperSmall p 1 = IS.fromDistinctAscList [1, p]
divisorsHelperSmall p a = IS.fromDistinctAscList $ 1 : p : p * p : map (p ^) [3 .. wordToInt a]
{-# INLINE divisorsHelperSmall #-}

-- | The set of all (positive) divisors up to an inclusive bound.
divisorsTo :: (UniqueFactorisation n, Integral n) => n -> ArithmeticFunction n (Set n)
divisorsTo _to = undefined undefined undefined

-- | Generate at most @a@ powers of @p@ up to an inclusive bound @b@.
divisorsToHelper :: (Ord n, Num n) => n -> n -> Word -> Set n
divisorsToHelper _ _ 0 = S.empty
divisorsToHelper b p 1 = if p <= b then S.singleton p else S.empty
divisorsToHelper b p a = S.fromDistinctAscList $ take (wordToInt a) $ takeWhile (<=b) $ iterate (p*) p
{-# INLINE divisorsToHelper #-}

-- | The number of (positive) divisors of an argument.
--
-- > tau = multiplicative (\_ k -> k + 1)
--
-- >>> map tau [1..10]
-- [1,2,2,3,2,4,2,4,3,4]
tau :: Num a => ArithmeticFunction n a
tau = multiplicative $ const (fromIntegral . succ)

-- | The sum of the @k@-th powers of (positive) divisors of an argument.
--
-- > sigma = multiplicative (\p k -> sum $ map (p ^) [0..k])
-- > sigma 0 = tau
sigma :: (Integral n, Num a, GcdDomain a) => Word -> ArithmeticFunction n a
sigma 0 = tau
sigma 1 = multiplicative $ sigmaHelper . fromIntegral . unPrime
sigma a = multiplicative $ sigmaHelper . (^ wordToInt a) . fromIntegral . unPrime
{-# INLINABLE sigma #-}

sigmaHelper :: (Num a, GcdDomain a) => a -> Word -> a
sigmaHelper pa 1 = pa + 1
sigmaHelper pa 2 = pa * pa + pa + 1
sigmaHelper pa k = fromJust ((pa ^ wordToInt (k + 1) - 1) `divide` (pa - 1))
{-# INLINE sigmaHelper #-}

-- | Calculates the totient of a positive number @n@, i.e.
--   the number of @k@ with @1 <= k <= n@ and @'gcd' n k == 1@,
--   in other words, the order of the group of units in @&#8484;/(n)@.
totient :: Num n => ArithmeticFunction n n
totient = multiplicative $ jordanHelper . unPrime
{-# INLINABLE totient #-}

-- | Calculates the k-th Jordan function of an argument.
--
-- > jordan 1 = totient
jordan :: Num n => Word -> ArithmeticFunction n n
jordan 0 = multiplicative $ \_ _ -> 0
jordan 1 = totient
jordan a = multiplicative $ jordanHelper . (^ wordToInt a) . unPrime

jordanHelper :: Num n => n -> Word -> n
jordanHelper pa 1 = pa - 1
jordanHelper pa 2 = (pa - 1) * pa
jordanHelper pa k = (pa - 1) * pa ^ wordToInt (k - 1)
{-# INLINE jordanHelper #-}

-- | Calculates the <https://en.wikipedia.org/wiki/Ramanujan_tau_function Ramanujan tau function>
--   of a positive number @n@, using formulas given <http://www.numbertheory.org/php/tau.html here>
ramanujan :: ArithmeticFunction Integer Integer
ramanujan = multiplicative $ ramanujanHelper . unPrime

ramanujanHelper :: Integer -> Word -> Integer
ramanujanHelper _ 0 = 1
ramanujanHelper 2 1 = -24
ramanujanHelper p 1 = (65 * sigmaHelper (p ^ (11 :: Int)) 1 + 691 * sigmaHelper (p ^ (5 :: Int)) 1 - 691 * 252 * 2 * sum [(sigma 5 $^ k) * (sigma 5 $^ (p-k)) | k <- [1..(p `quot` 2)]]) `quot` 756
ramanujanHelper p k = sum $ zipWith3 (\a b c -> a * b * c) paPowers tpPowers binomials
  where pa = p ^ (11 :: Int)
        tp = ramanujanHelper p 1
        paPowers = iterate (* (-pa)) 1
        binomials = scanl (\acc j -> acc * (k' - 2 * j) * (k' - 2 * j - 1) `quot` (k' - j) `quot` (j + 1)) 1 [0 .. k' `quot` 2 - 1]
        k' = fromIntegral k
        tpPowers = reverse $ take (length binomials) $ iterate (* tp^(2::Int)) (if even k then 1 else tp)
{-# INLINE ramanujanHelper #-}

-- | Calculates the Möbius function of an argument.
moebius :: ArithmeticFunction n Moebius
moebius = ArithmeticFunction mempty (const f) (<>)
  where
    f 1 = MoebiusN
    f 0 = MoebiusP
    f _ = MoebiusZ

-- | Calculates the Liouville function of an argument.
liouville :: ArithmeticFunction n Bool
liouville = ArithmeticFunction False (const odd) (/=)

-- | Calculates the Carmichael function for a positive integer, that is,
--   the (smallest) exponent of the group of units in @&#8484;/(n)@.
carmichael :: Integral n => ArithmeticFunction n n
carmichael = ArithmeticFunction 1 (f . unPrime) lcm
  where
    f 2 1 = 1
    f 2 2 = 2
    f 2 k = 2 ^ wordToInt (k - 2)
    f p 1 = p - 1
    f p 2 = (p - 1) * p
    f p k = (p - 1) * p ^ wordToInt (k - 1)
{-# SPECIALIZE carmichael :: ArithmeticFunction Int     Int #-}
{-# SPECIALIZE carmichael :: ArithmeticFunction Word    Word #-}
{-# SPECIALIZE carmichael :: ArithmeticFunction Integer Integer #-}
{-# SPECIALIZE carmichael :: ArithmeticFunction Natural Natural #-}

-- | Create an additive function from the function on prime's powers. See examples below.
additive :: Num a => (Prime n -> Word -> a) -> ArithmeticFunction n a
additive f = ArithmeticFunction 1 f (+)

-- | Number of distinct prime factors.
--
-- > smallOmega = additive (\_ _ -> 1)
smallOmega :: Num a => ArithmeticFunction n a
smallOmega = additive $ const $ const 1

-- | Number of prime factors, counted with multiplicity.
--
-- > bigOmega = additive (\_ k -> k)
bigOmega :: ArithmeticFunction n Word
bigOmega = additive $ const id

-- | The exponent of von Mangoldt function.
expMangoldt :: Num n => ArithmeticFunction n (Mangoldt n)
expMangoldt = ArithmeticFunction mempty (const . MangoldtOne . unPrime) (<>)

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

-- | Check if an integer is @n@-free. An integer @x@ is @n@-free if in its
-- factorisation into prime factors, no factor has an exponent larger than or
-- equal to @n@.
isNFree :: Word -> ArithmeticFunction n Bool
isNFree n = ArithmeticFunction True (\_ pow -> pow < n) (&&)

newtype SetProduct a = SetProduct { getSetProduct :: Set a }

instance (Num a, Ord a) => Semigroup (SetProduct a) where
  SetProduct s1 <> SetProduct s2 = SetProduct $ s1 <> s2 <> foldMap (\n -> S.mapMonotonic (* n) s2) s1

instance (Num a, Ord a) => Monoid (SetProduct a) where
  mempty  = SetProduct mempty
  mappend = (<>)

newtype ListProduct a = ListProduct { getListProduct :: [a] }

instance Num a => Semigroup (ListProduct a) where
  ListProduct s1 <> ListProduct s2 = ListProduct $ s1 <> s2 <> foldMap (\n -> map (* n) s2) s1

instance Num a => Monoid (ListProduct a) where
  mempty  = ListProduct mempty
  mappend = (<>)

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
  mappend = (<>)

newtype IntSetProduct = IntSetProduct { getIntSetProduct :: IntSet }

instance Semigroup IntSetProduct where
  IntSetProduct s1 <> IntSetProduct s2 = IntSetProduct $ IS.unions $ s1 : s2 : map (\n -> IS.map (* n) s2) (IS.toAscList s1)

instance Monoid IntSetProduct where
  mempty  = IntSetProduct mempty
  mappend = (<>)
