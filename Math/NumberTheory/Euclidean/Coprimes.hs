-- |
-- Module:      Math.NumberTheory.Euclidean.Coprimes
-- Copyright:   (c) 2017-2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Container for pairwise coprime numbers.

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.Euclidean.Coprimes
  ( splitIntoCoprimes
  , Coprimes
  , unCoprimes
  , singleton
  , insert
  ) where

import Prelude hiding (gcd, quot, rem)
import Data.Coerce
import Data.List (tails, mapAccumL)
#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup
#endif

import Math.NumberTheory.Euclidean

-- | A list of pairwise coprime numbers
-- with their multiplicities.
newtype Coprimes a b = Coprimes {
  unCoprimes :: [(a, b)] -- ^ Unwrap.
  }
  deriving (Eq, Show)

doPair :: (Eq a, Num a, Euclidean a, Eq b, Num b) => a -> b -> a -> b -> (a, a, [(a, b)])
doPair x xm y ym = case gcd x y of
  1 -> (x, y, [])
  g -> (x', y', concat rests)
    where
      (x', g', xgs) = doPair (x `quot` g) xm g (xm + ym)
      xgs' = if g' == 1 then xgs else ((g', xm + ym) : xgs)

      (y', rests) = mapAccumL go (y `quot` g) xgs'
      go w (t, tm) = (w', if t' == 1 then acc else (t', tm) : acc)
        where
          (w', t', acc) = doPair w ym t tm

_propDoPair :: (Eq a, Num a, Euclidean a, Integral b) => a -> b -> a -> b -> Bool
_propDoPair x xm y ym
  =  x `rem` x' == 0
  && y `rem` y' == 0
  && coprime x' y'
  && all (coprime x') (map fst rest)
  && all (coprime y') (map fst rest)
  && all (/= 1) (map fst rest)
  && and [ coprime s t | (s, _) : ts <- tails rest, (t, _) <- ts ]
  && (x ^ xm) * (y ^ ym) == (x' ^ xm) * (y' ^ ym) * product (map (\(r, k) -> r ^ k) rest)
  where
    (x', y', rest) = doPair x xm y ym

insertInternal
  :: forall a b.
     (Eq a, Num a, Euclidean a, Eq b, Num b)
  => a
  -> b
  -> Coprimes a b
  -> (Coprimes a b, Coprimes a b)
insertInternal 0   _ = const (Coprimes [(0, 1)], Coprimes [])
insertInternal xx xm = coerce (go ([], []) xx)
  where
    go :: ([(a, b)], [(a, b)]) -> a -> [(a, b)] -> ([(a, b)], [(a, b)])
    go (old, new) 1 rest = (rest ++ old, new)
    go (old, new) x [] = (old, (x, xm) : new)
    go _ _ ((0, _) : _) = ([(0, 1)], [])
    go (old, new) x ((y, ym) : rest)
      | y' == 1   = go (old, xys ++ new) x' rest
      | otherwise = go ((y', ym) : old, xys ++ new) x' rest
      where
        (x', y', xys) = doPair x xm y ym

-- | Wrap a non-zero number with its multiplicity into 'Coprimes'.
--
-- >>> singleton 210 1
-- Coprimes {unCoprimes = [(210,1)]}
singleton :: (Eq a, Num a, Eq b, Num b) => a -> b -> Coprimes a b
singleton 0 0 = Coprimes []
singleton 1 _ = Coprimes []
singleton a b = Coprimes [(a, b)]

-- | Add a non-zero number with its multiplicity to 'Coprimes'.
--
-- >>> insert 360 1 (singleton 210 1)
-- Coprimes {unCoprimes = [(7,1),(5,2),(3,3),(2,4)]}
-- >>> insert 2 4 (insert 7 1 (insert 5 2 (singleton 4 3)))
-- Coprimes {unCoprimes = [(7,1),(5,2),(2,10)]}
insert :: (Eq a, Num a, Euclidean a, Eq b, Num b) => a -> b -> Coprimes a b -> Coprimes a b
insert x xm ys = Coprimes $ unCoprimes zs <> unCoprimes ws
  where
    (zs, ws) = insertInternal x xm ys

instance (Eq a, Num a, Euclidean a, Eq b, Num b) => Semigroup (Coprimes a b) where
  (Coprimes xs) <> ys = Coprimes $ unCoprimes zs <> foldMap unCoprimes wss
    where
      (zs, wss) = mapAccumL (\vs (x, xm) -> insertInternal x xm vs) ys xs

instance (Eq a, Num a, Euclidean a, Eq b, Num b) => Monoid (Coprimes a b) where
  mempty  = Coprimes []
  mappend = (<>)

-- | The input list is assumed to be a factorisation of some number
-- into a list of powers of (possibly, composite) non-zero factors. The output
-- list is a factorisation of the same number such that all factors
-- are coprime. Such transformation is crucial to continue factorisation
-- (lazily, in parallel or concurrent fashion) without
-- having to merge multiplicities of primes, which occurs more than in one
-- composite factor.
--
-- >>> splitIntoCoprimes [(140, 1), (165, 1)]
-- Coprimes {unCoprimes = [(28,1),(33,1),(5,2)]}
-- >>> splitIntoCoprimes [(360, 1), (210, 1)]
-- Coprimes {unCoprimes = [(7,1),(5,2),(3,3),(2,4)]}
splitIntoCoprimes :: (Eq a, Num a, Euclidean a, Eq b, Num b) => [(a, b)] -> Coprimes a b
splitIntoCoprimes = foldl (\acc (x, xm) -> insert x xm acc) mempty
