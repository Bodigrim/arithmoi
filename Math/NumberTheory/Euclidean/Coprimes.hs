-- |
-- Module:      Math.NumberTheory.Euclidean.Coprimes
-- Copyright:   (c) 2017-2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Container for pairwise coprime numbers.

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Math.NumberTheory.Euclidean.Coprimes
  ( splitIntoCoprimes
  , Coprimes
  , unCoprimes
  , singleton
  , insert
  ) where

import Prelude hiding (gcd, quot, rem)
import Data.Coerce
import Data.Euclidean
import Data.List (tails, mapAccumL)
import Data.Maybe
#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup
#endif
import Data.Semiring (Semiring(..), isZero)

-- | A list of pairwise coprime numbers
-- with their multiplicities.
newtype Coprimes a b = Coprimes {
  unCoprimes :: [(a, b)] -- ^ Unwrap.
  }
  deriving (Eq, Show)

unsafeDivide :: GcdDomain a => a -> a -> a
unsafeDivide x y = case x `divide` y of
  Nothing -> error "violated prerequisite of unsafeDivide"
  Just z  -> z

-- | Check whether an element is a unit of the ring.
isUnit :: (Eq a, GcdDomain a) => a -> Bool
isUnit x = not (isZero x) && isJust (one `divide` x)

doPair :: (Eq a, GcdDomain a, Eq b, Num b) => a -> b -> a -> b -> (a, a, [(a, b)])
doPair x xm y ym
  | isUnit g  = (x, y, [])
  | otherwise = (x', y', concat rests)
    where
      g = gcd x y

      (x', g', xgs) = doPair (x `unsafeDivide` g) xm g (xm + ym)
      xgs' = if isUnit g' then xgs else ((g', xm + ym) : xgs)

      (y', rests) = mapAccumL go (y `unsafeDivide` g) xgs'
      go w (t, tm) = (w', if isUnit t' then acc else (t', tm) : acc)
        where
          (w', t', acc) = doPair w ym t tm

_propDoPair :: (Eq a, Num a, GcdDomain a, Integral b) => a -> b -> a -> b -> Bool
_propDoPair x xm y ym
  =  isJust (x `divide` x')
  && isJust (y `divide` y')
  && coprime x' y'
  && all (coprime x') (map fst rest)
  && all (coprime y') (map fst rest)
  && all (not . isUnit) (map fst rest)
  && and [ coprime s t | (s, _) : ts <- tails rest, (t, _) <- ts ]
  && abs ((x ^ xm) * (y ^ ym)) == abs ((x' ^ xm) * (y' ^ ym) * product (map (\(r, k) -> r ^ k) rest))
  where
    (x', y', rest) = doPair x xm y ym

insertInternal
  :: forall a b.
     (Eq a, GcdDomain a, Eq b, Num b)
  => a
  -> b
  -> Coprimes a b
  -> (Coprimes a b, Coprimes a b)
insertInternal xx xm
  | isZero xx && xm == 0 = (, Coprimes [])
  | isZero xx            = const (Coprimes [(zero, 1)], Coprimes [])
  | otherwise            = coerce (go ([], []) xx)
  where
    go :: ([(a, b)], [(a, b)]) -> a -> [(a, b)] -> ([(a, b)], [(a, b)])
    go (old, new) x rest
      | isUnit x = (rest ++ old, new)
    go (old, new) x [] = (old, (x, xm) : new)
    go _ _ ((x, _) : _)
      | isZero x = ([(zero, 1)], [])
    go (old, new) x ((y, ym) : rest)
      | isUnit y' = go (old, xys ++ new) x' rest
      | otherwise = go ((y', ym) : old, xys ++ new) x' rest
      where
        (x', y', xys) = doPair x xm y ym

-- | Wrap a non-zero number with its multiplicity into 'Coprimes'.
--
-- >>> singleton 210 1
-- Coprimes {unCoprimes = [(210,1)]}
singleton :: (Eq a, GcdDomain a, Eq b, Num b) => a -> b -> Coprimes a b
singleton a b
  | isZero a && b == 0 = Coprimes []
  | isUnit a           = Coprimes []
  | otherwise          = Coprimes [(a, b)]

-- | Add a non-zero number with its multiplicity to 'Coprimes'.
--
-- >>> insert 360 1 (singleton 210 1)
-- Coprimes {unCoprimes = [(7,1),(5,2),(3,3),(2,4)]}
-- >>> insert 2 4 (insert 7 1 (insert 5 2 (singleton 4 3)))
-- Coprimes {unCoprimes = [(7,1),(5,2),(2,10)]}
insert :: (Eq a, GcdDomain a, Eq b, Num b) => a -> b -> Coprimes a b -> Coprimes a b
insert x xm ys = Coprimes $ unCoprimes zs <> unCoprimes ws
  where
    (zs, ws) = insertInternal x xm ys

instance (Eq a, GcdDomain a, Eq b, Num b) => Semigroup (Coprimes a b) where
  (Coprimes xs) <> ys = Coprimes $ unCoprimes zs <> foldMap unCoprimes wss
    where
      (zs, wss) = mapAccumL (\vs (x, xm) -> insertInternal x xm vs) ys xs

instance (Eq a, GcdDomain a, Eq b, Num b) => Monoid (Coprimes a b) where
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
splitIntoCoprimes :: (Eq a, GcdDomain a, Eq b, Num b) => [(a, b)] -> Coprimes a b
splitIntoCoprimes = foldl (\acc (x, xm) -> insert x xm acc) mempty
