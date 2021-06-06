-- |
-- Module:      Math.NumberTheory.RootsOfUnity
-- Copyright:   (c) 2018 Bhavik Mehta
-- Licence:     MIT
-- Maintainer:  Bhavik Mehta <bhavikmehta8@gmail.com>
--
-- Implementation of roots of unity
--


module Math.NumberTheory.RootsOfUnity
(  
-- * Roots of unity
   RootOfUnity (..)
-- ** Conversions
   , toRootOfUnity
   , toComplex )

where

import Data.Complex                                        (Complex(..), cis)
import Data.Semigroup                                      (Semigroup(..))
import Data.Ratio                                          ((%), numerator, denominator)

-- | A representation of <https://en.wikipedia.org/wiki/Root_of_unity roots of unity>: complex
-- numbers \(z\) for which there is \(n\) such that \(z^n=1\).
newtype RootOfUnity =
  RootOfUnity { -- | Every root of unity can be expressed as \(e^{2 \pi i q}\) for some
                -- rational \(q\) satisfying \(0 \leq q < 1\), this function extracts it.
                fromRootOfUnity :: Rational }
  deriving (Eq)

instance Show RootOfUnity where
  show (RootOfUnity q)
    | n == 0    = "1"
    | d == 1    = "-1"
    | n == 1    = "e^(πi/" ++ show d ++ ")"
    | otherwise = "e^(" ++ show n ++ "πi/" ++ show d ++ ")"
    where n = numerator (2*q)
          d = denominator (2*q)

-- | Given a rational \(q\), produce the root of unity \(e^{2 \pi i q}\).
toRootOfUnity :: Rational -> RootOfUnity
toRootOfUnity q = RootOfUnity ((n `rem` d) % d)
  where n = numerator q
        d = denominator q
        -- effectively q `mod` 1
  -- This smart constructor ensures that the rational is always in the range 0 <= q < 1.

-- | This Semigroup is in fact a group, so @'stimes'@ can be called with a negative first argument.
instance Semigroup RootOfUnity where
  RootOfUnity q1 <> RootOfUnity q2 = toRootOfUnity (q1 + q2)
  stimes k (RootOfUnity q) = toRootOfUnity (q * (toInteger k % 1))

instance Monoid RootOfUnity where
  mappend = (<>)
  mempty = RootOfUnity 0

-- | Convert a root of unity into an inexact complex number. Due to floating point inaccuracies,
-- it is recommended to avoid use of this until the end of a calculation. Alternatively, with
-- the [cyclotomic](http://hackage.haskell.org/package/cyclotomic-0.5.1) package, one can use
-- @[polarRat](https://hackage.haskell.org/package/cyclotomic-0.5.1/docs/Data-Complex-Cyclotomic.html#v:polarRat)
-- 1 . @'fromRootOfUnity' to convert to a cyclotomic number.
toComplex :: Floating a => RootOfUnity -> Complex a
toComplex (RootOfUnity t)
  | t == 1/2 = (-1) :+ 0
  | t == 1/4 = 0 :+ 1
  | t == 3/4 = 0 :+ (-1)
  | otherwise = cis . (2*pi*) . fromRational $ t
