-- |
-- Module:      Math.NumberTheory.Moduli.SomeMod
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Safe modular arithmetic with modulo on type level.
--

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.Moduli.SomeMod
  ( SomeMod(..)
  , modulo
  , invertSomeMod
  , powSomeMod
  ) where

import Data.Euclidean (GcdDomain(..), Euclidean(..), Field)
import Data.Mod
import Data.Proxy
#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup
#endif
import Data.Semiring (Semiring(..), Ring(..))
import Data.Type.Equality
import GHC.TypeNats.Compat
import Numeric.Natural

-- | This type represents residues with unknown modulo and rational numbers.
-- One can freely combine them in arithmetic expressions, but each operation
-- will spend time on modulo's recalculation:
--
-- >>> 2 `modulo` 10 + 4 `modulo` 15
-- (1 `modulo` 5)
-- >>> (2 `modulo` 10) * (4 `modulo` 15)
-- (3 `modulo` 5)
-- >>> 2 `modulo` 10 + fromRational (3 % 7)
-- (1 `modulo` 10)
-- >>> 2 `modulo` 10 * fromRational (3 % 7)
-- (8 `modulo` 10)
--
-- If performance is crucial, it is recommended to extract @Mod m@ for further processing
-- by pattern matching. E. g.,
--
-- > case modulo n m of
-- >   SomeMod k -> process k -- Here k has type Mod m
-- >   InfMod{}  -> error "impossible"
data SomeMod where
  SomeMod :: KnownNat m => Mod m -> SomeMod

instance Eq SomeMod where
  SomeMod mx == SomeMod my =
    natVal mx == natVal my && unMod mx == unMod my

instance Ord SomeMod where
  SomeMod mx `compare` SomeMod my =
    natVal mx `compare` natVal my <> unMod mx `compare` unMod my

instance Show SomeMod where
  show (SomeMod m) = show m

-- | Create modular value by representative of residue class and modulo.
-- One can use the result either directly (via functions from 'Num' and 'Fractional'),
-- or deconstruct it by pattern matching. Note that 'modulo' never returns 'InfMod'.
modulo :: Integer -> Natural -> SomeMod
modulo n m = case someNatVal m of
  SomeNat (_ :: Proxy t) -> SomeMod (fromInteger n :: Mod t)
{-# INLINABLE modulo #-}
infixl 7 `modulo`

liftUnOp
  :: (forall k. KnownNat k => Mod k -> Mod k)
  -> (Rational -> Rational)
  -> Either SomeMod Rational
  -> Either SomeMod Rational
liftUnOp fm fr = \case
  Left (SomeMod m) -> Left (SomeMod (fm m))
  Right r -> Right (fr r)
{-# INLINEABLE liftUnOp #-}

liftBinOpMod
  :: (KnownNat m, KnownNat n)
  => (forall k. KnownNat k => Mod k -> Mod k -> Mod k)
  -> Mod m
  -> Mod n
  -> SomeMod
liftBinOpMod f mx my = case someNatVal m of
  SomeNat (_ :: Proxy t) ->
    SomeMod (fromIntegral (x `mod` m) `f` fromIntegral (y `mod` m) :: Mod t)
  where
    x = unMod mx
    y = unMod my
    m = natVal mx `Prelude.gcd` natVal my

liftBinOp
  :: (forall k. KnownNat k => Mod k -> Mod k -> Mod k)
  -> (Rational -> Rational -> Rational)
  -> Either SomeMod Rational
  -> Either SomeMod Rational
  -> Either SomeMod Rational
liftBinOp _ fr (Right rx) (Right ry) = Right  (rx `fr` ry)
liftBinOp fm _ (Right rx) (Left (SomeMod my)) = Left (SomeMod (fromRational rx `fm` my))
liftBinOp fm _ (Left (SomeMod mx)) (Right ry) = Left (SomeMod (mx `fm` fromRational ry))
liftBinOp fm _ (Left (SomeMod (mx :: Mod m))) (Left (SomeMod (my :: Mod n)))
  = Left $ case (Proxy :: Proxy m) `sameNat` (Proxy :: Proxy n) of
    Nothing   -> liftBinOpMod fm mx my
    Just Refl -> SomeMod (mx `fm` my)

instance Num (Either SomeMod Rational) where
  (+)    = liftBinOp (+) (+)
  (-)    = liftBinOp (-) (-)
  negate = liftUnOp Prelude.negate Prelude.negate
  {-# INLINE negate #-}
  (*)    = liftBinOp (*) (*)
  abs    = id
  {-# INLINE abs #-}
  signum = const 1
  {-# INLINE signum #-}
  fromInteger = Right . fromInteger
  {-# INLINE fromInteger #-}

instance Semiring (Either SomeMod Rational) where
  plus  = (+)
  times = (*)
  zero  = Right 0
  one   = Right 1
  fromNatural = Right . fromIntegral

instance Ring (Either SomeMod Rational) where
  negate = Prelude.negate

-- | Beware that division by residue, which is not coprime with the modulo,
-- will result in runtime error. Consider using 'invertSomeMod' instead.
instance Fractional (Either SomeMod Rational) where
  fromRational = Right
  {-# INLINE fromRational #-}
  recip (Left x) = case invertSomeMod x of
    Nothing -> error $ "recip{SomeMod}: residue is not coprime with modulo"
    Just y  -> Left y
  recip (Right x) = Right (recip x)

-- | See the warning about division above.
instance GcdDomain (Either SomeMod Rational) where
  divide x y = Just (x / y)
  gcd        = const $ const 1
  lcm        = const $ const 1
  coprime    = const $ const True

-- | See the warning about division above.
instance Euclidean (Either SomeMod Rational) where
  degree      = const 0
  quotRem x y = (x / y, 0)
  quot        = (/)
  rem         = const $ const 0

-- | See the warning about division above.
instance Field (Either SomeMod Rational)

-- | Computes the inverse value, if it exists.
--
-- >>> invertSomeMod (3 `modulo` 10)
-- Just (7 `modulo` 10) -- because 3 * 7 = 1 :: Mod 10
-- >>> invertSomeMod (4 `modulo` 10)
-- Nothing
-- >>> invertSomeMod (fromRational (2 % 5))
-- Just 5 % 2
invertSomeMod :: SomeMod -> Maybe SomeMod
invertSomeMod (SomeMod m) = fmap SomeMod (invertMod m)
{-# INLINABLE [1] invertSomeMod #-}

{-# SPECIALISE [1] powSomeMod ::
  SomeMod -> Integer -> SomeMod,
  SomeMod -> Natural -> SomeMod,
  SomeMod -> Int     -> SomeMod,
  SomeMod -> Word    -> SomeMod #-}

-- | Drop-in replacement for 'Prelude.^', with much better performance.
-- When -O is enabled, there is a rewrite rule, which specialises 'Prelude.^' to 'powSomeMod'.
--
-- >>> powSomeMod (3 `modulo` 10) 4
-- (1 `modulo` 10)
powSomeMod :: Integral a => SomeMod -> a -> SomeMod
powSomeMod (SomeMod m) a = SomeMod (m ^% a)
{-# INLINABLE [1] powSomeMod #-}

{-# RULES "^%SomeMod" forall x p. x ^ p = powSomeMod x p #-}
