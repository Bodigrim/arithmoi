-- |
-- Module:      Math.NumberTheory.Moduli.Class
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Safe modular arithmetic with modulo on type level.
--

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UnboxedTuples              #-}

module Math.NumberTheory.Moduli.Class
  ( -- * Known modulo
    Mod
  , getVal
  , getNatVal
  , getMod
  , getNatMod
  , invertMod
  , powMod
  , (^%)
  -- * Multiplicative group
  , MultMod
  , multElement
  , isMultElement
  , invertGroup
  -- * Unknown modulo
  , SomeMod(..)
  , modulo
  , invertSomeMod
  , powSomeMod
  -- * Re-exported from GHC.TypeNats.Compat
  , KnownNat
  ) where

import Data.Mod
import Data.Proxy
import Data.Semigroup
import Data.Type.Equality
import GHC.Natural (Natural(..))
import GHC.TypeNats.Compat

-- | This type represents elements of the multiplicative group mod m, i.e.
-- those elements which are coprime to m. Use @toMultElement@ to construct.
newtype MultMod m = MultMod {
  multElement :: Mod m -- ^ Unwrap a residue.
  } deriving (Eq, Ord, Show)

instance KnownNat m => Semigroup (MultMod m) where
  MultMod a <> MultMod b = MultMod (a * b)
  stimes k a@(MultMod a')
    | k >= 0 = MultMod (powMod a' k)
    | otherwise = invertGroup $ stimes (-k) a
  -- ^ This Semigroup is in fact a group, so @stimes@ can be called with a negative first argument.

instance KnownNat m => Monoid (MultMod m) where
  mempty = MultMod 1
  mappend = (<>)

instance KnownNat m => Bounded (MultMod m) where
  minBound = MultMod 1
  maxBound = MultMod (-1)

-- | Attempt to construct a multiplicative group element.
isMultElement :: KnownNat m => Mod m -> Maybe (MultMod m)
isMultElement a = if getNatVal a `gcd` getNatMod a == 1
                     then Just $ MultMod a
                     else Nothing

-- | For elements of the multiplicative group, we can safely perform the inverse
-- without needing to worry about failure.
invertGroup :: KnownNat m => MultMod m -> MultMod m
invertGroup (MultMod a) = case invertMod a of
                            Just b -> MultMod b
                            Nothing -> error "Math.NumberTheory.Moduli.invertGroup: failed to invert element"

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
  InfMod  :: Rational -> SomeMod

instance Eq SomeMod where
  SomeMod mx == SomeMod my =
    getMod mx == getMod my && getVal mx == getVal my
  InfMod rx  == InfMod ry  = rx == ry
  _          == _          = False

instance Ord SomeMod where
  SomeMod mx `compare` SomeMod my =
    getMod mx `compare` getMod my <> getVal mx `compare` getVal my
  SomeMod{} `compare` InfMod{} = LT
  InfMod{} `compare` SomeMod{} = GT
  InfMod rx `compare` InfMod ry = rx `compare` ry

instance Show SomeMod where
  show = \case
    SomeMod m -> show m
    InfMod  r -> show r

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
  -> SomeMod
  -> SomeMod
liftUnOp fm fr = \case
  SomeMod m -> SomeMod (fm m)
  InfMod  r -> InfMod  (fr r)
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
    x = getNatVal mx
    y = getNatVal my
    m = natVal mx `gcd` natVal my

liftBinOp
  :: (forall k. KnownNat k => Mod k -> Mod k -> Mod k)
  -> (Rational -> Rational -> Rational)
  -> SomeMod
  -> SomeMod
  -> SomeMod
liftBinOp _ fr (InfMod rx)  (InfMod ry)  = InfMod  (rx `fr` ry)
liftBinOp fm _ (InfMod rx)  (SomeMod my) = SomeMod (fromRational rx `fm` my)
liftBinOp fm _ (SomeMod mx) (InfMod ry)  = SomeMod (mx `fm` fromRational ry)
liftBinOp fm _ (SomeMod (mx :: Mod m)) (SomeMod (my :: Mod n))
  = case (Proxy :: Proxy m) `sameNat` (Proxy :: Proxy n) of
    Nothing   -> liftBinOpMod fm mx my
    Just Refl -> SomeMod (mx `fm` my)

instance Num SomeMod where
  (+)    = liftBinOp (+) (+)
  (-)    = liftBinOp (-) (+)
  negate = liftUnOp negate negate
  {-# INLINE negate #-}
  (*)    = liftBinOp (*) (*)
  abs    = id
  {-# INLINE abs #-}
  signum = const 1
  {-# INLINE signum #-}
  fromInteger = InfMod . fromInteger
  {-# INLINE fromInteger #-}

-- | Beware that division by residue, which is not coprime with the modulo,
-- will result in runtime error. Consider using 'invertSomeMod' instead.
instance Fractional SomeMod where
  fromRational = InfMod
  {-# INLINE fromRational #-}
  recip x = case invertSomeMod x of
    Nothing -> error $ "recip{SomeMod}: residue is not coprime with modulo"
    Just y  -> y

-- | Computes the inverse value, if it exists.
--
-- >>> invertSomeMod (3 `modulo` 10)
-- Just (7 `modulo` 10) -- because 3 * 7 = 1 :: Mod 10
-- >>> invertSomeMod (4 `modulo` 10)
-- Nothing
-- >>> invertSomeMod (fromRational (2 % 5))
-- Just 5 % 2
invertSomeMod :: SomeMod -> Maybe SomeMod
invertSomeMod = \case
  SomeMod m -> fmap SomeMod (invertMod m)
  InfMod  r -> Just (InfMod (recip r))
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
powSomeMod (InfMod  r) a = InfMod  (r ^  a)
{-# INLINABLE [1] powSomeMod #-}

{-# RULES "^%SomeMod" forall x p. x ^ p = powSomeMod x p #-}
