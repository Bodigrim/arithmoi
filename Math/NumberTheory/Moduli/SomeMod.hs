-- |
-- Module:      Math.NumberTheory.Moduli.SomeMod
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Safe modular arithmetic with modulo on type level.
--

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
import Data.Semiring (Semiring(..), Ring(..))
import Data.Type.Equality
import GHC.TypeNats (KnownNat, SomeNat(..), sameNat, natVal, someNatVal)
import Numeric.Natural

-- | This type represents residues with unknown modulo and rational numbers.
-- One can freely combine them in arithmetic expressions, but each operation
-- will spend time on modulo's recalculation:
--
-- >>> 2 `modulo` 10 + 4 `modulo` 15
-- (1 `modulo` 5)
-- >>> (2 `modulo` 10) * (4 `modulo` 15)
-- (3 `modulo` 5)
-- >>> import Data.Ratio
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
    natVal mx == natVal my && unMod mx == unMod my
  InfMod rx  == InfMod ry  = rx == ry
  _          == _          = False

instance Ord SomeMod where
  SomeMod mx `compare` SomeMod my =
    natVal mx `compare` natVal my <> unMod mx `compare` unMod my
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
    x = unMod mx
    y = unMod my
    m = natVal mx `Prelude.gcd` natVal my

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
  (-)    = liftBinOp (-) (-)
  negate = liftUnOp Prelude.negate Prelude.negate
  {-# INLINE negate #-}
  (*)    = liftBinOp (*) (*)
  abs    = id
  {-# INLINE abs #-}
  signum = const 1
  {-# INLINE signum #-}
  fromInteger = InfMod . fromInteger
  {-# INLINE fromInteger #-}

instance Semiring SomeMod where
  plus  = (+)
  times = (*)
  zero  = InfMod 0
  one   = InfMod 1
  fromNatural = fromIntegral

instance Ring SomeMod where
  negate = Prelude.negate

-- | Beware that division by residue, which is not coprime with the modulo,
-- will result in runtime error. Consider using 'invertSomeMod' instead.
instance Fractional SomeMod where
  fromRational = InfMod
  {-# INLINE fromRational #-}
  recip x = case invertSomeMod x of
    Nothing -> error "recip{SomeMod}: residue is not coprime with modulo"
    Just y  -> y

-- | See the warning about division above.
instance GcdDomain SomeMod where
  divide x y = Just (x / y)
  gcd        = const $ const 1
  lcm        = const $ const 1
  coprime    = const $ const True

-- | See the warning about division above.
instance Euclidean SomeMod where
  degree      = const 0
  quotRem x y = (x / y, 0)
  quot        = (/)
  rem         = const $ const 0

-- | See the warning about division above.
instance Field SomeMod

-- | Computes the inverse value, if it exists.
--
-- >>> invertSomeMod (3 `modulo` 10) -- because 3 * 7 = 1 :: Mod 10
-- Just (7 `modulo` 10)
-- >>> invertSomeMod (4 `modulo` 10)
-- Nothing
-- >>> import Data.Ratio
-- >>> invertSomeMod (fromRational (2 % 5))
-- Just 5 % 2
invertSomeMod :: SomeMod -> Maybe SomeMod
invertSomeMod = \case
  SomeMod m -> fmap SomeMod (invertMod m)
  InfMod  r -> Just (InfMod (recip r))
{-# INLINABLE [1] invertSomeMod #-}

{-# SPECIALISE [1] powSomeMod :: SomeMod -> Integer -> SomeMod #-}
{-# SPECIALISE [1] powSomeMod :: SomeMod -> Natural -> SomeMod #-}
{-# SPECIALISE [1] powSomeMod :: SomeMod -> Int     -> SomeMod #-}
{-# SPECIALISE [1] powSomeMod :: SomeMod -> Word    -> SomeMod #-}

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
