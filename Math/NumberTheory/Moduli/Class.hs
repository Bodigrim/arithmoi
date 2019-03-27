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

import Data.Proxy
import Data.Ratio
import Data.Semigroup
import Data.Type.Equality
import GHC.Exts
import GHC.Integer.GMP.Internals
import GHC.Natural (Natural(..), powModNatural)
import GHC.TypeNats.Compat

-- | Wrapper for residues modulo @m@.
--
-- @Mod 3 :: Mod 10@ stands for the class of integers, congruent to 3 modulo 10 (…−17, −7, 3, 13, 23…).
-- The modulo is stored on type level, so it is impossible, for example, to add up by mistake
-- residues with different moduli.
--
-- >>> :set -XDataKinds
-- >>> (3 :: Mod 10) + (4 :: Mod 12)
-- error: Couldn't match type ‘12’ with ‘10’...
-- >>> (3 :: Mod 10) + 8
-- (1 `modulo` 10)
--
-- Note that modulo cannot be negative.
newtype Mod (m :: Nat) = Mod Natural
  deriving (Eq, Ord, Enum)

instance KnownNat m => Show (Mod m) where
  show m = "(" ++ show (getVal m) ++ " `modulo` " ++ show (getMod m) ++ ")"

instance KnownNat m => Bounded (Mod m) where
  minBound = Mod 0
  maxBound = let mx = Mod (getNatMod mx - 1) in mx

instance KnownNat m => Num (Mod m) where
  mx@(Mod x) + Mod y =
    Mod $ if xy >= m then xy - m else xy
    where
      xy = x + y
      m = getNatMod mx
  {-# INLINE (+) #-}
  mx@(Mod x) - Mod y =
    Mod $ if x >= y then x - y else m + x - y
    where
      m = getNatMod mx
  {-# INLINE (-) #-}
  negate mx@(Mod x) =
    Mod $ if x == 0 then 0 else getNatMod mx - x
  {-# INLINE negate #-}

  mx@(Mod !x) * (Mod !y) =
    Mod $ x * y `rem` getNatMod mx
    -- `rem` is slightly faster than `mod`
  {-# INLINE (*) #-}

  abs = id
  {-# INLINE abs #-}
  signum = const $ Mod 1
  {-# INLINE signum #-}
  fromInteger x = mx
    where
      mx = Mod $ fromInteger $ x `mod` getMod mx
  {-# INLINE fromInteger #-}

-- | Beware that division by residue, which is not coprime with the modulo,
-- will result in runtime error. Consider using 'invertMod' instead.
instance KnownNat m => Fractional (Mod m) where
  fromRational r = case denominator r of
    1   -> num
    den -> num / fromInteger den
    where
      num = fromInteger (numerator r)
  {-# INLINE fromRational #-}
  recip mx = case invertMod mx of
    Nothing -> error $ "recip{Mod}: residue is not coprime with modulo"
    Just y  -> y
  {-# INLINE recip #-}

-- | Linking type and value levels: extract modulo @m@ as a value.
getMod :: KnownNat m => Mod m -> Integer
getMod = toInteger . natVal
{-# INLINE getMod #-}

-- | Linking type and value levels: extract modulo @m@ as a value.
getNatMod :: KnownNat m => Mod m -> Natural
getNatMod = natVal
{-# INLINE getNatMod #-}

-- | The canonical representative of the residue class, always between 0 and @m-1@ inclusively.
getVal :: Mod m -> Integer
getVal (Mod x) = toInteger x
{-# INLINE getVal #-}

-- | The canonical representative of the residue class, always between 0 and @m-1@ inclusively.
getNatVal :: Mod m -> Natural
getNatVal (Mod x) = x
{-# INLINE getNatVal #-}

-- | Computes the modular inverse, if the residue is coprime with the modulo.
--
-- >>> :set -XDataKinds
-- >>> invertMod (3 :: Mod 10)
-- Just (7 `modulo` 10) -- because 3 * 7 = 1 :: Mod 10
-- >>> invertMod (4 :: Mod 10)
-- Nothing
invertMod :: KnownNat m => Mod m -> Maybe (Mod m)
invertMod mx
  = if y <= 0
    then Nothing
    else Just $ Mod $ fromInteger y
  where
    -- first argument of recipModInteger is guaranteed to be positive
    y = recipModInteger (getVal mx) (getMod mx)
{-# INLINABLE invertMod #-}

-- | Drop-in replacement for 'Prelude.^', with much better performance.
--
-- >>> :set -XDataKinds
-- >>> powMod (3 :: Mod 10) 4
-- (1 `modulo` 10)
powMod :: (KnownNat m, Integral a) => Mod m -> a -> Mod m
powMod mx a
  | a < 0     = error $ "^{Mod}: negative exponent"
  | otherwise = Mod $ powModNatural (getNatVal mx) (fromIntegral a) (getNatMod mx)
{-# INLINABLE [1] powMod #-}

{-# SPECIALISE [1] powMod ::
  KnownNat m => Mod m -> Integer -> Mod m,
  KnownNat m => Mod m -> Natural -> Mod m,
  KnownNat m => Mod m -> Int     -> Mod m,
  KnownNat m => Mod m -> Word    -> Mod m #-}

{-# RULES
"powMod/2/Integer"     forall x. powMod x (2 :: Integer) = let u = x in u*u
"powMod/3/Integer"     forall x. powMod x (3 :: Integer) = let u = x in u*u*u
"powMod/2/Int"         forall x. powMod x (2 :: Int)     = let u = x in u*u
"powMod/3/Int"         forall x. powMod x (3 :: Int)     = let u = x in u*u*u
"powMod/2/Word"        forall x. powMod x (2 :: Word)    = let u = x in u*u
"powMod/3/Word"        forall x. powMod x (3 :: Word)    = let u = x in u*u*u
#-}

-- | Infix synonym of 'powMod'.
(^%) :: (KnownNat m, Integral a) => Mod m -> a -> Mod m
(^%) = powMod
{-# INLINE (^%) #-}

infixr 8 ^%

-- Unfortunately, such rule never fires due to technical details
-- of type classes in Core.
-- {-# RULES "^%Mod" forall (x :: KnownNat m => Mod m) p. x ^ p = x ^% p #-}

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
  SomeMod mx == SomeMod my = getMod mx == getMod my && getVal mx == getVal my
  InfMod rx  == InfMod ry  = rx == ry
  _          == _          = False

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
liftBinOpMod f mx@(Mod x) my@(Mod y) = case someNatVal m of
  SomeNat (_ :: Proxy t) -> SomeMod (Mod (x `mod` m) `f` Mod (y `mod` m) :: Mod t)
  where
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
