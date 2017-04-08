-- |
-- Module:      Math.NumberTheory.Moduli.Class
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Safe modular arithmetic with modulo on type level.
--

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Math.NumberTheory.Moduli.Class
  ( Mod(..)
  , getMod
  , invertMod

  , SomeMod(..)
  , modulo
  , getSomeMod
  , getSomeVal
  , invertSomeMod
  ) where

import Data.Proxy
import Data.Ratio
import Data.Type.Equality
import GHC.Integer.GMP.Internals
import GHC.TypeLits
import Numeric.Natural

-- | Wrapper for residues modulo @m@.
newtype Mod (m :: Nat) = Mod
  { getVal :: Integer -- ^ Extract residue.
  } deriving (Eq, Ord)

instance KnownNat m => Show (Mod m) where
  show m = "(" ++ show (getVal m) ++ " mod " ++ show (natVal m) ++ ")"

instance KnownNat m => Num (Mod m) where
  mx@(Mod x) + Mod y =
    Mod $ if xy >= m then xy - m else xy
    where
      xy = x + y
      m = natVal mx
  mx@(Mod x) - Mod y =
    Mod $ if x >= y then x - y else m + x - y
    where
      m = natVal mx
  negate mx@(Mod x) =
    Mod $ if x == 0 then 0 else natVal mx - x
  mx@(Mod x) * Mod y =
    Mod $ x * y `mod` natVal mx
  abs = id
  signum = const $ Mod 1
  fromInteger x = mx
    where
      mx = Mod $ fromInteger $ x `mod` natVal mx

instance KnownNat m => Fractional (Mod m) where
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
  recip mx = case invertMod mx of
    Nothing -> error $ "recip{Mod}: residue is not coprime with modulo"
    Just y  -> y

-- | Linking type and value level: extract modulo @m@ as a value.
getMod :: KnownNat m => Mod m -> Natural
getMod = fromInteger . natVal

invertMod :: KnownNat m => Mod m -> Maybe (Mod m)
invertMod mx@(Mod x) = case recipModInteger x (natVal mx) of
  0 -> Nothing
  y -> Just (Mod y)

powMod :: (KnownNat m, Integral a) => Mod m -> a -> Mod m
powMod mx@(Mod x) a
  | a < 0     = error $ "^{Mod}: negative exponent"
  | otherwise = Mod $ powModInteger x (toInteger a) (natVal mx)

#if __GLASGOW_HASKELL__ >= 800
{-# RULES
"^/Mod" forall (x :: KnownNat m => Mod m) p. x ^ p = powMod x p
#-}
#endif

data SomeMod where
  SomeMod :: KnownNat m => Mod m -> SomeMod

instance Eq SomeMod where
  SomeMod mx == SomeMod my = getMod mx == getMod my && getVal mx == getVal my

instance Show SomeMod where
  show (SomeMod m) = show m

modulo :: Integer -> Natural -> SomeMod
modulo n m = case someNatVal m' of
  Nothing                       -> error "modulo: negative modulo"
  Just (SomeNat (_ :: Proxy t)) -> SomeMod (Mod r :: Mod t)
  where
    m' = fromIntegral m
    r = fromInteger $ n `mod` m'

liftUnOp
  :: (forall k. KnownNat k => Mod k -> Mod k)
  -> SomeMod
  -> SomeMod
liftUnOp f (SomeMod mx) = SomeMod (f mx)

liftBinOpMod
  :: (KnownNat m, KnownNat n)
  => (forall k. KnownNat k => Mod k -> Mod k -> Mod k)
  -> Mod m
  -> Mod n
  -> SomeMod
liftBinOpMod f mx@(Mod x) my@(Mod y) = case someNatVal m of
  Nothing                       -> error "modulo: negative modulo"
  Just (SomeNat (_ :: Proxy t)) -> SomeMod (Mod (x `mod` m) `f` Mod (y `mod` m) :: Mod t)
  where
    m = natVal mx `gcd` natVal my

liftBinOp
  :: (forall k. KnownNat k => Mod k -> Mod k -> Mod k)
  -> SomeMod
  -> SomeMod
  -> SomeMod
liftBinOp f (SomeMod (mx :: Mod m)) (SomeMod (my :: Mod n))
    = case (Proxy :: Proxy m) `sameNat` (Proxy :: Proxy n) of
      Nothing   -> liftBinOpMod f mx my
      Just Refl -> SomeMod (mx `f` my)

-- | 'fromInteger' implementation does not make much sense,
-- it is present for the sake of completeness.
instance Num SomeMod where
  (+)    = liftBinOp (+)
  (-)    = liftBinOp (-)
  negate = liftUnOp negate
  (*)    = liftBinOp (*)
  abs    = id
  signum = liftUnOp signum
  fromInteger x = modulo 1 (fromInteger x)

-- | 'fromRational' implementation does not make much sense,
-- it is present for the sake of completeness.
instance Fractional SomeMod where
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
  recip = liftUnOp recip

-- | Linking type and value level: extract modulo as a value.
getSomeMod :: SomeMod -> Natural
getSomeMod (SomeMod mx) = getMod mx

-- | Extract residue.
getSomeVal :: SomeMod -> Integer
getSomeVal (SomeMod mx) = getVal mx

invertSomeMod :: SomeMod -> Maybe SomeMod
invertSomeMod (SomeMod mx) = SomeMod <$> invertMod mx

powSomeMod :: Integral a => SomeMod -> a -> SomeMod
powSomeMod (SomeMod mx) a = SomeMod (powMod mx a)

#if __GLASGOW_HASKELL__ >= 800
{-# RULES
"^/SomeMod" forall (x :: SomeMod) p. x ^ p = powSomeMod x p
#-}
#endif
