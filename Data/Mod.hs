-- |
-- Module:      Data.Mod
-- Copyright:   (c) 2017-2019 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Safe modular arithmetic with modulo on type level.
--

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnboxedTuples              #-}

module Data.Mod
  ( Mod
  , unMod
  , invertMod
  , (^%)
  ) where

import Control.Exception
import Control.DeepSeq
import Data.Euclidean (GcdDomain(..), Euclidean(..), Field)
import Data.Ratio
import Data.Semiring (Semiring(..), Ring(..))
import GHC.Exts
import GHC.Generics
import GHC.Integer.GMP.Internals
import GHC.Natural (Natural(..), powModNatural)

#if MIN_VERSION_base(4,11,0)
import GHC.TypeNats hiding (Mod)
#elif MIN_VERSION_base(4,10,0)
import GHC.TypeNats
#else

import GHC.TypeLits hiding (natVal, someNatVal)
import qualified GHC.TypeLits as TL

natVal :: KnownNat n => proxy n -> Natural
natVal = fromInteger . TL.natVal

someNatVal :: Natural -> SomeNat
someNatVal n = case TL.someNatVal (toInteger n) of
  Nothing -> error "someNatVal: impossible negative argument"
  Just sn -> sn

#endif

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
newtype Mod (m :: Nat) = Mod
  { unMod :: Natural
  -- ^ The canonical representative of the residue class,
  -- always between 0 and m-1 inclusively.
  }
  deriving (Eq, Ord, Generic)

instance NFData (Mod m)

instance KnownNat m => Show (Mod m) where
  show m = "(" ++ show (unMod m) ++ " `modulo` " ++ show (natVal m) ++ ")"

instance KnownNat m => Enum (Mod m) where
  succ x = if x == maxBound then throw Overflow  else coerce (succ @Natural) x
  pred x = if x == minBound then throw Underflow else coerce (pred @Natural) x

  toEnum   = fromIntegral
  fromEnum = fromIntegral . unMod

  enumFrom x       = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)

  enumFromTo     = coerce (enumFromTo     @Natural)
  enumFromThenTo = coerce (enumFromThenTo @Natural)

instance KnownNat m => Bounded (Mod m) where
  minBound = Mod 0
  maxBound = let mx = Mod (natVal mx - 1) in mx

instance KnownNat m => Num (Mod m) where
  mx@(Mod x) + Mod y =
    Mod $ if xy >= m then xy - m else xy
    where
      xy = x + y
      m = natVal mx
  {-# INLINE (+) #-}
  mx@(Mod x) - Mod y =
    Mod $ if x >= y then x - y else m + x - y
    where
      m = natVal mx
  {-# INLINE (-) #-}
  negate mx@(Mod x) =
    Mod $ if x == 0 then 0 else natVal mx - x
  {-# INLINE negate #-}

  -- If modulo is small and fits into one machine word,
  -- there is no need to use long arithmetic at all
  -- and we can save some allocations.
  mx@(Mod (NatS# x#)) * (Mod (NatS# y#)) = case natVal mx of
    NatS# m# -> let !(# z1#, z2# #) = timesWord2# x# y# in
                let !(# _, r# #) = quotRemWord2# z1# z2# m# in
                Mod (NatS# r#)
    NatJ# b# -> let !(# z1#, z2# #) = timesWord2# x# y# in
                let r# = wordToBigNat2 z1# z2# `remBigNat` b# in
                Mod $ if isTrue# (sizeofBigNat# r# ==# 1#)
                  then NatS# (bigNatToWord r#)
                  else NatJ# r#

  mx@(Mod !x) * (Mod !y) =
    Mod $ x * y `Prelude.rem` natVal mx
    -- `rem` is slightly faster than `mod`
  {-# INLINE (*) #-}

  abs = id
  {-# INLINE abs #-}
  signum = const $ Mod 1
  {-# INLINE signum #-}
  fromInteger x = mx
    where
      mx = Mod $ fromInteger $ x `mod` toInteger (natVal mx)
  {-# INLINE fromInteger #-}

instance KnownNat m => Semiring (Mod m) where
  plus  = (+)
  {-# INLINE plus #-}
  times = (*)
  {-# INLINE times #-}
  zero  = Mod 0
  {-# INLINE zero #-}
  one   = mx
    where
      mx = if natVal mx > 1 then Mod 1 else Mod 0
  {-# INLINE one #-}
  fromNatural x = mx
    where
      mx = Mod $ x `mod` natVal mx
  {-# INLINE fromNatural #-}

instance KnownNat m => Ring (Mod m) where
  negate = Prelude.negate
  {-# INLINE negate #-}

-- | Division by residue, which is not coprime with the modulo,
-- will throw 'DivideByZero'. Consider using 'invertMod' for non-prime moduli.
instance KnownNat m => Fractional (Mod m) where
  fromRational r = case denominator r of
    1   -> num
    den -> num / fromInteger den
    where
      num = fromInteger (numerator r)
  {-# INLINE fromRational #-}
  recip mx = case invertMod mx of
    Nothing -> throw DivideByZero
    Just y  -> y
  {-# INLINE recip #-}

-- | Division by residue, which is not coprime with the modulo,
-- will throw 'DivideByZero'. Consider using 'invertMod' for non-prime moduli.
instance KnownNat m => GcdDomain (Mod m) where
  divide x y = Just (x / y)
  gcd        = const $ const 1
  lcm        = const $ const 1
  coprime    = const $ const True

-- | Division by residue, which is not coprime with the modulo,
-- will throw 'DivideByZero'. Consider using 'invertMod' for non-prime moduli.
instance KnownNat m => Euclidean (Mod m) where
  degree      = const 0
  quotRem x y = (x / y, 0)
  quot        = (/)
  rem         = const $ const 0

-- | Division by residue, which is not coprime with the modulo,
-- will throw 'DivideByZero'. Consider using 'invertMod' for non-prime moduli.
instance KnownNat m => Field (Mod m)

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
    y = recipModInteger (toInteger (unMod mx)) (toInteger (natVal mx))
{-# INLINABLE invertMod #-}

-- | Drop-in replacement for 'Prelude.^', with much better performance.
--
-- >>> :set -XDataKinds
-- >>> (3 :: Mod 10) ^% 4
-- (1 `modulo` 10)
(^%) :: (KnownNat m, Integral a) => Mod m -> a -> Mod m
mx ^% a
  | a < 0     = case invertMod mx of
    Nothing ->  throw DivideByZero
    Just my ->  Mod $ powModNatural (unMod my) (fromIntegral (-a)) (natVal mx)
  | otherwise = Mod $ powModNatural (unMod mx) (fromIntegral a)    (natVal mx)
{-# INLINABLE [1] (^%) #-}

{-# SPECIALISE [1] (^%) ::
  KnownNat m => Mod m -> Integer -> Mod m,
  KnownNat m => Mod m -> Natural -> Mod m,
  KnownNat m => Mod m -> Int     -> Mod m,
  KnownNat m => Mod m -> Word    -> Mod m #-}

{-# RULES
"powMod"               forall (x :: KnownNat m => Mod m) p. x ^ p = x ^% p

"powMod/2/Integer"     forall x. x ^% (2 :: Integer) = let u = x in u*u
"powMod/3/Integer"     forall x. x ^% (3 :: Integer) = let u = x in u*u*u
"powMod/2/Int"         forall x. x ^% (2 :: Int)     = let u = x in u*u
"powMod/3/Int"         forall x. x ^% (3 :: Int)     = let u = x in u*u*u
"powMod/2/Word"        forall x. x ^% (2 :: Word)    = let u = x in u*u
"powMod/3/Word"        forall x. x ^% (3 :: Word)    = let u = x in u*u*u
#-}

infixr 8 ^%
