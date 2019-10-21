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
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UnboxedTuples              #-}

module Data.Mod
  ( -- * Known modulo
    Mod
  , getVal
  , getNatVal
  , getMod
  , getNatMod
  , invertMod
  , powMod
  , (^%)
  ) where

import Data.Ratio
import GHC.Exts
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

  -- If modulo is small and fits into one machine word,
  -- there is no need to use long arithmetic at all
  -- and we can save some allocations.
  mx@(Mod (NatS# x#)) * (Mod (NatS# y#)) = case getNatMod mx of
    NatS# m# -> let !(# z1#, z2# #) = timesWord2# x# y# in
                let !(# _, r# #) = quotRemWord2# z1# z2# m# in
                Mod (NatS# r#)
    NatJ# b# -> let !(# z1#, z2# #) = timesWord2# x# y# in
                let r# = wordToBigNat2 z1# z2# `remBigNat` b# in
                Mod $ if isTrue# (sizeofBigNat# r# ==# 1#)
                  then NatS# (bigNatToWord r#)
                  else NatJ# r#

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
