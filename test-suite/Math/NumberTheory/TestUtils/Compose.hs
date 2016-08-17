-- |
-- Module:      Math.NumberTheory.TestUtils.Compose
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Utils to test Math.NumberTheory
--

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

{-# OPTIONS_GHC -fno-warn-orphans               #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module Math.NumberTheory.TestUtils.Compose where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif
import Control.Arrow
import Data.Functor.Classes
import Data.Functor.Compose

import Test.Tasty.QuickCheck (Arbitrary(..))
import Test.SmallCheck.Series (Serial(..))

instance Num (f (g a)) => Num (Compose f g a) where
  (Compose a) + (Compose b) = Compose (a + b)
  (Compose a) * (Compose b) = Compose (a * b)
  abs = Compose . abs . getCompose
  signum = Compose . signum . getCompose
  negate = Compose . negate . getCompose
  fromInteger = Compose . fromInteger

instance Enum (f (g a)) => Enum (Compose f g a) where
  toEnum = Compose . toEnum
  fromEnum = fromEnum . getCompose

instance Bounded (f (g a)) => Bounded (Compose f g a) where
  minBound = Compose minBound
  maxBound = Compose maxBound

instance (Functor f, Ord1 f, Ord1 g, Ord a, Real (f (g a))) => Real (Compose f g a) where
  toRational = toRational . getCompose

instance (Functor f, Ord1 f, Ord1 g, Ord a, Integral (f (g a))) => Integral (Compose f g a) where
  quotRem (Compose a) (Compose b) = (Compose *** Compose) (quotRem a b)
  toInteger = toInteger . getCompose

instance (Monad m, Serial m (f (g a))) => Serial m (Compose f g a) where
  series = Compose <$> series

instance Arbitrary (f (g a)) => Arbitrary (Compose f g a) where
  arbitrary = Compose <$> arbitrary
  shrink = fmap Compose . shrink . getCompose
