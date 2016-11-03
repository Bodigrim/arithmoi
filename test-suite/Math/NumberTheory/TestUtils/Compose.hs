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

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Math.NumberTheory.TestUtils.Compose where

import Data.Functor.Compose

import Test.Tasty.QuickCheck (Arbitrary)
import Test.SmallCheck.Series (Serial)

deriving instance Num (f (g a))     => Num (Compose f g a)
deriving instance Enum (f (g a))    => Enum (Compose f g a)
deriving instance Bounded (f (g a)) => Bounded (Compose f g a)

deriving instance (Ord (Compose f g a), Real (f (g a)))     => Real (Compose f g a)
deriving instance (Ord (Compose f g a), Integral (f (g a))) => Integral (Compose f g a)

deriving instance Arbitrary (f (g a)) => Arbitrary (Compose f g a)

instance (Monad m, Serial m (f (g a))) => Serial m (Compose f g a)
