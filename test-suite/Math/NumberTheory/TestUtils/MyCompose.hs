-- |
-- Module:      Math.NumberTheory.TestUtils.MyCompose
-- Copyright:   (c) 2016-2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Utils to test Math.NumberTheory
--

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Math.NumberTheory.TestUtils.MyCompose
  ( MyCompose(..)
  ) where

import GHC.Generics (Generic)

import Test.QuickCheck (Arbitrary)
import Test.SmallCheck.Series (Serial)

newtype MyCompose f g a = MyCompose { getMyCompose :: f (g a) }
  deriving (Eq, Ord, Show, Functor, Num, Enum, Bounded, Real, Integral, Arbitrary, Generic)

instance (Monad m, Serial m (f (g a))) => Serial m (MyCompose f g a)
