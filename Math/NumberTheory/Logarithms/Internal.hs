-- |
-- Module:      Math.NumberTheory.Logarithms.Internal
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Low level stuff for integer logarithms.
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_HADDOCK hide #-}
module Math.NumberTheory.Logarithms.Internal
    ( -- * Functions
      integerLog2#
    , wordLog2#
    ) where

-- Stuff is already there
import GHC.Integer.Logarithms
