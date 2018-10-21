{-# LANGUAGE CPP         #-}

{-# OPTIONS_HADDOCK hide #-}
#if MIN_VERSION_base(4,10,0)
module GHC.TypeNats.Compat
  ( module GHC.TypeNats
  ) where

#if MIN_VERSION_base(4,11,0)
import GHC.TypeNats hiding (Mod)
#else
import GHC.TypeNats
#endif
#else

module GHC.TypeNats.Compat
  ( Nat
  , KnownNat
  , SomeNat(..)
  , natVal
  , someNatVal
  , sameNat
  ) where

import GHC.TypeLits (Nat, KnownNat, SomeNat(..), sameNat)
import qualified GHC.TypeLits as TL
import Numeric.Natural

natVal :: KnownNat n => proxy n -> Natural
natVal = fromInteger . TL.natVal

someNatVal :: Natural -> SomeNat
someNatVal n = case TL.someNatVal (toInteger n) of
  Nothing -> error "someNatVal: impossible negative argument"
  Just sn -> sn

#endif
