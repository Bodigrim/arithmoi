-- |
-- Module:      Math.NumberTheory.DirichletCharacters
-- Copyright:   (c) 2018 Bhavik Mehta
-- Licence:     MIT
-- Maintainer:  Bhavik Mehta <bhavikmehta8@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Implementation and enumeration of Dirichlet characters.
--

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Math.NumberTheory.DirichletCharacters where

#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup
#endif
import GHC.TypeNats.Compat                   (Nat)
import Numeric.Natural                       (Natural)
import Data.Ratio
import Data.Complex

import Math.NumberTheory.Moduli              (CyclicGroup(..), isPrimitiveRoot', chineseRemainder2, KnownNat, MultMod, discreteLogarithmPP, getVal, multElement)
import Math.NumberTheory.UniqueFactorisation (UniqueFactorisation, unPrime, Prime, factorise)
import Math.NumberTheory.Powers              (powMod)

-- data DirichletCharacter (n :: Nat) = Generated (Map Natural Natural)
--   deriving (Eq)
data DirichletCharacter (n :: Nat) = Generated [DirichletFactor]

newtype RootOfUnity = RootOfUnity { getFraction :: Rational }
  deriving (Eq, Show)
  -- RootOfUnity q represents e^(2pi i * q)
  -- I am happy with a custom Show instance if that's preferred

toRootOfUnity :: Rational -> RootOfUnity
toRootOfUnity q = RootOfUnity ((n `rem` d) % d)
  where n = numerator q
        d = denominator q
        -- effectively q `mod` 1

instance Semigroup RootOfUnity where
  (RootOfUnity q1) <> (RootOfUnity q2) = toRootOfUnity (q1 + q2)

instance Monoid RootOfUnity where
  mappend = (<>)
  mempty = RootOfUnity 0

fromRootOfUnity :: Floating a => RootOfUnity -> Complex a
fromRootOfUnity = cis . (2*pi*) . fromRational . getFraction

canonGenHelp :: (Integral a, UniqueFactorisation a) => (Prime a, Word) -> [a]
canonGenHelp (p, k)
  | p' == 2, k == 1 = []
  | p' == 2, k == 2 = [3]
  | p' == 2         = [5, p'^k - 1]
  | k == 1          = [modP]
  | otherwise       = [if powMod modP (p'-1) (p'*p') == 1 then modP + p' else modP]
  where p'   = unPrime p
        modP = head $ filter (isPrimitiveRoot' (CGOddPrimePower p 1)) [2..p' - 1]

generators :: Natural -> [Natural]
generators 1 = [1]
generators 2 = [1] -- special cases of trivial group
generators n = do
  (p,k) <- factorise n
  let factor = unPrime p ^ k
      rest = n `div` factor
  g <- canonGenHelp (p,k)
  return $ crt (g,factor) (1,rest)

crt :: (Natural, Natural) -> (Natural,Natural) -> Natural
crt (r1,md1) (r2,md2) = fromInteger $ chineseRemainder2 (toInteger r1,toInteger md1) (toInteger r2,toInteger md2)

-- TODO: improve using bitshifts
lambda :: Integer -> Word -> Integer
lambda x e = ((powMod x (2^(e-1)) (2^(2*e-1)) - 1) `div` (2^(e+1))) `mod` (2^(e-2))

data DirichletFactor = OddPrime { getPrime :: Prime Natural
                                , getPower :: Word
                                , getGenerator :: Natural
                                , getValue :: Natural
                                }
                      | Four { getValue :: Natural }
                      | TwoPower { getPower :: Word
                                 , getFirstValue :: Natural
                                 , getSecondValue :: Natural
                                 }

evaluate :: KnownNat n => DirichletCharacter n -> MultMod n -> RootOfUnity
evaluate (Generated ds) m = foldMap (evalFactor m') ds
  where m' = getVal $ multElement m

evalFactor :: Integer -> DirichletFactor -> RootOfUnity
evalFactor m =
  \case
    OddPrime (unPrime -> p) k a b -> toRootOfUnity (toInteger (b * discreteLogarithmPP p k (fromIntegral a) (m `rem` p^k)) % (p^(k-1)*(p-1)))
    Four b                        -> toRootOfUnity (((toInteger b) * (if (m `rem` 4) == 1 then 1 else 0)) % 2)
    TwoPower k s b                -> toRootOfUnity ((toInteger s) * (if (m `rem` 4) == 1 then 1 else 0) % 2) <> toRootOfUnity (toInteger b * lambda m'' k % (2^(k-2)))
                                       where m' = m `rem` (2^k)
                                             m'' = if m' `rem` 4 == 1
                                                      then m'
                                                      else 2^k - m'
