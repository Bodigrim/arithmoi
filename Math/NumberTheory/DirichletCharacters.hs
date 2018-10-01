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
{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.DirichletCharacters where

#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup
#endif
import GHC.TypeNats.Compat
import Numeric.Natural                            (Natural)
import Data.Bits                                  (testBit, (.&.), bit)
import Data.Ratio                                 (Rational, (%), numerator, denominator)
import Data.Complex                               (Complex, cis)
import Data.Proxy                                 (Proxy(..))
import Data.List                                  (mapAccumL)

import Math.NumberTheory.ArithmeticFunctions      (totient)
import Math.NumberTheory.Moduli.Class             (KnownNat, MultMod, getVal, multElement, Mod, isMultElement)
import Math.NumberTheory.Moduli.DiscreteLogarithm (discreteLogarithmPP)
import Math.NumberTheory.UniqueFactorisation      (UniqueFactorisation, unPrime, Prime, factorise)
import Math.NumberTheory.Powers                   (powMod)
import Math.NumberTheory.Utils.FromIntegral       (wordToInt)

import Math.NumberTheory.Moduli.PrimitiveRoot

data DirichletCharacter (n :: Nat) = Generated [DirichletFactor]
  deriving (Eq, Show)

data DirichletFactor = OddPrime { getPrime :: Prime Natural
                                , getPower :: Word
                                , getGenerator :: Natural
                                , getValue :: Natural
                                }
                      | TwoPower { getPower :: Word
                                 , getFirstValue :: Natural
                                 , getSecondValue :: Natural
                                 }
                                 deriving (Eq, Show)

newtype RootOfUnity = RootOfUnity { getFraction :: Rational }
  deriving (Eq, Show)
  -- RootOfUnity q represents e^(2pi i * q)

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

generator :: (Integral a, UniqueFactorisation a) => Prime a -> Word -> a
generator p k
  | k == 1 = modP
  | otherwise = if powMod modP (p'-1) (p'*p') == 1 then modP + p' else modP
  where p' = unPrime p
        modP = head $ filter (isPrimitiveRoot' (CGOddPrimePower p 1)) [2..p'-1]

-- TODO: improve using bitshifts
lambda :: Integer -> Word -> Integer
lambda x e = ((powMod x (2^(e-1)) (2^(2*e-1)) - 1) `div` (2^(e+1))) `mod` (2^(e-2))

generalEval :: KnownNat n => DirichletCharacter n -> Mod n -> Maybe RootOfUnity
generalEval chi = fmap (evaluate chi) . isMultElement

evaluate :: KnownNat n => DirichletCharacter n -> MultMod n -> RootOfUnity
evaluate (Generated ds) m = foldMap (evalFactor m') ds
  where m' = getVal $ multElement m

evalFactor :: Integer -> DirichletFactor -> RootOfUnity
evalFactor m =
  \case
    OddPrime (toInteger . unPrime -> p) k (toInteger -> a) b ->
      toRootOfUnity (toInteger (b * discreteLogarithmPP p k a (m `rem` p^k)) % (p^(k-1)*(p-1)))
    TwoPower k s b -> toRootOfUnity (toInteger s * (if testBit m 1 then 1 else 0) % 2) <> toRootOfUnity (toInteger b * lambda m'' k % (2^(k-2)))
                                       where m' = m .&. kBits
                                             m'' = if testBit m 1
                                                      then bit (wordToInt k) - m'
                                                      else m'
                                             kBits = bit (wordToInt k) - 1

trivialChar :: KnownNat n => DirichletCharacter n
trivialChar = minBound

mulChars :: DirichletCharacter n -> DirichletCharacter n -> DirichletCharacter n
mulChars (Generated x) (Generated y) = Generated (zipWith combine x y)
  where combine :: DirichletFactor -> DirichletFactor -> DirichletFactor
        combine (OddPrime p k g n) (OddPrime _ _ _ m) = OddPrime p k g ((n + m) `mod` unPrime p ^ k)
        combine (TwoPower k a n) (TwoPower _ b m) = TwoPower k ((a + b) `mod` 2) ((n + m) `mod` 2^(k-2))
        combine _ _ = error "Malformed DirichletCharacter"

instance Semigroup (DirichletCharacter n) where
  (<>) = mulChars

instance KnownNat n => Monoid (DirichletCharacter n) where
  mempty = trivialChar

instance KnownNat n => Enum (DirichletCharacter n) where
  toEnum = fromIndex
  fromEnum = characterNumber
  -- TODO: write better succ and pred, by re-using the existing generators instead of recalculating them each time

instance KnownNat n => Bounded (DirichletCharacter n) where
  minBound = fromIndex (0 :: Int)
  maxBound = fromIndex (totient n - 1)
    where n = natVal (Proxy :: Proxy n)

characterNumber :: Integral a => DirichletCharacter n -> a
characterNumber (Generated y) = foldr go 0 y
  where go = \case
               OddPrime p k _ a -> \x -> x * (p'^(k-1)*(p'-1)) + (fromIntegral a)
                 where p' = fromIntegral (unPrime p)
               TwoPower k a b   -> \x -> (x * (2^(k-2)) + fromIntegral b) * 2 + (fromIntegral a)
               -- TODO: again use bitshifts to optimise

fromIndex :: forall a n. (KnownNat n, Integral a) => a -> DirichletCharacter n
fromIndex m
  | m < 0 = error "Enum DirichletCharacter: negative input"
  | m >= maxi = error "Enum DirichletCharacter: input too large"
  | otherwise = Generated (go (factorise n))
  where n = natVal (Proxy :: Proxy n)
        maxi = fromIntegral $ totient n
        m' = fromIntegral m
        go :: [(Prime Natural, Word)] -> [DirichletFactor]
        go [] = []
        go f@((p,k):xs) = case (unPrime p, k) of
                            (2,1) -> odds m' xs
                            (2,_) -> TwoPower k a2 b2: odds b1 xs
                                       where (a1,a2) = quotRem (fromIntegral m) 2
                                             (b1,b2) = quotRem a1 (2^(k-2))
                            _ -> odds m' f
        odds :: Natural -> [(Prime Natural, Word)] -> [DirichletFactor]
        odds t = snd . mapAccumL func t
          where func a (p,k) = (q, OddPrime p k (generator p k) r)
                  where (q,r) = quotRem a (p'^(k-1)*(p'-1))
                        p' = unPrime p
