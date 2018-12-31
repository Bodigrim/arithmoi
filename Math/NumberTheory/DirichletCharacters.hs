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
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Math.NumberTheory.DirichletCharacters
  (
  -- * Roots of unity
    RootOfUnity
  -- ** Conversions
  , toRootOfUnity
  , fromRootOfUnity
  , toComplex
  -- * Dirichlet characters
  , DirichletCharacter
  , evaluate
  , generalEval
  , toFunction
  , fromIndex
  , characterNumber
  -- ** Special Dirichlet characters
  , principalChar
  , isPrincipal
  , jacobiCharacter
  , induced
  -- ** Real Dirichlet characters
  , RealCharacter
  , isRealCharacter
  , getRealChar
  , toRealFunction
  ) where

import Data.Semigroup
import GHC.TypeNats.Compat
import Numeric.Natural                            (Natural)
import Data.Bits                                  (testBit, (.&.), bit)
import Data.Ratio                                 (Rational, (%), numerator, denominator)
import Data.Complex                               (Complex, cis)
import Data.Proxy                                 (Proxy(..))
import Data.List                                  (mapAccumL)

import Math.NumberTheory.ArithmeticFunctions      (totient)
import Math.NumberTheory.Moduli.Class             (KnownNat, MultMod, getVal, multElement, Mod, isMultElement)
import Math.NumberTheory.Moduli.Jacobi            (jacobi, JacobiSymbol(..))
import Math.NumberTheory.Moduli.DiscreteLogarithm (discreteLogarithmPP)
import Math.NumberTheory.UniqueFactorisation      (UniqueFactorisation, unPrime, Prime, factorise)
import Math.NumberTheory.Powers                   (powMod)
import Math.NumberTheory.Utils.FromIntegral       (wordToInt)

import Math.NumberTheory.Moduli.PrimitiveRoot

data DirichletCharacter (n :: Nat) = Generated [DirichletFactor]
  deriving (Show)

data DirichletFactor = OddPrime { _getPrime :: Prime Natural
                                , _getPower :: Word
                                , _getGenerator :: Natural
                                , _getValue :: Natural
                                }
                      | TwoPower { _getPower :: Word
                                 , _getFirstValue :: Natural
                                 , _getSecondValue :: Natural
                                 }
                                 deriving (Show)

instance Eq (DirichletCharacter n) where
  Generated a == Generated b = go a b
    where go [] [] = True
          go (OddPrime _ _ _ x : xs) (OddPrime _ _ _ y : ys) = x == y && go xs ys
          go (TwoPower _ x1 x2 : xs) (TwoPower _ y1 y2 : ys) = x1 == y1 && x2 == y2 && go xs ys
          go _ _ = False

-- | A representation of <https://en.wikipedia.org/wiki/Root_of_unity roots of unity>: complex
-- numbers \(z\) for which there is \(n\) such that \(z^n=1\).
newtype RootOfUnity =
  RootOfUnity { -- | Every root of unity can be expressed as \(e^{2 \pi i q}\) for some
                -- rational \(q\) satisfying \(0 \leq q < 1\), this function extracts it.
                fromRootOfUnity :: Rational }
  deriving (Eq)

instance Show RootOfUnity where
  show (RootOfUnity q)
    | n == 0    = "e^0"
    | d == 1    = "e^(πi)"
    | n == 1    = "e^(πi/" ++ show d ++ ")"
    | otherwise = "e^(" ++ show n ++ "πi/" ++ show d ++ ")"
    where n = numerator (2*q)
          d = denominator (2*q)

-- | Given a rational \(q\), produce the root of unity \(e^{2 \pi i q}\).
toRootOfUnity :: Rational -> RootOfUnity
toRootOfUnity q = RootOfUnity ((n `rem` d) % d)
  where n = numerator q
        d = denominator q
        -- effectively q `mod` 1
  -- This smart constructor ensures that the rational is always in the range 0 <= q < 1.

-- | This Semigroup is in fact a group, so @stimes@ can be called with a negative first argument.
instance Semigroup RootOfUnity where
  (RootOfUnity q1) <> (RootOfUnity q2) = toRootOfUnity (q1 + q2)
  stimes k (RootOfUnity q) = toRootOfUnity (q * fromIntegral k)

instance Monoid RootOfUnity where
  mappend = (<>)
  mempty = RootOfUnity 0

-- | Convert a root of unity into an inexact complex number. Due to floating point inaccuracies,
-- it is recommended to avoid use of this until the end of a calculation. Alternatively, with
-- the [cyclotomic](http://hackage.haskell.org/package/cyclotomic-0.5.1) package, one can use
-- @[polarRat](https://hackage.haskell.org/package/cyclotomic-0.5.1/docs/Data-Complex-Cyclotomic.html#v:polarRat)
-- 1 . @'fromRootOfUnity' to convert to a cyclotomic number.
toComplex :: Floating a => RootOfUnity -> Complex a
toComplex = cis . (2*pi*) . fromRational . fromRootOfUnity

-- | For primes, the canonical primitive root is the smallest such. For prime powers \(p^k\),
-- either the smallest primitive root \(g\) mod \(p\) works, or \(g+p\) works.
generator :: (Integral a, UniqueFactorisation a) => Prime a -> Word -> a
generator p k
  | k == 1 = modP
  | otherwise = if powMod modP (p'-1) (p'*p') == 1 then modP + p' else modP
  where p' = unPrime p
        modP = head $ filter (isPrimitiveRoot' (CGOddPrimePower p 1)) [2..p'-1]

-- | Implement the function \(\lambda\) from page 5 of
-- https://www2.eecs.berkeley.edu/Pubs/TechRpts/1984/CSD-84-186.pdf
lambda :: Integer -> Word -> Integer
lambda x e = ((powMod x (2^(e-1)) (2^(2*e-1)) - 1) `div` (2^(e+1))) `mod` (2^(e-2))

generalEval :: KnownNat n => DirichletCharacter n -> Mod n -> Maybe RootOfUnity
generalEval chi = fmap (evaluate chi) . isMultElement

toFunction :: (Integral a, RealFloat b, KnownNat n) => DirichletCharacter n -> a -> Complex b
toFunction chi = maybe 0 toComplex . generalEval chi . fromIntegral

evaluate :: DirichletCharacter n -> MultMod n -> RootOfUnity
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

-- | Give the principal character for this modulus: a principal character mod n is 1 for a coprime
-- to n, and 0 otherwise.
principalChar :: KnownNat n => DirichletCharacter n
principalChar = minBound

mulChars :: DirichletCharacter n -> DirichletCharacter n -> DirichletCharacter n
mulChars (Generated x) (Generated y) = Generated (zipWith combine x y)
  where combine :: DirichletFactor -> DirichletFactor -> DirichletFactor
        combine (OddPrime p k g n) (OddPrime _ _ _ m) = OddPrime p k g ((n + m) `mod` (p'^(k-1)*(p'-1)))
          where p' = unPrime p
        combine (TwoPower k a n) (TwoPower _ b m) = TwoPower k ((a + b) `mod` 2) ((n + m) `mod` 2^(k-2))
        combine _ _ = error "internal error: malformed DirichletCharacter"

instance Semigroup (DirichletCharacter n) where
  (<>) = mulChars

instance KnownNat n => Monoid (DirichletCharacter n) where
  mempty = principalChar

-- | We define `succ` and `pred` with more efficient implementations than
-- `toEnum . (+1) . fromEnum`.
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

-- | Test if a given Dirichlet character is prinicpal for its modulus: a principal character mod
-- \(n\) is 1 for \(a\) coprime to \(n\), and 0 otherwise.
isPrincipal :: KnownNat n => DirichletCharacter n -> Bool
isPrincipal chi = chi == principalChar

-- | Induce a Dirichlet character to a higher modulus. If \(d \mid n\), then \(a \bmod{n}\) can be
-- reduced to \(a \bmod{d}\). Thus, a multiplicative function on \(\mathbb{Z}/d\mathbb{Z}\)
-- induces a multiplicative function on \(\mathbb{Z}/n\mathbb{Z}\).
--
-- >>> :set -XTypeApplications
-- >>> chi = fromIndex 5 :: DirichletCharacter 45
-- >>> chi2 = induced @135 chi
-- >>> :t chi2
-- Maybe (DirichletCharacter 135)
--
induced :: forall n d. (KnownNat d, KnownNat n) => DirichletCharacter d -> Maybe (DirichletCharacter n)
induced (Generated start) = if n `rem` d == 0
                            then Just (Generated (combine n' start))
                            else Nothing
  where n = natVal (Proxy :: Proxy n)
        d = natVal (Proxy :: Proxy d)
        n' = factorise n
        combine :: [(Prime Natural, Word)] -> [DirichletFactor] -> [DirichletFactor]
        combine [] _ = []
        combine t [] = plain t
        combine ((p1,k1):xs) (y:ys)
          | unPrime p1 == 2, TwoPower k2 a b <- y = TwoPower k1 a (b*2^(k1-k2)): combine xs ys
          | OddPrime p2 1 _g a <- y, p1 == p2 = OddPrime p2 k1 (generator p2 k1) (a*unPrime p1^(k1-1)): combine xs ys
            -- generator p2 k1 will be g or g + p2, and we already know g is a primroot mod p
            -- so should be able to save work instead of running generator
          | OddPrime p2 k2 g a <- y, p1 == p2 = OddPrime p2 k1 g (a*unPrime p1^(k1-k2)): combine xs ys
          | unPrime p1 == 2, k1 >= 2 = TwoPower k1 0 0: combine xs (y:ys)
          | unPrime p1 == 2 = combine xs (y:ys)
          | otherwise = OddPrime p1 k1 (generator p1 k1) 0: combine xs (y:ys)
        plain :: [(Prime Natural, Word)] -> [DirichletFactor]
        plain [] = []
        plain f@((p,k):xs) = case (unPrime p, k) of
                               (2,1) -> map rest xs
                               (2,_) -> TwoPower k 0 0: map rest xs
                               _ -> map rest f
        rest :: (Prime Natural, Word) -> DirichletFactor
        rest (p,k) = OddPrime p k (generator p k) 0

-- | The <https://en.wikipedia.org/wiki/Jacobi_symbol Jacobi symbol> gives a real Dirichlet
-- character for odd moduli.
jacobiCharacter :: forall n. KnownNat n => Maybe (RealCharacter n)
jacobiCharacter = if odd n
                     then Just (RealChar (Generated (func <$> factorise n)))
                     else Nothing
  where n = natVal (Proxy :: Proxy n)
        func :: (Prime Natural, Word) -> DirichletFactor
        func (p,k) = OddPrime p k g val -- we know p is odd since n is odd and p | n
          where p' = unPrime p
                g = generator p k
                val = case k `stimes` jacobi g p' of
                        One -> 0
                        MinusOne -> p'^(k-1)*((p'-1) `div` 2) -- p is odd so this is fine
                        Zero -> error "internal error in jacobiCharacter: please report this as a bug"

-- | A Dirichlet character is real if it is real-valued.
newtype RealCharacter n = RealChar { -- | Extract the character itself from a `RealCharacter`.
                                     getRealChar :: DirichletCharacter n
                                   }

-- | Test if a given `DirichletCharacter` is real, and if so give a `RealCharacter`.
isRealCharacter :: DirichletCharacter n -> Maybe (RealCharacter n)
isRealCharacter t@(Generated xs) = if all real xs then Just (RealChar t) else Nothing
  where real :: DirichletFactor -> Bool
        real (OddPrime (unPrime -> p) k _ a) = a == 0 || a*2 == p^(k-1)*(p-1)
        real (TwoPower k _ b) = b == 0 || b == 2^(k-3)

-- TODO: it should be possible to calculate this without evaluate/generalEval
-- and thus avoid using discrete log calculations: consider the order of m
-- inside each of the factor groups?
-- | Evaluate a real Dirichlet character, which can only take values \(-1,0,1\).
toRealFunction :: KnownNat n => RealCharacter n -> Natural -> Int
toRealFunction (RealChar chi) m = case generalEval chi (fromIntegral m) of
                                    Nothing -> 0
                                    Just t | t == mempty -> 1
                                    Just t | t == RootOfUnity (1 % 2) -> -1
                                    _ -> error "internal error in toRealFunction: please report this as a bug"
