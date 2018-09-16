-- |
-- Module:      Math.NumberTheory.Moduli.Equations
-- Copyright:   (c) 2018 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Polynomial modular equations.
--

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Math.NumberTheory.Moduli.Equations
  ( solveLinear
  , solveQuadratic
  ) where

import GHC.Integer.GMP.Internals

import Math.NumberTheory.Moduli.Chinese
import Math.NumberTheory.Moduli.Class
import Math.NumberTheory.Moduli.Sqrt
import Math.NumberTheory.UniqueFactorisation
import Math.NumberTheory.Utils (recipMod)

-------------------------------------------------------------------------------
-- Linear equations

-- | Find all solutions of ax + b ≡ 0 (mod m).
--
-- >>> :set -XDataKinds
-- >>> solveLinear (6 :: Mod 10) 4 -- solving 6x + 4 ≡ 0 (mod 10)
-- [(1 `modulo` 10),(6 `modulo` 10)]
solveLinear
  :: KnownNat m
  => Mod m   -- ^ a
  -> Mod m   -- ^ b
  -> [Mod m] -- ^ list of x
solveLinear a b = map fromInteger $ solveLinear' (getMod a) (getVal a) (getVal b)

solveLinear' :: Integer -> Integer -> Integer -> [Integer]
solveLinear' m a b = case solveLinearCoprime m' (a `quot` d) (b `quot` d) of
  Nothing -> []
  Just x  -> map (\i -> x + m' * i) [0 .. d - 1]
  where
    d = m `gcd` a `gcd` b
    m' = m `quot` d

solveLinearCoprime :: Integer -> Integer -> Integer -> Maybe Integer
solveLinearCoprime 1 _ _ = Just 0
solveLinearCoprime m a b = (\a1 -> negate b * a1 `mod` m) <$> recipMod a m

-------------------------------------------------------------------------------
-- Quadratic equations

-- | Find all solutions of ax² + bx + c ≡ 0 (mod m).
--
-- >>> :set -XDataKinds
-- >>> solveQuadratic (1 :: Mod 32) 0 (-17) -- solving x² - 17 ≡ 0 (mod 32)
-- [(9 `modulo` 32),(25 `modulo` 32),(7 `modulo` 32),(23 `modulo` 32)]
solveQuadratic
  :: KnownNat m
  => Mod m   -- ^ a
  -> Mod m   -- ^ b
  -> Mod m   -- ^ c
  -> [Mod m] -- ^ list of c
solveQuadratic a b c
  = map fromInteger
  $ fst
  $ combine
  $ map (\(p, n) -> (solveQuadraticPrimePower a' b' c' p n, unPrime p ^ n))
  $ factorise
  $ getMod a
  where
    a' = getVal a
    b' = getVal b
    c' = getVal c

    combine :: [([Integer], Integer)] -> ([Integer], Integer)
    combine = foldl
      (\(xs, xm) (ys, ym) -> ([ chineseRemainder2 (x, xm) (y, ym) | x <- xs, y <- ys ], xm * ym))
      ([0], 1)

solveQuadraticPrimePower
  :: Integer
  -> Integer
  -> Integer
  -> Prime Integer
  -> Word
  -> [Integer]
solveQuadraticPrimePower a b c p = go
  where
    go :: Word -> [Integer]
    go 0 = [0]
    go 1 = solveQuadraticPrime a b c p
    go k = concatMap (liftRoot k) (go (k - 1))

    -- Hensel lifting
    -- https://en.wikipedia.org/wiki/Hensel%27s_lemma#Hensel_lifting
    liftRoot :: Word -> Integer -> [Integer]
    liftRoot k r = case recipMod (2 * a * r + b) pk of
      Nothing -> case fr of
        0 -> map (\i -> r + pk `quot` p' * i) [0 .. p' - 1]
        _ -> []
      Just invDeriv -> [(r - fr * invDeriv) `mod` pk]
      where
        pk = p' ^ k
        fr = (a * r * r + b * r + c) `mod` pk

    p' :: Integer
    p' = unPrime p

solveQuadraticPrime
  :: Integer
  -> Integer
  -> Integer
  -> Prime Integer
  -> [Integer]
solveQuadraticPrime a b c (unPrime -> 2 :: Integer)
  = case (even c, even (a + b)) of
    (True, True) -> [0, 1]
    (True, _)    -> [0]
    (_, False)   -> [1]
    _            -> []
solveQuadraticPrime a b c p
  | a `mod` p' == 0
  = solveLinear' p' b c
  | otherwise
  = map (\n -> n * recipModInteger (2 * a) p' `mod` p')
  $ case sqrtModMaybe (b * b - 4 * a * c) (FieldCharacteristic p 1) of
    Nothing -> []
    Just 0  -> [- b]
    Just rt -> [rt - b, - rt - b]
    where
      p' :: Integer
      p' = unPrime p

