-- |
-- Module:       Math.NumberTheory.Moduli.DiscreteLogarithm
-- Copyright:    (c) 2018 Nathan van Doorn
-- License:      MIT
-- Maintainer:   Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:    Provisional
-- Portability:  Non-portable
--

module Math.NumberTheory.Moduli.DiscreteLogarithm where

import Data.Maybe
import Math.NumberTheory.Moduli.Class

-- | Computes the discrete logarithm. @'discreteLogarithm' a b@ finds a value x,
-- such that @a '^%' x == b@, if such a value exists. This uses Pollard's rho
-- algorithm to compute the logarithm.
--
-- >>> discreteLogarithm (3 :: Mod 17) 14
-- Just 9
--
-- >>> discreteLogarithm (2 :: Mod 17) 14
-- Nothing
discreteLogarithm :: (KnownNat m, Integral b) => Mod m -> Mod m -> Maybe b
discreteLogarithm a0 b0 = case getNatMod a0 of
  0 -> Nothing
  1 -> Just 0
  m -> fromInteger <$> go m 0 0 0 0 1 1
  where
    f m x = case x `mod` 3 of
      0 -> x  * x `mod` m
      1 -> getNatVal a0 * x `mod` m
      2 -> getNatVal b0 * x `mod` m
      _ -> error "Impossible"

    g m x n = case x `mod` 3 of
      0 -> 2 * n `mod` (m - 1)
      1 -> n + 1 `mod` (m - 1)
      2 -> n
      _ -> error "Impossible"

    h m x n = case x `mod` 3 of
      0 -> 2 * n `mod` (m - 1)
      1 -> n
      2 -> n + 1 `mod` (m - 1)
      _ -> error "Impossible"

    go m a a2 b b2 x x2 =
      let
        x' = f m x
        a' = g m x a
        b' = h m x b
        x2' = f m (f m x2)
        a2' = g m (f m x2) (g m x2 a2)
        b2' = h m (f m x2) (h m x2 b2)
     in if x' == x2'
       then case toInteger b' `modulo` (m - 1) of
         SomeMod bm ->
           let b2m = fromIntegral b2'
               am  = fromIntegral a'
               a2m = fromIntegral a2'
           in if bm - b2m == 0
              then Nothing
              else case solveCongruence (bm - b2m) (a2m - am) of
                Nothing -> Nothing
                Just (SomeMod em) ->
                  let e = getVal em
                      n = getMod em
                  in listToMaybe [y | t <- [0..toInteger m`div`n], let y = e + t * n, a0 ^% y == b0]
                Just (InfMod {}) -> error "Impossible"
         InfMod {} -> error "Impossible"
       else go m a' a2' b' b2' x' x2'

    -- it would be nice if this were exported!
    solveCongruence :: KnownNat n => Mod n -> Mod n -> Maybe SomeMod
    solveCongruence km lm = case (gcd (getVal km) (getMod km), getVal lm) of
      (1, _) -> do
        k' <- invertMod km
        Just $ SomeMod (k' * lm)
      (d, l) | (l', 0) <- l `divMod` d ->
        let k' = getVal km `div` d
            m' = getNatMod km `div` fromInteger d
        in case k' `modulo` m' of
          SomeMod k'' ->
            let l'' = realToFrac l' `asTypeOf` k''
            in solveCongruence k'' l''
          InfMod {} -> error "Impossible"
      _ -> Nothing
