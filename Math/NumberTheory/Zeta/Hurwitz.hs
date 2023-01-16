-- |
-- Module:      Math.NumberTheory.Zeta.Hurwitz
-- Copyright:   (c) 2018 Alexandre Rodrigues Baldé
-- Licence:     MIT
-- Maintainer:  Alexandre Rodrigues Baldé <alexandrer_b@outlook.com>
--
-- Hurwitz zeta function.

{-# LANGUAGE PostfixOperators    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.Zeta.Hurwitz
  ( zetaHurwitz
  ) where

import Data.List.Infinite (Infinite(..), (....))
import qualified Data.List.Infinite as Inf

import Math.NumberTheory.Recurrences (bernoulli, factorial)
import Math.NumberTheory.Zeta.Utils  (skipEvens, skipOdds)

-- | Values of Hurwitz zeta function evaluated at @ζ(s, a)@ for @s ∈ [0, 1 ..]@.
--
-- The algorithm used was based on the Euler-Maclaurin formula and was derived
-- from <http://fredrikj.net/thesis/thesis.pdf Fast and Rigorous Computation of Special Functions to High Precision>
-- by F. Johansson, chapter 4.8, formula 4.8.5.
-- The error for each value in this recurrence is given in formula 4.8.9 as an
--  indefinite integral, and in formula 4.8.12 as a closed form formula.
--
-- It is the __user's responsibility__ to provide an appropriate precision for
-- the type chosen.
--
-- For instance, when using @Double@s, it does not make sense
-- to provide a number @ε < 1e-53@ as the desired precision. For @Float@s,
-- providing an @ε < 1e-24@ also does not make sense.
-- Example of how to call the function:
--
-- >>> zetaHurwitz 1e-15 0.25 !! 5
-- 1024.3489745265808
zetaHurwitz :: forall a . (Floating a, Ord a) => a -> a -> Infinite a
zetaHurwitz eps a = Inf.zipWith3 (\s i t -> s + i + t) ss is ts
  where
    -- When given @1e-14@ as the @eps@ argument, this'll be
    -- @div (33 * (length . takeWhile (>= 1) . iterate (/ 10) . recip) 1e-14) 10 == div (33 * 14) 10@
    -- @div (33 * 14) 10 == 46.
    -- meaning @N,M@ in formula 4.8.5 will be @46@.
    -- Multiplying by 33 and dividing by 10 is because asking for @14@ digits
    -- of decimal precision equals asking for @(log 10 / log 2) * 14 ~ 3.3 * 14 ~ 46@
    -- bits of precision.
    digitsOfPrecision :: Integer
    digitsOfPrecision =
       let magnitude = toInteger . length . takeWhile (>= 1) . iterate (/ 10) . recip $ eps
       in  div (magnitude * 33) 10

    -- @a + n@
    aPlusN :: a
    aPlusN = a + fromInteger digitsOfPrecision

    -- @[(a + n)^s | s <- [0, 1, 2 ..]]@
    powsOfAPlusN :: Infinite a
    powsOfAPlusN = Inf.iterate (aPlusN *) 1

    -- [                   [      1      ] |                   ]
    -- | \sum_{k=0}^\(n-1) | ----------- | | s <- [0, 1, 2 ..] |
    -- [                   [ (a + k) ^ s ] |                   ]
    -- @S@ value in 4.8.5 formula.
    ss :: Infinite a
    ss = let numbers = map ((a +) . fromInteger) [0..digitsOfPrecision-1]
             denoms  = replicate (fromInteger digitsOfPrecision) 1 :<
                       Inf.iterate (zipWith (*) numbers) numbers
         in Inf.map (sum . map recip) denoms

    -- [ (a + n) ^ (1 - s)            a + n         |                   ]
    -- | ----------------- = ---------------------- | s <- [0, 1, 2 ..] |
    -- [       s - 1          (a + n) ^ s * (s - 1) |                   ]
    -- @I@ value in 4.8.5 formula.
    is :: Infinite a
    is = let denoms = Inf.zipWith
                      (\powOfA int -> powOfA * fromInteger int)
                      powsOfAPlusN
                      ((-1, 0)....)
         in Inf.map (aPlusN /) denoms

    -- [      1      |             ]
    -- [ ----------- | s <- [0 ..] ]
    -- [ (a + n) ^ s |             ]
    constants2 :: Infinite a
    constants2 = Inf.map recip powsOfAPlusN

    -- [ [(s)_(2*k - 1) | k <- [1 ..]], s <- [0 ..]], i.e. odd indices of
    -- infinite rising factorial sequences, each sequence starting at a
    -- positive integer.
    pochhammers :: Infinite (Infinite Integer)
    pochhammers = let -- [ [(s)_k | k <- [1 ..]], s <- [1 ..]]
                      pochhs :: Infinite (Infinite Integer)
                      pochhs = Inf.iterate (\(x :< xs) -> Inf.map (`div` x) xs) (Inf.tail factorial)
                  in -- When @s@ is @0@, the infinite sequence of rising
                     -- factorials starting at @s@ is @[0,0,0,0..]@.
                     Inf.repeat 0 :< Inf.map skipOdds pochhs

    -- [            B_2k           |             ]
    -- | ------------------------- | k <- [1 ..] |
    -- [ (2k)! (a + n) ^ (2*k - 1) |             ]
    second :: [a]
    second =
        Inf.take (fromInteger digitsOfPrecision) $
        Inf.zipWith3
        (\bern evenFac denom -> fromRational bern / (denom * fromInteger evenFac))
        (Inf.tail $ skipOdds bernoulli)
        (Inf.tail $ skipOdds factorial)
        -- Recall that @powsOfAPlusN = [(a + n) ^ s | s <- [0 ..]]@, so this
        -- is @[(a + n) ^ (2 * s - 1) | s <- [1 ..]]@
        (skipEvens powsOfAPlusN)

    fracs :: Infinite a
    fracs = Inf.map
            (sum . zipWith (\s p -> s * fromInteger p) second . Inf.toList)
            pochhammers

    -- Infinite list of @T@ values in 4.8.5 formula, for every @s@ in
    -- @[0, 1, 2 ..]@.
    ts :: Infinite a
    ts = Inf.zipWith
         (\constant2 frac -> constant2 * (0.5 + frac))
         constants2
         fracs
