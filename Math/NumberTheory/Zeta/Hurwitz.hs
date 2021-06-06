-- |
-- Module:      Math.NumberTheory.Zeta.Hurwitz
-- Copyright:   (c) 2018 Alexandre Rodrigues Baldé
-- Licence:     MIT
-- Maintainer:  Alexandre Rodrigues Baldé <alexandrer_b@outlook.com>
--
-- Hurwitz zeta function.

{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.Zeta.Hurwitz
  ( zetaHurwitz
  ) where

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
zetaHurwitz :: forall a . (Floating a, Ord a) => a -> a -> [a]
zetaHurwitz eps a = zipWith3 (\s i t -> s + i + t) ss is ts
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
    powsOfAPlusN :: [a]
    powsOfAPlusN = iterate (aPlusN *) 1

    -- [                   [      1      ] |                   ]
    -- | \sum_{k=0}^\(n-1) | ----------- | | s <- [0, 1, 2 ..] |
    -- [                   [ (a + k) ^ s ] |                   ]
    -- @S@ value in 4.8.5 formula.
    ss :: [a]
    ss = let numbers = map ((a +) . fromInteger) [0..digitsOfPrecision-1]
             denoms  = replicate (fromInteger digitsOfPrecision) 1 :
                       iterate (zipWith (*) numbers) numbers
         in map (sum . map recip) denoms

    -- [ (a + n) ^ (1 - s)            a + n         |                   ]
    -- | ----------------- = ---------------------- | s <- [0, 1, 2 ..] |
    -- [       s - 1          (a + n) ^ s * (s - 1) |                   ]
    -- @I@ value in 4.8.5 formula.
    is :: [a]
    is = let denoms = zipWith
                      (\powOfA int -> powOfA * fromInteger int)
                      powsOfAPlusN
                      [-1, 0..]
         in map (aPlusN /) denoms

    -- [      1      |             ]
    -- [ ----------- | s <- [0 ..] ]
    -- [ (a + n) ^ s |             ]
    constants2 :: [a]
    constants2 = map recip powsOfAPlusN

    -- [ [(s)_(2*k - 1) | k <- [1 ..]], s <- [0 ..]], i.e. odd indices of
    -- infinite rising factorial sequences, each sequence starting at a
    -- positive integer.
    pochhammers :: [[Integer]]
    pochhammers = let -- [ [(s)_k | k <- [1 ..]], s <- [1 ..]]
                      pochhs :: [[Integer]]
                      pochhs = iterate (\(x : xs) -> map (`div` x) xs) (tail factorial)
                  in -- When @s@ is @0@, the infinite sequence of rising
                     -- factorials starting at @s@ is @[0,0,0,0..]@.
                     repeat 0 : map skipOdds pochhs

    -- [            B_2k           |             ]
    -- | ------------------------- | k <- [1 ..] |
    -- [ (2k)! (a + n) ^ (2*k - 1) |             ]
    second :: [a]
    second =
        take (fromInteger digitsOfPrecision) $
        zipWith3
        (\bern evenFac denom -> fromRational bern / (denom * fromInteger evenFac))
        (tail $ skipOdds bernoulli)
        (tail $ skipOdds factorial)
        -- Recall that @powsOfAPlusN = [(a + n) ^ s | s <- [0 ..]]@, so this
        -- is @[(a + n) ^ (2 * s - 1) | s <- [1 ..]]@
        (skipEvens powsOfAPlusN)

    fracs :: [a]
    fracs = map
            (sum . zipWith (\s p -> s * fromInteger p) second)
            pochhammers

    -- Infinite list of @T@ values in 4.8.5 formula, for every @s@ in
    -- @[0, 1, 2 ..]@.
    ts :: [a]
    ts = zipWith
         (\constant2 frac -> constant2 * (0.5 + frac))
         constants2
         fracs
