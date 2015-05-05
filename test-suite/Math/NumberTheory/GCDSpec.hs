{-# LANGUAGE ScopedTypeVariables #-}
module Math.NumberTheory.GCDSpec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Math.NumberTheory.GCD
import Data.Word (Word)

main :: IO ()
main = hspec spec

-- | Check whether (d, u, v) tuple is the extended GCD of (x, y) pair.
-- That is check for:
--
-- gcd x y == d
-- u * x + v * y == d
isExtendedGcdOf :: (Integral a, Integral c) => (a, c, c) -> (a, a) -> Bool
isExtendedGcdOf (d, u, v) (x, y) = (gcd x y == d) && (fromIntegral u * x + fromIntegral v * y == d)

spec :: Spec
spec = do
  describe "extendedGCD" $ do
    prop "is correct" $ do
      \x y -> extendedGCD x y `isExtendedGcdOf` (x, y :: Integer)

  describe "binaryExtendedGCD" $ do
    -- check general version
    prop "is correct for Integer" $ do
      \x y -> (binaryExtendedGCD x y :: (Integer, Integer, Integer)) `isExtendedGcdOf` (x, y)
    -- check specialised version
    prop "is correct for Word" $ do
      \x y -> (binaryExtendedGCD x y :: (Word, Int, Int)) `isExtendedGcdOf` (x, y)
    -- check specialised version
    prop "is correct for Int" $ do
      \x y -> (binaryExtendedGCD x y :: (Int, Int, Int)) `isExtendedGcdOf` (x, y)
