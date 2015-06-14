module Math.NumberTheory.Powers.SquaresSpec (main, spec) where

import Test.Hspec
import Math.NumberTheory.Powers.Squares

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "integerSquareRoot'" $ do
    it "finds the square root of 9" $ do
      integerSquareRoot' (9 :: Integer) `shouldBe` 3
