module Math.NumberTheory.Powers.FourthSpec (main, spec) where

import Test.Hspec
import Math.NumberTheory.Powers.Fourth

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "integerFourthRoot'" $ do
    it "finds the cube root of 81" $ do
      integerFourthRoot' (81 :: Integer) `shouldBe` 3
