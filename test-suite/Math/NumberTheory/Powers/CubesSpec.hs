module Math.NumberTheory.Powers.CubesSpec (main, spec) where

import Test.Hspec
import Math.NumberTheory.Powers.Cubes

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "integerCubeRoot'" $ do
    it "finds the cube root of 27" $ do
      integerCubeRoot' (27 :: Integer) `shouldBe` 3
