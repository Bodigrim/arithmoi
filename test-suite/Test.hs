import Test.Tasty

import qualified Math.NumberTheory.Powers.CubesTests as Cubes
import qualified Math.NumberTheory.Powers.FourthTests as Fourth
import qualified Math.NumberTheory.Powers.GeneralTests as General
import qualified Math.NumberTheory.Powers.IntegerTests as Integer
import qualified Math.NumberTheory.Powers.SquaresTests as Squares

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Powers"
  [ Cubes.testSuite
  , Fourth.testSuite
  , General.testSuite
  , Integer.testSuite
  , Squares.testSuite
  ]
