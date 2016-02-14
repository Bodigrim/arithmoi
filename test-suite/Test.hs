import Test.Tasty

import qualified Math.NumberTheory.GCDTests as GCD
import qualified Math.NumberTheory.GCD.LowLevelTests as GCDLowLevel

import qualified Math.NumberTheory.LogarithmsTests as Logarithms

import qualified Math.NumberTheory.LucasTests as Lucas

import qualified Math.NumberTheory.MoebiusInversionTests as MoebiusInversion
import qualified Math.NumberTheory.MoebiusInversion.IntTests as MoebiusInversionInt

import qualified Math.NumberTheory.Powers.CubesTests as Cubes
import qualified Math.NumberTheory.Powers.FourthTests as Fourth
import qualified Math.NumberTheory.Powers.GeneralTests as General
import qualified Math.NumberTheory.Powers.IntegerTests as Integer
import qualified Math.NumberTheory.Powers.SquaresTests as Squares

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All"
  [ testGroup "Powers"
    [ Cubes.testSuite
    , Fourth.testSuite
    , General.testSuite
    , Integer.testSuite
    , Squares.testSuite
    ]
  , testGroup "GCD"
    [ GCD.testSuite
    , GCDLowLevel.testSuite
    ]
  , testGroup "Logarithms"
    [ Logarithms.testSuite
    ]
  , testGroup "Lucas"
    [ Lucas.testSuite
    ]
  , testGroup "MoebiusInversion"
    [ MoebiusInversion.testSuite
    , MoebiusInversionInt.testSuite
    ]
  ]
