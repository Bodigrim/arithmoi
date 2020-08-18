module Math.NumberTheory.Primes.QuadraticSieveTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Math.NumberTheory.TestUtils ()
import Math.NumberTheory.Primes
import Math.NumberTheory.Primes.Factorisation.QuadraticSieve
import qualified Debug.Trace

trace :: String -> a -> a
trace = if debug then Debug.Trace.trace else const id

debug :: Bool
debug = True

checkQuadratic :: Large Int -> Large Int -> Bool
checkQuadratic (Large i) (Large j)
  | p == 2 || q == 2 || p == q = True
  | otherwise                  = n `mod` factor == 0
  where
    factor = trace ("Number: " ++ show n) $ quadraticSieve n
    n = p * q
    p = toInteger . unPrime . nextPrime $ i --`mod` 100000000
    q = toInteger . unPrime . nextPrime $ j --`mod` 100000000

testSuite :: TestTree
testSuite = testGroup "QuadraticSieve"
  [ QC.testProperty "Successful Factorisations" checkQuadratic
  ]
