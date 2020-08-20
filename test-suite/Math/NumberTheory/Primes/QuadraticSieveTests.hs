module Math.NumberTheory.Primes.QuadraticSieveTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Math.NumberTheory.TestUtils
import Math.NumberTheory.Primes
import Math.NumberTheory.Primes.Factorisation.QuadraticSieve
import qualified Debug.Trace

trace :: String -> a -> a
trace = if debug then Debug.Trace.trace else const id

debug :: Bool
debug = False

checkSquares :: Large Int -> Large Int -> Bool
checkSquares (Large i) (Large j)
  | p == 2 || q == 2 || p == q = True
  | otherwise                  = (x * x - y * y) `mod` n == 0
  where
    (x, y) = trace ("Number: " ++ show n) $ head $ findSquares n $ autoConfig n
    n = p * q
    p = toInteger . unPrime . nextPrime $ i
    q = toInteger . unPrime . nextPrime $ j

checkSmallFactor :: Int -> Int -> Bool
checkSmallFactor i j
  | p == 2 || q == 2 || p == q = True
  | otherwise                  = n `mod` factor == 0
  where
    factor = trace ("Number: " ++ show n) $ quadraticSieve n
    n = p * q
    p = toInteger . unPrime . nextPrime $ i
    q = toInteger . unPrime . nextPrime $ j

checkFactor :: Large Int -> Large Int -> Bool
checkFactor (Large i) (Large j)
  | p == 2 || q == 2 || p == q = True
  | otherwise                  = n `mod` factor == 0
  where
    factor = trace ("Number: " ++ show n) $ quadraticSieve n
    n = p * q
    p = toInteger . unPrime . nextPrime $ i
    q = toInteger . unPrime . nextPrime $ j

testSuite :: TestTree
testSuite = testGroup "QuadraticSieve"
  [ QC.testProperty "Squares Property" checkSquares
  , testSmallAndQuick "Small Factorisations" checkSmallFactor
  , QC.testProperty "Factorisations" checkFactor
  ]
