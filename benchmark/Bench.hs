module Main where

import Gauge.Main

import Math.NumberTheory.ArithmeticFunctionsBench as ArithmeticFunctions
import Math.NumberTheory.DiscreteLogarithmBench as DiscreteLogarithm
import Math.NumberTheory.EisensteinIntegersBench as Eisenstein
import Math.NumberTheory.EuclideanBench as Euclidean
import Math.NumberTheory.GaussianIntegersBench as Gaussian
import Math.NumberTheory.JacobiBench as Jacobi
import Math.NumberTheory.MertensBench as Mertens
import Math.NumberTheory.PowersBench as Powers
import Math.NumberTheory.PrimesBench as Primes
import Math.NumberTheory.PrimitiveRootsBench as PrimitiveRoots
import Math.NumberTheory.RecurrencesBench as Recurrences
import Math.NumberTheory.SieveBlockBench as SieveBlock
import Math.NumberTheory.SmoothNumbersBench as SmoothNumbers
import Math.NumberTheory.ZetaBench as Zeta

main :: IO ()
main = defaultMain
  [ ArithmeticFunctions.benchSuite
  , DiscreteLogarithm.benchSuite
  , Eisenstein.benchSuite
  , Euclidean.benchSuite
  , Gaussian.benchSuite
  , Jacobi.benchSuite
  , Mertens.benchSuite
  , Powers.benchSuite
  , Primes.benchSuite
  , PrimitiveRoots.benchSuite
  , Recurrences.benchSuite
  , SieveBlock.benchSuite
  , SmoothNumbers.benchSuite
  , Zeta.benchSuite
  ]
