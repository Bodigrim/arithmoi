module Main where

import Test.Tasty.Bench

import Math.NumberTheory.ArithmeticFunctionsBench as ArithmeticFunctions
import Math.NumberTheory.DiscreteLogarithmBench as DiscreteLogarithm
import Math.NumberTheory.EisensteinIntegersBench as Eisenstein
import Math.NumberTheory.GaussianIntegersBench as Gaussian
import Math.NumberTheory.InverseBench as Inverse
import Math.NumberTheory.JacobiBench as Jacobi
import Math.NumberTheory.MertensBench as Mertens
import Math.NumberTheory.PrimesBench as Primes
import Math.NumberTheory.PrimitiveRootsBench as PrimitiveRoots
import Math.NumberTheory.RecurrencesBench as Recurrences
import Math.NumberTheory.SequenceBench as Sequence
import Math.NumberTheory.SieveBlockBench as SieveBlock
import Math.NumberTheory.SmoothNumbersBench as SmoothNumbers
import Math.NumberTheory.ZetaBench as Zeta

main :: IO ()
main = defaultMain
  [ ArithmeticFunctions.benchSuite
  , DiscreteLogarithm.benchSuite
  , Eisenstein.benchSuite
  , Gaussian.benchSuite
  , Inverse.benchSuite
  , Jacobi.benchSuite
  , Mertens.benchSuite
  , Primes.benchSuite
  , PrimitiveRoots.benchSuite
  , Recurrences.benchSuite
  , Sequence.benchSuite
  , SieveBlock.benchSuite
  , SmoothNumbers.benchSuite
  , Zeta.benchSuite
  ]
