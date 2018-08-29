module Main where

import Gauge.Main

import Math.NumberTheory.ArithmeticFunctionsBench as ArithmeticFunctions
import Math.NumberTheory.EisensteinIntegersBench as Eisenstein
import Math.NumberTheory.GaussianIntegersBench as Gaussian
import Math.NumberTheory.GCDBench as GCD
import Math.NumberTheory.JacobiBench as Jacobi
import Math.NumberTheory.MertensBench as Mertens
import Math.NumberTheory.PowersBench as Powers
import Math.NumberTheory.PrimesBench as Primes
import Math.NumberTheory.PrimitiveRootsBench as PrimitiveRoots
import Math.NumberTheory.RecurrenciesBench as Recurrencies
import Math.NumberTheory.SieveBlockBench as SieveBlock
import Math.NumberTheory.SmoothNumbersBench as SmoothNumbers

main :: IO ()
main = defaultMain
  [ ArithmeticFunctions.benchSuite
  , Eisenstein.benchSuite
  , Gaussian.benchSuite
  , GCD.benchSuite
  , Jacobi.benchSuite
  , Mertens.benchSuite
  , Powers.benchSuite
  , Primes.benchSuite
  , PrimitiveRoots.benchSuite
  , Recurrencies.benchSuite
  , SieveBlock.benchSuite
  , SmoothNumbers.benchSuite
  ]
