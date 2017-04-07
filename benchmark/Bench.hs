module Main where

import Criterion.Main

import Math.NumberTheory.ArithmeticFunctionsBench as ArithmeticFunctions
import Math.NumberTheory.PowersBench as Powers
import Math.NumberTheory.PrimesBench as Primes
import Math.NumberTheory.RecurrenciesBench as Recurrencies

main = defaultMain
  [ ArithmeticFunctions.benchSuite
  , Powers.benchSuite
  , Primes.benchSuite
  , Recurrencies.benchSuite
  ]
