{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Main where

import Criterion.Main

import Math.NumberTheory.ArithmeticFunctionsBench as ArithmeticFunctions
import Math.NumberTheory.PowersBench as Powers

main = defaultMain
  [ ArithmeticFunctions.benchSuite
  , Powers.benchSuite
  ]
