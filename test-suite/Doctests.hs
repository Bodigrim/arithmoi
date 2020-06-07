module Main where

import Build_doctests
import Test.DocTest

main :: IO ()
main = doctest $ flags ++ pkgs ++ module_sources
