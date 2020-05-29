module Math.NumberTheory.Primes.Factorisation.QuadraticSieve
  ( quadraticSieve
  )

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Math.NumberTheory.Primes.Sieve.SmoothSieve

quadraticSieve :: Int -> (Int, Int)
-- Input assumed odd composite positive integer
quadraticSieve n =
    let b = 
