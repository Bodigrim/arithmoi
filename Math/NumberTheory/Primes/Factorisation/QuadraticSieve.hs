module Math.NumberTheory.Primes.Factorisation.QuadraticSieve
  ( quadraticSieve
  ) where

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Math.NumberTheory.Primes
import Math.NumberTheory.Primes.Sieve.SmoothSieve
import Math.NumberTheory.Primes.Sieve.Eratosthenes
import Math.NumberTheory.Moduli.JacobiSymbol

quadraticSieve :: Integer -> Int -> U.Vector Int
-- Input assumed odd composite positive integer
quadraticSieve n b = goodPrimes
    where
        -- Remove 2
        vectorOfPrimes = V.fromList (tail listOfPrimes)
        listOfPrimes = map (fromIntegral . unPrime) (primeList (primeSieve (toInteger b)))
        goodPrimes = U.filter (\x -> (jacobi n x == One)) vectorOfPrimes
