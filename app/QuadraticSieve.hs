import Math.NumberTheory.Primes.Factorisation.QuadraticSieve
import Math.NumberTheory.Logarithms
main :: IO ()
main = print $ quadraticSieveManual n qsc
  where
    qsc = QuadraticSieveConfig t m k h
    h = 19
    k = 2
    m = t + 900
    t = 12 * floor (exp (sqrt (le * log le) / 2) :: Double)
    l = integerLog10 n
    le = fromIntegral l * log 10
    n = 313171213163547737474104867123
