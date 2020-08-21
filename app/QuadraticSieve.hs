import Math.NumberTheory.Primes.Factorisation.QuadraticSieve

main :: IO ()
main = print $ quadraticSieveManual n qsc
  where
    qsc = QuadraticSieveConfig t m k h
    h = 18
    k = 2
    m = 3 * t `div` 2 + 900
    t = floor ((*12) . sqrt . exp . sqrt $ log (fromInteger n) * log (log (fromInteger n)) :: Double)
    n = 313171213163547737474104867123
