import Math.NumberTheory.Primes.Factorisation.QuadraticSieve

main :: IO ()
main = print $ quadraticSieve n b (2*b)
  where
    b = floor l
    l = (/10) . exp . sqrt $ log (fromInteger n) * log (log (fromInteger n)) :: Double
    n = 313171213163547737474104867123
