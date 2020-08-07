import Math.NumberTheory.Primes.Factorisation.QuadraticSieve

main :: IO ()
main = print $ quadraticSieve n t t 5
  where
    t = floor b
    b = (*50) . sqrt . exp . sqrt $ log (fromInteger n) * log (log (fromInteger n)) :: Double
    n = 820239374043355485537790987979115484333
      -- 313171213163547737474104867123
      -- 820239374043355485537790987979115484333
      -- 1000000000100000000000000000074000000005700000000000000000969
