import Math.NumberTheory.Primes.Factorisation.QuadraticSieve
-- import Math.NumberTheory.Primes

-- main :: IO ()
-- main = print $ factorise (111651716720340435022729265051422437839882656616232819593383 :: Integer)

main :: IO ()
main = print $ quadraticSieve n b (2*b)
  where
    b = floor l
    l = (*0.1) . exp . sqrt $ log (fromInteger n) * log (log (fromInteger n)) :: Double
    n = 313171213163547737474104867123
