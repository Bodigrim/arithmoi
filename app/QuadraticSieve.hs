import Math.NumberTheory.Primes.Factorisation.QuadraticSieve

main :: IO ()
main = print $ findFactor 14148943687120141751230853 (quadraticSieve 14148943687120141751230853 1500000 3000000)
