-- |
-- Module:      Math.NumberTheory.Primes.Testing.Certificates.Internal
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Certificates for primality or compositeness.
{-# LANGUAGE CPP, MagicHash, BangPatterns #-}
{-# OPTIONS_HADDOCK hide #-}
module Math.NumberTheory.Primes.Testing.Certificates.Internal
    ( Certificate(..)
    , CompositenessProof(..)
    , PrimalityProof(..)
    , checkCertificate
    , checkCompositenessProof
    , checkPrimalityProof
    , certify
    , trivial
    , smallCert
    , certifyBPSW
    ) where

import Data.List
import Data.Word
import Data.Bits
import Data.Maybe

import Math.NumberTheory.Moduli
import Math.NumberTheory.Utils
import Math.NumberTheory.Primes.Factorisation.TrialDivision
import Math.NumberTheory.Primes.Factorisation.Montgomery
import Math.NumberTheory.Primes.Testing.Probabilistic
import Math.NumberTheory.Primes.Sieve.Eratosthenes
import Math.NumberTheory.Powers.Squares

data Certificate
    = Composite !CompositenessProof
    | Prime !PrimalityProof
      deriving Show

data CompositenessProof
    = Factors { composite, firstFactor, secondFactor :: !Integer }
    | StrongFermat { composite, witness :: !Integer }
    | LucasSelfridge { composite :: !Integer }
      deriving Show

data PrimalityProof
    = Pocklington { cprime :: !Integer
                  , factorisedPart, cofactor :: !Integer
                  , knownFactors :: ![(Integer,Int,Integer,PrimalityProof)]
                  }
    | TrialDivision { cprime, tdLimit :: !Integer }
    | Trivial { cprime :: !Integer }
      deriving Show

checkCertificate :: Certificate -> Bool
checkCertificate (Composite cp) = checkCompositenessProof cp
checkCertificate (Prime pp) = checkPrimalityProof pp

checkCompositenessProof :: CompositenessProof -> Bool
checkCompositenessProof (Factors c a b) = a > 1 && b > 1 && a*b == c
checkCompositenessProof (StrongFermat c w) = w > 1 && c > w && not (isStrongFermatPP c w)
checkCompositenessProof (LucasSelfridge c) = c > 3 && fromIntegral c .&. (1 :: Int) == 1 && lucasTest c

checkPrimalityProof :: PrimalityProof -> Bool
checkPrimalityProof (Trivial n) = isTrivialPrime n
checkPrimalityProof (TrialDivision p b) = p <= b*b && trialDivisionPrimeTo b p
checkPrimalityProof (Pocklington p a b fcts) = b > 0 && a > b && a*b == pm1 && a == ppProd fcts && all verify fcts
  where
    pm1 = p-1
    ppProd pps = product [pf^e | (pf,e,_,_) <- pps]
    verify (pf,e,base,proof) = pf == cprime proof && crit pf base && checkPrimalityProof proof
    crit pf base = gcd p (x-1) == 1 && y == 1
      where
        x = powerModInteger' base (pm1 `quot` pf) p
        y = powerModInteger' x pf p

-- | @'trivial'@ records a trivially known prime.
--   If the argument is not one of them, an error is raised.
trivial :: Integer -> PrimalityProof
trivial n = fromMaybe oops $ maybeTrivial n
  where
    oops = error ("trivial: " ++ show n ++ " isn't a trivially known prime.")

-- | @'maybeTrivial'@ finds out if its argument is a trivially known
--   prime or not and returns the appropriate.
maybeTrivial :: Integer -> Maybe PrimalityProof
maybeTrivial n
    | isTrivialPrime n  = Just (Trivial n)
    | otherwise         = Nothing

-- | @'isTrivialPrime'@ checks whether its argument is a trivially
--   known prime.
isTrivialPrime :: Integer -> Bool
isTrivialPrime n = n `elem` trivialPrimes

-- | List of trivially known primes.
trivialPrimes :: [Integer]
trivialPrimes = [2,3,5,7,11,13,17,19,23,29]

smallCert :: Integer -> PrimalityProof
smallCert n
    | n < 30    = Trivial n
    | otherwise = TrialDivision n (integerSquareRoot' n + 1)

certify :: Integer -> Certificate
certify n
    | n < 2     = error "Only numbers larger than 1 can be certified"
    | n < 31    = case trialDivisionWith trivialPrimes n of
                    ((p,_):_) | p < n     -> Composite (Factors n p (n `quot` p))
                              | otherwise -> Prime (TrialDivision n 30)
    | n < 10^12 = let r2 = integerSquareRoot' n + 2 in
                  case trialDivisionTo r2 n of
                    ((p,_):_) | p < n       -> Composite (Factors n p (n `quot` p))
                              | otherwise   -> Prime (TrialDivision n r2)
    | otherwise = case smallFactors 100000 n of
                    ([], Just m) | not (isStrongFermatPP n 2) -> Composite (StrongFermat n 2)
                                 | not (lucasTest n) -> Composite (LucasSelfridge n)
                                 | otherwise -> Prime (certifyBPSW n)       -- if it isn't we error and ask for a report.
                    ((p,_):_, _) | p == n -> Prime (TrialDivision n (min 100000 n))
                                 | otherwise -> Composite (Factors n p (n `quot` p))
                    _ -> error ("***Error factorising " ++ show n ++ "! Please report this to maintainer of arithmoi.")

certifyBPSW :: Integer -> PrimalityProof
certifyBPSW n = Pocklington n a b kfcts
  where
    nm1 = n-1
    h = nm1 `quot` 2
    (a,pp,b) = findDecomposition nm1
    kfcts0 = map check pp
    kfcts = foldl' force [] kfcts0
    force xs t@(_,_,_,prf) = prf `seq` (t:xs)
    check (p,e,byTD) = go 2
      where
        go bs
            | bs > h    = error (bpswMessage n)
            | x == 1    = go (bs+1)
            | g /= 1    = error (bpswMessage n ++ found g)
            | y /= 1    = error (bpswMessage n ++ fermat bs)
            | byTD      = (p,e,bs, if p < 30 then Trivial p else TrialDivision p (integerSquareRoot' p + 2))
            | otherwise = case certify p of
                            Composite cpr -> error ("***Error in factorisation code: " ++ show p
                                                        ++ " was supposed to be prime but isn't.\n"
                                                        ++ "Please report this to the maintainer.")
                            Prime ppr ->(p,e,bs,ppr)
              where
                q = nm1 `quot` p
                x = powerModInteger' bs q n
                y = powerModInteger' x p n
                g = gcd n (x-1)

findDecomposition :: Integer -> (Integer, [(Integer,Int,Bool)], Integer)
findDecomposition n = go 1 n [] prms
  where
    sr = integerSquareRoot' n
    pbd = min 1000000 (sr+20)
    prms = primeList (primeSieve $ pbd)
    go a b afs (p:ps)
        | a > b     = (a,afs,b)
        | otherwise = case splitOff p b of
                        (0,_) -> go a b afs ps
                        (e,q) -> go (a*p^e) q ((p,e,True):afs) ps
    go a b afs []
        | a > b     = (a,afs,b)
        | bailliePSW b  = (b,[(b,1,False)],a)   -- Until a Baillie PSW pseudoprime is found, I'm going with this
        | e == 0    = error ("Error in factorisation, " ++ show p ++ " was found as a factor of " ++ show b ++ " but isn't.")
        | otherwise = go (a*p^e) q ((p,e,False):afs) []
          where
            p = findFactor b 8 6
            (e,q) = splitOff p b

findFactor :: Integer -> Int -> Integer -> Integer
findFactor n digits s = case findLoop n lo hi count s of
                          Left t  -> findFactor n (digits+5) t
                          Right f -> f
  where
    (lo,hi,count) = findParms digits

findLoop :: Integer -> Word -> Word -> Int -> Integer -> Either Integer Integer
findLoop _ _  _  0  s = Left s
findLoop n lo hi ct s
    | n <= s+2  = Left 6
    | otherwise = case montgomeryFactorisation n lo hi s of
                    Nothing -> findLoop n lo hi (ct-1) (s+1)
                    Just fct
                        | bailliePSW fct -> Right fct
                        | otherwise -> Right (findFactor fct 8 (s+1))

bpswMessage :: Integer -> String
bpswMessage n = unlines
                    [ "\n***Congratulations! You found a Baillie PSW pseudoprime!"
                    , "Please report this finding to the package maintainer,"
                    , "<daniel.is.fischer@googlemail.com>"
                    , "The number in question is:\n"
                    , show n
                    , "\nOther parties like wikipedia might also be interested."
                    , "\nSorry for aborting your programme, but this is a major discovery."
                    ]

found :: Integer -> String
found g = "\nA nontrivial divisor is:\n" ++ show g

fermat :: Integer -> String
fermat b = "\nThe Fermat test fails for base\n" ++ show b
