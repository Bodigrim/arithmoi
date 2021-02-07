-- |
-- Module:      Math.NumberTheory.Primes.Testing.Certified
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
--
-- Deterministic primality testing.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Math.NumberTheory.Primes.Testing.Certified
  ( isCertifiedPrime
  ) where

import Data.List (foldl')
import Data.Bits ((.&.))
import Data.Mod
import Data.Proxy
import GHC.Integer.GMP.Internals (powModInteger)
import GHC.TypeNats (SomeNat(..), someNatVal)

import Math.NumberTheory.Roots (integerSquareRoot)
import Math.NumberTheory.Primes (unPrime)
import Math.NumberTheory.Primes.Factorisation.TrialDivision (trialDivisionPrimeTo, trialDivisionTo, trialDivisionWith)
import Math.NumberTheory.Primes.Factorisation.Montgomery (montgomeryFactorisation, smallFactors, findParms)
import Math.NumberTheory.Primes.Testing.Probabilistic (bailliePSW, isPrime, isStrongFermatPP, lucasTest)
import Math.NumberTheory.Primes.Sieve.Atkin
import Math.NumberTheory.Utils (splitOff)

-- | @'isCertifiedPrime' n@ tests primality of @n@, first trial division
--   by small primes is performed, then a Baillie PSW test and finally a
--   prime certificate is constructed and verified, provided no step before
--   found @n@ to be composite. Constructing prime certificates can take
--   a /very/ long time, so use this with care.
isCertifiedPrime :: Integer -> Bool
isCertifiedPrime n
    | n < 0     = isCertifiedPrime (-n)
    | otherwise = isPrime n && ((n < bpbd) || checkPrimalityProof (certifyBPSW n))
      where
        bpbd = 100000000000000000
-- Although it is known that there are no Baillie PSW pseudoprimes below 2^64,
-- use the verified bound 10^17, I don't know whether Gilchrist's result has been
-- verified yet.

-- | A proof of primality of a positive number. The type is
--   abstract to ensure the validity of proofs.
data PrimalityProof
    = Pocklington { cprime :: !Integer          -- ^ The number whose primality is proved.
                  , _factorisedPart, _cofactor :: !Integer
                  , _knownFactors :: ![(Integer, Word, Integer, PrimalityProof)]
                  }
    | TrialDivision { cprime :: !Integer        -- ^ The number whose primality is proved.
                    , tdLimit :: !Int }
    | Trivial { cprime :: !Integer              -- ^ The number whose primality is proved.
              }
      deriving Show

-- | Check the validity of a 'PrimalityProof'. Since it should be
--   impossible to create invalid proofs by the public interface, this
--   should never return 'False'.
checkPrimalityProof :: PrimalityProof -> Bool
checkPrimalityProof (Trivial n) =
  isTrivialPrime n
checkPrimalityProof (TrialDivision p b) =
  p <= toInteger b * toInteger b && trialDivisionPrimeTo b p
checkPrimalityProof (Pocklington p a b fcts) =
  b > 0 && a > b && a*b == pm1 && a == ppProd fcts && all verify fcts
  where
    pm1 = p-1
    ppProd pps = product [pf^e | (pf,e,_,_) <- pps]
    verify (pf,_,base,proof) = pf == cprime proof && crit pf base && checkPrimalityProof proof
    crit pf base = gcd p (x-1) == 1 && y == 1
      where
        x = powModInteger base (pm1 `quot` pf) p
        y = powModInteger x pf p

-- | @'isTrivialPrime'@ checks whether its argument is a trivially
--   known prime.
isTrivialPrime :: Integer -> Bool
isTrivialPrime n = n `elem` trivialPrimes

-- | List of trivially known primes.
trivialPrimes :: [Integer]
trivialPrimes = [2,3,5,7,11,13,17,19,23,29]

-- | Certify a small number. This is not exposed and should only
--   be used where correct. It is always checked after use, though,
--   so it shouldn't be able to lie.
smallCert :: Integer -> PrimalityProof
smallCert n
    | n < 30    = Trivial n
    | otherwise = TrialDivision n (fromInteger (integerSquareRoot n + 1))

-- | @'certify' n@ constructs, for @n > 1@, a proof of either
--   primality or compositeness of @n@. This may take a very long
--   time if the number has no small(ish) prime divisors
certify :: Integer -> Maybe PrimalityProof
certify n
    | n < 2     = error "Only numbers larger than 1 can be certified"
    | n < 31    = case trialDivisionWith trivialPrimes n of
                    ((p,_):_) | p < n     -> Nothing
                              | otherwise -> Just (Trivial n)
                    _ -> error "Impossible"
    | n < billi = let r2 = fromInteger (integerSquareRoot n + 2) in
                  case trialDivisionTo r2 n of
                    ((p,_):_) | p < n       -> Nothing
                              | otherwise   -> Just (TrialDivision n r2)
                    _ -> error "Impossible"
    | otherwise = case smallFactors (fromInteger (abs n)) of
                    ([], Just _) | not (isStrongFermatPP n 2) -> Nothing
                                 | not (lucasTest n) -> Nothing
                                 | otherwise -> Just (certifyBPSW n)       -- if it isn't we error and ask for a report.
                    ((toInteger -> p,_):_, _)
                      | p == n -> Just (TrialDivision n (fromInteger (min 100000 n)))
                      | otherwise -> Nothing
                    _ -> error ("***Error factorising " ++ show n ++ "! Please report this to maintainer of arithmoi.")
      where
        billi = 1000000000000

-- | Certify a number known to be not too small, having no small prime divisors and having
--   passed the Baillie PSW test. So we assume it's prime, erroring if not.
--   Since it's presumably a large number, we don't bother with trial division and
--   construct a Pocklington certificate.
certifyBPSW :: Integer -> PrimalityProof
certifyBPSW n = Pocklington n a b kfcts
  where
    nm1 = n-1
    h = nm1 `quot` 2
    m3 = fromInteger n .&. (3 :: Int) == 3
    (a,pp,b) = findDecomposition nm1
    kfcts0 = map check pp
    kfcts = foldl' force [] kfcts0
    force xs t@(_,_,_,prf) = prf `seq` (t:xs)
    check (p,e,byTD) = go 2
      where
        go bs
            | bs > h    = error (bpswMessage n)
            | x == 1    = if m3 && (p == 2) then (p,e,n-bs,Trivial 2) else go (bs+1)
            | g /= 1    = error (bpswMessage n ++ found g)
            | y /= 1    = error (bpswMessage n ++ fermat bs)
            | byTD      = (p,e,bs, smallCert p)
            | otherwise = case certify p of
                            Nothing -> error ("***Error in factorisation code: " ++ show p
                                                        ++ " was supposed to be prime but isn't.\n"
                                                        ++ "Please report this to the maintainer.\n\n")
                            Just ppr ->(p,e,bs,ppr)
              where
                q = nm1 `quot` p
                x = powModInteger bs q n
                y = powModInteger x p n
                g = gcd n (x-1)

-- | Find a decomposition of p-1 for the pocklington certificate.
--   Usually bloody slow if p-1 has two (or more) /large/ prime divisors.
findDecomposition :: Integer -> (Integer, [(Integer, Word, Bool)], Integer)
findDecomposition n = go 1 n [] prms
  where
    sr = integerSquareRoot n
    pbd = fromInteger (min 1000000 (sr+20))
    prms = map (toInteger . unPrime) (atkinFromTo 0 pbd)
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

-- | Find a factor of a known composite with approximately digits digits,
--   starting with curve s. Actually, this may loop infinitely, but the
--   loop should not be entered before the heat death of the universe.
findFactor :: Integer -> Int -> Integer -> Integer
findFactor n digits s = case findLoop n lo loSieve hi count s of
                          Left t  -> findFactor n (digits+5) t
                          Right f -> f
  where
    (lo, loSieve, hi, count) = findParms digits

-- | Find a factor or say with which curve to continue.
findLoop :: Integer -> Word -> PrimeSieve -> Word -> Word -> Integer -> Either Integer Integer
findLoop _ _ _ _ 0 s = Left s
findLoop n lo loSieve hi ct s
    | n <= s+2  = Left 6
    | otherwise = case someNatVal (fromInteger n) of
                    SomeNat (_ :: Proxy t) -> case montgomeryFactorisation lo loSieve hi (fromInteger s :: Mod t) of
                      Nothing -> findLoop n lo loSieve hi (ct-1) (s+1)
                      Just fct
                        | bailliePSW fct -> Right fct
                        | otherwise -> Right (findFactor fct 8 (s+1))

-- | Message in the unlikely case a Baillie PSW pseudoprime is found.
bpswMessage :: Integer -> String
bpswMessage n = unlines
                    [ "\n***Congratulations! You found a Baillie PSW pseudoprime!"
                    , "Please report this finding to the maintainers:"
                    , "<daniel.is.fischer@googlemail.com>,"
                    , "<andrew.lelechenko@gmail.com>"
                    , "The number in question is:\n"
                    , show n
                    , "\nOther parties like wikipedia might also be interested."
                    , "\nSorry for aborting your programm, but this is a major discovery."
                    ]

-- | Found a factor
found :: Integer -> String
found g = "\nA nontrivial divisor is:\n" ++ show g

-- | Fermat failure
fermat :: Integer -> String
fermat b = "\nThe Fermat test fails for base\n" ++ show b
