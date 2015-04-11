-- |
-- Module:      Math.NumberTheory.Primes.Testing.Certificates.Internal
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Certificates for primality or compositeness.
{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
module Math.NumberTheory.Primes.Testing.Certificates.Internal
    ( Certificate(..)
    , CompositenessProof(..)
    , PrimalityProof(..)
    , CompositenessArgument(..)
    , PrimalityArgument(..)
    , checkCertificate
    , checkCompositenessProof
    , checkPrimalityProof
    , certify
    , trivial
    , smallCert
    , certifyBPSW
    , argueCertificate
    , arguePrimality
    , argueCompositeness
    , verifyPrimalityArgument
    , verifyCompositenessArgument
    ) where

import Data.List
#if __GLASGOW_HASKELL__ < 709
import Data.Word
#endif
import Data.Bits
import Data.Maybe

import Math.NumberTheory.Moduli
import Math.NumberTheory.Utils
import Math.NumberTheory.Primes.Factorisation.TrialDivision
import Math.NumberTheory.Primes.Factorisation.Montgomery
import Math.NumberTheory.Primes.Testing.Probabilistic
import Math.NumberTheory.Primes.Sieve.Eratosthenes
import Math.NumberTheory.Powers.Squares

-- | A certificate of either compositeness or primality of an
--   'Integer'. Only numbers @> 1@ can be certified, trying to
--   create a certificate for other numbers raises an error.
data Certificate
    = Composite !CompositenessProof
    | Prime !PrimalityProof
      deriving Show

-- | A proof of compositeness of a positive number. The type is
--   abstract to ensure the validity of proofs.
data CompositenessProof
    = Factors { composite :: !Integer           -- ^ The number whose compositeness is proved.
              , firstFactor
              , secondFactor :: !Integer }
    | StrongFermat { composite :: !Integer      -- ^ The number whose compositeness is proved.
                   , witness :: !Integer }
    | LucasSelfridge { composite :: !Integer    -- ^ The number whose compositeness is proved.
                     }
      deriving Show

-- | An argument for compositeness of a number (which must be @> 1@).
--   'CompositenessProof's translate directly to 'CompositenessArguments',
--   correct arguments can be transformed into proofs. This type allows the
--   manipulation of proofs while maintaining their correctness.
--   The only way to access components of a 'CompositenessProof' except
--   the composite is through this type.
data CompositenessArgument
    = Divisors { compo, firstDivisor, secondDivisor :: Integer }
                                                -- ^ @compo == firstDiv*secondDiv@, where all are @> 1@
    | Fermat { compo, fermatBase :: Integer }   -- ^ @compo@ fails the strong Fermat test for @fermatBase@
    | Lucas { compo :: Integer }                -- ^ @compo@ fails the Lucas-Selfridge test
    | Belief { compo :: Integer }               -- ^ No particular reason given
      deriving (Show, Read, Eq, Ord)

-- | A proof of primality of a positive number. The type is
--   abstract to ensure the validity of proofs.
data PrimalityProof
    = Pocklington { cprime :: !Integer          -- ^ The number whose primality is proved.
                  , factorisedPart, cofactor :: !Integer
                  , knownFactors :: ![(Integer,Int,Integer,PrimalityProof)]
                  }
    | TrialDivision { cprime :: !Integer        -- ^ The number whose primality is proved.
                    , tdLimit :: !Integer }
    | Trivial { cprime :: !Integer              -- ^ The number whose primality is proved.
              }
      deriving Show

-- | An argument for primality of a number (which must be @> 1@).
--   'PrimalityProof's translate directly to 'PrimalityArguments',
--   correct arguments can be transformed into proofs. This type allows the
--   manipulation of proofs while maintaining their correctness.
--   The only way to access components of a 'PrimalityProof' except
--   the prime is through this type.
data PrimalityArgument
    = Pock { aprime :: Integer
           , largeFactor, smallFactor :: Integer
           , factorList :: [(Integer,Int,Integer,PrimalityArgument)]
           }                                 -- ^ A suggested Pocklington certificate
    | Division { aprime, alimit :: Integer } -- ^ Primality should be provable by trial division to @alimit@
    | Obvious { aprime :: Integer }          -- ^ @aprime@ is said to be obviously prime, that holds for primes @< 30@
    | Assumption { aprime :: Integer }       -- ^ Primality assumed
      deriving (Show, Read, Eq, Ord)

argueCertificate :: Certificate -> Either CompositenessArgument PrimalityArgument
argueCertificate (Composite proof) = Left (argueCompositeness proof)
argueCertificate (Prime proof) = Right (arguePrimality proof)

-- | @'arguePrimality'@ transforms a proof of primality into an argument for primality.
arguePrimality :: PrimalityProof -> PrimalityArgument
arguePrimality (TrialDivision p l) = Division p l
arguePrimality (Trivial p) = Obvious p
arguePrimality (Pocklington p a b fcts) = Pock p a b (map argue fcts)
  where
    argue (x,y,z,prf) = (x,y,z,arguePrimality prf)

-- | @'verifyPrimalityArgument'@ checks the given argument and constructs a proof from
--   it, if it is valid. For the explicit arguments, this is simple and resonably fast,
--   for an 'Assumption', the verification uses 'certify' and hence may take a long time.
verifyPrimalityArgument :: PrimalityArgument -> Maybe PrimalityProof
verifyPrimalityArgument (Assumption p)
    = case certify p of
        Composite _ -> Nothing
        Prime proof -> Just proof
verifyPrimalityArgument arg
    | checkPrimalityProof prf   = Just prf
    | otherwise                 = Nothing
      where
        prf = primProof arg

-- | not exported, this is the one place where invalid proofs can be constructed
primProof :: PrimalityArgument -> PrimalityProof
primProof (Division p l) = TrialDivision p l
primProof (Obvious p) = Trivial p
primProof (Assumption p) = case certify p of
                             Composite _ -> Trivial p   -- we're faking to not raise an error
                             Prime proof -> proof
primProof (Pock p a b fcts) = Pocklington p a b (map prove fcts)
  where
    prove (x,y,z,arg) = (x,y,z,primProof arg)

-- | @'argueCompositeness'@ transforms a proof of compositeness into an argument
--   for compositeness.
argueCompositeness :: CompositenessProof -> CompositenessArgument
argueCompositeness (Factors c f s) = Divisors c f s
argueCompositeness (StrongFermat c b) = Fermat c b
argueCompositeness (LucasSelfridge c) = Lucas c

-- | @'verifyCompositenessArgument'@ checks the given argument and constructs a proof from
--   it, if it is valid. For the explicit arguments, this is simple and resonably fast,
--   for a 'Belief', the verification uses 'certify' and hence may take a long time.
verifyCompositenessArgument :: CompositenessArgument -> Maybe CompositenessProof
verifyCompositenessArgument (Belief c)
    = case certify c of
        Composite proof -> Just proof
        Prime _ -> Nothing
verifyCompositenessArgument arg
    | checkCompositenessProof prf = Just prf
    | otherwise = Nothing
      where
        prf = compProof arg

-- | not exported, here is where invalid proofs can be constructed,
--   they must not leak
compProof :: CompositenessArgument -> CompositenessProof
compProof (Divisors c f s) = Factors c f s
compProof (Fermat c b) = StrongFermat c b
compProof (Lucas c) = LucasSelfridge c
compProof (Belief _) = error "Trying to prove by belief"

-- | Check the validity of a 'Certificate'. Since it should be impossible
--   to construct invalid certificates by the public interface, this should
--   never return 'False'.
checkCertificate :: Certificate -> Bool
checkCertificate (Composite cp) = checkCompositenessProof cp
checkCertificate (Prime pp) = checkPrimalityProof pp

-- | Check the validity of a 'CompositenessProof'. Since it should be
--   impossible to create invalid proofs by the public interface, this
--   should never return 'False'.
checkCompositenessProof :: CompositenessProof -> Bool
checkCompositenessProof (Factors c a b) = a > 1 && b > 1 && a*b == c
checkCompositenessProof (StrongFermat c w) = w > 1 && c > w && not (isStrongFermatPP c w)
checkCompositenessProof (LucasSelfridge c) = c > 3 && fromIntegral c .&. (1 :: Int) == 1 && lucasTest c

-- | Check the validity of a 'PrimalityProof'. Since it should be
--   impossible to create invalid proofs by the public interface, this
--   should never return 'False'.
checkPrimalityProof :: PrimalityProof -> Bool
checkPrimalityProof (Trivial n) = isTrivialPrime n
checkPrimalityProof (TrialDivision p b) = p <= b*b && trialDivisionPrimeTo b p
checkPrimalityProof (Pocklington p a b fcts) = b > 0 && a > b && a*b == pm1 && a == ppProd fcts && all verify fcts
  where
    pm1 = p-1
    ppProd pps = product [pf^e | (pf,e,_,_) <- pps]
    verify (pf,_,base,proof) = pf == cprime proof && crit pf base && checkPrimalityProof proof
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

-- | Certify a small number. This is not exposed and should only
--   be used where correct. It is always checked after use, though,
--   so it shouldn't be able to lie.
smallCert :: Integer -> PrimalityProof
smallCert n
    | n < 30    = Trivial n
    | otherwise = TrialDivision n (integerSquareRoot' n + 1)

-- | @'certify' n@ constructs, for @n > 1@, a proof of either
--   primality or compositeness of @n@. This may take a very long
--   time if the number has no small(ish) prime divisors
certify :: Integer -> Certificate
certify n
    | n < 2     = error "Only numbers larger than 1 can be certified"
    | n < 31    = case trialDivisionWith trivialPrimes n of
                    ((p,_):_) | p < n     -> Composite (Factors n p (n `quot` p))
                              | otherwise -> Prime (Trivial n)
                    _ -> error "Impossible"
    | n < billi = let r2 = integerSquareRoot' n + 2 in
                  case trialDivisionTo r2 n of
                    ((p,_):_) | p < n       -> Composite (Factors n p (n `quot` p))
                              | otherwise   -> Prime (TrialDivision n r2)
                    _ -> error "Impossible"
    | otherwise = case smallFactors 100000 n of
                    ([], Just _) | not (isStrongFermatPP n 2) -> Composite (StrongFermat n 2)
                                 | not (lucasTest n) -> Composite (LucasSelfridge n)
                                 | otherwise -> Prime (certifyBPSW n)       -- if it isn't we error and ask for a report.
                    ((p,_):_, _) | p == n -> Prime (TrialDivision n (min 100000 n))
                                 | otherwise -> Composite (Factors n p (n `quot` p))
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
                            Composite cpr -> error ("***Error in factorisation code: " ++ show p
                                                        ++ " was supposed to be prime but isn't.\n"
                                                        ++ "Please report this to the maintainer.\n\n"
                                                        ++ show cpr)
                            Prime ppr ->(p,e,bs,ppr)
              where
                q = nm1 `quot` p
                x = powerModInteger' bs q n
                y = powerModInteger' x p n
                g = gcd n (x-1)

-- | Find a decomposition of p-1 for the pocklington certificate.
--   Usually bloody slow if p-1 has two (or more) /large/ prime divisors.
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

-- | Find a factor of a known composite with approximately digits digits,
--   starting with curve s. Actually, this may loop infinitely, but the
--   loop should not be entered before the heat death of the universe.
findFactor :: Integer -> Int -> Integer -> Integer
findFactor n digits s = case findLoop n lo hi count s of
                          Left t  -> findFactor n (digits+5) t
                          Right f -> f
  where
    (lo,hi,count) = findParms digits

-- | Find a factor or say with which curve to continue.
findLoop :: Integer -> Word -> Word -> Int -> Integer -> Either Integer Integer
findLoop _ _  _  0  s = Left s
findLoop n lo hi ct s
    | n <= s+2  = Left 6
    | otherwise = case montgomeryFactorisation n lo hi s of
                    Nothing -> findLoop n lo hi (ct-1) (s+1)
                    Just fct
                        | bailliePSW fct -> Right fct
                        | otherwise -> Right (findFactor fct 8 (s+1))

-- | Message in the unlikely case a Baillie PSW pseudoprime is found.
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

-- | Found a factor
found :: Integer -> String
found g = "\nA nontrivial divisor is:\n" ++ show g

-- | Fermat failure
fermat :: Integer -> String
fermat b = "\nThe Fermat test fails for base\n" ++ show b
