-- |
-- Module:      Math.NumberTheory.Primes.Testing.Certified
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Deterministic primality testing.
module Math.NumberTheory.Primes.Testing.Certified (isCertifiedPrime) where

import Math.NumberTheory.Primes.Testing.Probabilistic
import Math.NumberTheory.Primes.Testing.Certificates.Internal

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
