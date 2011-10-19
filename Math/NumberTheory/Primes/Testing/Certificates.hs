-- |
-- Module:      Math.NumberTheory.Primes.Testing.Certificates
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Certificates for primality or compositeness.
module Math.NumberTheory.Primes.Testing.Certificates
    ( -- * Certificates
      Certificate(..)
    , argueCertificate
    , CompositenessProof
    , composite
    , PrimalityProof
    , cprime
      -- * Arguments
    , CompositenessArgument(..)
    , PrimalityArgument(..)
      -- ** Weaken proofs to arguments
    , arguePrimality
    , argueCompositeness
      -- ** Prove valid arguments
    , verifyPrimalityArgument
    , verifyCompositenessArgument
      -- * Determine and prove whether a number is prime or composite
    , certify
      -- ** Checks for the paranoid
    , checkCertificate
    , checkCompositenessProof
    , checkPrimalityProof
    ) where

import Math.NumberTheory.Primes.Testing.Certificates.Internal

