-- |
-- Module:      Math.NumberTheory.Primes.Factorisation.Certified
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Factorisation proving the primality of the found factors.
--
-- For large numbers, this will be very slow in general.
-- Use only if you're paranoid or must be /really/ sure.
{-# LANGUAGE BangPatterns, CPP #-}
module Math.NumberTheory.Primes.Factorisation.Certified
  ( certifiedFactorisation
  , certificateFactorisation
  , provenFactorisation
  ) where

import System.Random
import Control.Monad.State.Strict
#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Data.Maybe
import Data.Bits

import Math.NumberTheory.Primes.Factorisation.Montgomery
import Math.NumberTheory.Primes.Testing.Certificates.Internal
import Math.NumberTheory.Primes.Testing.Probabilistic

-- | @'certifiedFactorisation' n@ produces the prime factorisation
--   of @n@, proving the primality of the factors, but doesn't report the proofs.
certifiedFactorisation :: Integer -> [(Integer,Int)]
certifiedFactorisation = map fst . certificateFactorisation

-- | @'certificateFactorisation' n@ produces a 'provenFactorisation'
--   with a default bound of @100000@.
certificateFactorisation :: Integer -> [((Integer,Int),PrimalityProof)]
certificateFactorisation n = provenFactorisation 100000 n

-- | @'provenFactorisation' bound n@ constructs a the prime factorisation of @n@
--   (which must be positive) together with proofs of primality of the factors,
--   using trial division up to @bound@ (which is arbitrarily replaced by @2000@
--   if the supplied value is smaller) and elliptic curve factorisation for the
--   remaining factors if necessary.
--
--   Construction of primality proofs can take a /very/ long time, so this
--   will usually be slow (but should be faster than using 'factorise' and
--   proving the primality of the factors from scratch).
provenFactorisation :: Integer -> Integer -> [((Integer,Int),PrimalityProof)]
provenFactorisation _ 1 = []
provenFactorisation bd n
    | n < 2     = error "provenFactorisation: argument not positive"
    | bd < 2000 = provenFactorisation 2000 n
    | otherwise = test $
      case smallFactors bd n of
        (sfs,mb) -> map (\t@(p,_) -> (t, smallCert p)) sfs
            ++ case mb of
                 Nothing -> []
                 Just k -> certiFactorisation (Just $ bd*(bd+2)) primeCheck (randomR . (,) 6)
                                                (mkStdGen $ fromIntegral n `xor` 0xdeadbeef) Nothing k

-- | verify that we indeed have a correct primality proof
test :: [((Integer,Int),PrimalityProof)] -> [((Integer,Int),PrimalityProof)]
test (t@((p,_),prf):more)
    | p == cprime prf && checkPrimalityProof prf    = t : test more
    | otherwise = error (invalid p prf)
test [] = []

-- | produce a proof of primality for primes
--   Only called for (not too small) numbers known to have no small prime factors,
--   so we can directly use BPSW without trial division.
primeCheck :: Integer -> Maybe PrimalityProof
primeCheck n
    | bailliePSW n  = case certifyBPSW n of
                        proof@Pocklington{} -> Just proof
                        _ -> Nothing
    | otherwise = Nothing

-- | produce a certified factorisation
--   Assumes all small prime factors have been stripped before.
--   Since it is not exported, that is known to hold.
--   This is a near duplicate of 'curveFactorisation', I should some time
--   clean this up.
certiFactorisation :: Maybe Integer                 -- ^ Lower bound for composite divisors
                   -> (Integer -> Maybe PrimalityProof)
                                                    -- ^ A primality test
                   -> (Integer -> g -> (Integer,g)) -- ^ A PRNG
                   -> g                             -- ^ Initial PRNG state
                   -> Maybe Int                     -- ^ Estimated number of digits of the smallest prime factor
                   -> Integer                       -- ^ The number to factorise
                   -> [((Integer,Int),PrimalityProof)]
                                                    -- ^ List of prime factors, exponents and primality proofs
certiFactorisation primeBound primeTest prng seed mbdigs n
    = case ptest n of
        Just proof -> [((n,1),proof)]
        Nothing -> evalState (fact n digits) seed
      where
        digits = fromMaybe 8 mbdigs
        mult 1 xs = xs
        mult j xs = [((p,j*k),c) | ((p,k),c) <- xs]
        vdb xs = [(p,2*e) | (p,e) <- xs]
        dbl (u,v) = (mult 2 u, vdb v)
        ptest = case primeBound of
                  Just bd -> \k -> if k <= bd then (Just $ smallCert k) else primeTest k
                  Nothing -> primeTest
        rndR k = state (\gen -> prng k gen)
        fact m digs = do let (b1,b2,ct) = findParms digs
                         (pfs,cfs) <- repFact m b1 b2 ct
                         if null cfs
                           then return pfs
                           else do
                               nfs <- forM cfs $ \(k,j) ->
                                   mult j <$> fact k (if null pfs then digs+4 else digs)
                               return (mergeAll $ pfs:nfs)
        repFact m b1 b2 count
            | count < 0 = return ([],[(m,1)])
            | otherwise = do
                s <- rndR m
                case montgomeryFactorisation m b1 b2 s of
                  Nothing -> repFact m b1 b2 (count-1)
                  Just d  -> do
                      let !cof = m `quot` d
                      case gcd cof d of
                        1 -> do
                            (dp,dc) <- case ptest d of
                                         Just proof -> return ([((d,1),proof)],[])
                                         Nothing -> repFact d b1 b2 (count-1)
                            (cp,cc) <- case ptest cof of
                                         Just proof -> return ([((cof,1),proof)],[])
                                         Nothing -> repFact cof b1 b2 (count-1)
                            return (merge dp cp, dc ++ cc)
                        g -> do
                            let d' = d `quot` g
                                c' = cof `quot` g
                            (dp,dc) <- case ptest d' of
                                         Just proof -> return ([((d',1),proof)],[])
                                         Nothing -> repFact d' b1 b2 (count-1)
                            (cp,cc) <- case ptest c' of
                                         Just proof -> return ([((c',1),proof)],[])
                                         Nothing -> repFact c' b1 b2 (count-1)
                            (gp,gc) <- case ptest g of
                                         Just proof -> return ([((g,2),proof)],[])
                                         Nothing -> dbl <$> repFact g b1 b2 (count-1)
                            return  (mergeAll [dp,cp,gp], dc ++ cc ++ gc)

-- | merge two lists of factors, so that the result is strictly increasing (wrt the primes)
merge :: [((Integer,Int),PrimalityProof)] -> [((Integer,Int),PrimalityProof)] -> [((Integer,Int),PrimalityProof)]
merge xxs@(x@((p,e),c):xs) yys@(y@((q,d),_):ys)
    = case compare p q of
        LT -> x : merge xs yys
        EQ -> ((p,e+d),c) : merge xs ys
        GT -> y : merge xxs ys
merge [] ys = ys
merge xs _  = xs

-- | merge a list of lists of factors so that the result is strictly increasing (wrt the primes)
mergeAll :: [[((Integer,Int),PrimalityProof)]] -> [((Integer,Int),PrimalityProof)]
mergeAll [] = []
mergeAll [xs] = xs
mergeAll (xs:ys:zss) = merge (merge xs ys) (mergeAll zss)

-- | message for an invalid proof, should never be used
invalid :: Integer -> PrimalityProof -> String
invalid p prf = unlines
                    [ "\nInvalid primality proof constructed, please report this to the package maintainer!"
                    , "The supposed prime was:\n"
                    , show p
                    , "\nThe presumed proof was:\n"
                    , show prf
                    ]
