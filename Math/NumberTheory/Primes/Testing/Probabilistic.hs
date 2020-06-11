-- |
-- Module:      Math.NumberTheory.Primes.Testing.Probabilistic
-- Copyright:   (c) 2011 Daniel Fischer, 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
--
-- Probabilistic primality tests, Miller-Rabin and Baillie-PSW.

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.Primes.Testing.Probabilistic
  ( isPrime
  , millerRabinV
  , bailliePSW
  , isStrongFermatPP
  , isFermatPP
  , lucasTest
  ) where

import Data.Bits
import Data.Mod
import Data.Proxy
import GHC.Base
import GHC.Integer.GMP.Internals
import GHC.TypeNats (KnownNat, SomeNat(..), someNatVal)

import Math.NumberTheory.Moduli.JacobiSymbol
import Math.NumberTheory.Utils
import Math.NumberTheory.Roots

-- | @isPrime n@ tests whether @n@ is a prime (negative or positive).
--   It is a combination of trial division and Baillie-PSW test.
--
--   If @isPrime n@ returns @False@ then @n@ is definitely composite.
--   There is a theoretical possibility that @isPrime n@ is @True@,
--   but in fact @n@ is not prime. However, no such numbers are known
--   and none exist below @2^64@. If you have found one, please report it,
--   because it is a major discovery.
isPrime :: Integer -> Bool
isPrime n
  | n < 0       = isPrime (-n)
  | n < 2       = False
  | n < 4       = True
  | otherwise   = millerRabinV 0 n -- trial division test
                  && bailliePSW n

-- | Miller-Rabin probabilistic primality test. It consists of the trial
-- division test and several rounds of the strong Fermat test with different
-- bases. The choice of trial divisors and bases are
-- implementation details and may change in future silently.
--
-- First argument stands for the number of rounds of strong Fermat test.
-- If it is 0, only trial division test is performed.
--
-- If @millerRabinV k n@ returns @False@ then @n@ is definitely composite.
-- Otherwise @n@ may appear composite with probability @1/4^k@.
millerRabinV :: Int -> Integer -> Bool
millerRabinV (I# k) n = case testPrimeInteger n k of
  0# -> False
  _  -> True

-- | @'isStrongFermatPP' n b@ tests whether non-negative @n@ is
--   a strong Fermat probable prime for base @b@.
--
--   Apart from primes, also some composite numbers have the tested
--   property, but those are rare. Very rare are composite numbers
--   having the property for many bases, so testing a large prime
--   candidate with several bases can identify composite numbers
--   with high probability. An odd number @n > 3@ is prime if and
--   only if @'isStrongFermatPP' n b@ holds for all @b@ with
--   @2 <= b <= (n-1)/2@, but of course checking all those bases
--   would be less efficient than trial division, so one normally
--   checks only a relatively small number of bases, depending on
--   the desired degree of certainty. The probability that a randomly
--   chosen base doesn't identify a composite number @n@ is less than
--   @1/4@, so five to ten tests give a reasonable level of certainty
--   in general.
--
--   Please consult <https://miller-rabin.appspot.com Deterministic variants of the Miller-Rabin primality test>
--   for the best choice of bases.
isStrongFermatPP :: Integer -> Integer -> Bool
isStrongFermatPP n b
  | n < 0          = error "isStrongFermatPP: negative argument"
  | n <= 1         = False
  | n == 2         = True
  | otherwise      = case someNatVal (fromInteger n) of
                     SomeNat (_ :: Proxy t) -> isStrongFermatPPMod (fromInteger b :: Mod t)

isStrongFermatPPMod :: KnownNat n => Mod n -> Bool
isStrongFermatPPMod b = b == 0 || a == 1 || go t a
  where
    m = -1
    (t, u) = shiftToOddCount $ unMod m
    a = b ^% u

    go 0 _ = False
    go k x = x == m || go (k - 1) (x * x)

-- | @'isFermatPP' n b@ tests whether @n@ is a Fermat probable prime
--   for the base @b@, that is, whether @b^(n-1) `mod` n == 1@.
--   This is a weaker but simpler condition. However, more is lost
--   in strength than is gained in simplicity, so for primality testing,
--   the strong check should be used. The remarks about
--   the choice of bases to test from @'isStrongFermatPP'@ apply
--   with the modification that if @a@ and @b@ are Fermat bases
--   for @n@, then @a*b@ /always/ is a Fermat base for @n@ too.
--   A /Charmichael number/ is a composite number @n@ which is a
--   Fermat probable prime for all bases @b@ coprime to @n@. By the
--   above, only primes @p <= n/2@ not dividing @n@ need to be tested
--   to identify Carmichael numbers (however, testing all those
--   primes would be less efficient than determining Carmichaelness
--   from the prime factorisation; but testing an appropriate number
--   of prime bases is reasonable to find out whether it's worth the
--   effort to undertake the prime factorisation).
isFermatPP :: Integer -> Integer -> Bool
isFermatPP n b = case someNatVal (fromInteger n) of
  SomeNat (_ :: Proxy t) -> (fromInteger b :: Mod t) ^% (n-1) == 1

-- | Primality test after Baillie, Pomerance, Selfridge and Wagstaff.
--   The Baillie-PSW test consists of a strong Fermat probable primality
--   test followed by a (strong) Lucas primality test. This implementation
--   assumes that the number @n@ to test is odd and larger than @3@.
--   Even and small numbers have to be handled before. Also, before
--   applying this test, trial division by small primes should be performed
--   to identify many composites cheaply (although the Baillie-PSW test is
--   rather fast, about the same speed as a strong Fermat test for four or
--   five bases usually, it is, for large numbers, much more costly than
--   trial division by small primes, the primes less than @1000@, say, so
--   eliminating numbers with small prime factors beforehand is more efficient).
--
--   The Baillie-PSW test is very reliable, so far no composite numbers
--   passing it are known, and it is known (Gilchrist 2010) that no
--   Baillie-PSW pseudoprimes exist below @2^64@. However, a heuristic argument
--   by Pomerance indicates that there are likely infinitely many Baillie-PSW
--   pseudoprimes. On the other hand, according to
--   <http://mathworld.wolfram.com/Baillie-PSWPrimalityTest.html> there is
--   reason to believe that there are none with less than several
--   thousand digits, so that for most use cases the test can be
--   considered definitive.
bailliePSW :: Integer -> Bool
bailliePSW n = isStrongFermatPP n 2 && lucasTest n

-- precondition: n odd, > 3 (no small prime factors, typically large)
-- | The Lucas-Selfridge test, including square-check, but without
--   the Fermat test. For package-internal use only.
lucasTest :: Integer -> Bool
lucasTest n
  | isSquare n || d == 0 = False
  | d == 1               = True
  | otherwise            = uo == 0 || go t vo qo
    where
      d = find True 5
      find !pos cd = case jacobi (n `rem` cd) cd of
                       MinusOne -> if pos then cd else (-cd)
                       Zero     -> if cd == n then 1 else 0
                       One      -> find (not pos) (cd+2)
      q = (1-d) `quot` 4
      (t,o) = shiftToOddCount (n+1)
      (uo, vo, qo) = testLucas n q o
      go 0 _ _ = False
      go s vn qn = vn == 0 || go (s-1) ((vn*vn-2*qn) `rem` n) ((qn*qn) `rem` n)


-- n odd positive, n > abs q, index odd
testLucas :: Integer -> Integer -> Integer -> (Integer, Integer, Integer)
testLucas n q (S# i#) = look (finiteBitSize (0 :: Word) - 2)
  where
    j = I# i#
    look k
      | testBit j k = go (k-1) 1 1 1 q
      | otherwise   = look (k-1)
    go k un un1 vn qn
      | k < 0       = (un, vn, qn)
      | testBit j k = go (k-1) u2n1 u2n2 v2n1 q2n1
      | otherwise   = go (k-1) u2n u2n1 v2n q2n
        where
          u2n   = (un*vn) `rem` n
          u2n1  = (un1*vn-qn) `rem` n
          u2n2  = ((un1-q*un)*vn-qn) `rem` n
          v2n   = (vn*vn-2*qn) `rem` n
          v2n1  = ((un1 - (2*q)*un)*vn-qn) `rem` n
          q2n   = (qn*qn) `rem` n
          q2n1  = (qn*qn*q) `rem` n
testLucas n q (Jp# bn#) = test (s# -# 1#)
  where
    s# = sizeofBigNat# bn#
    test j# = case indexBigNat# bn# j# of
                0## -> test (j# -# 1#)
                w# -> look (j# -# 1#) (W# w#) (finiteBitSize (0 :: Word) - 1)
    look j# w i
      | testBit w i = go j# w (i - 1) 1 1 1 q
      | otherwise   = look j# w (i-1)
    go k# w i un un1 vn qn
      | i < 0       = if isTrue# (k# <# 0#)
                         then (un,vn,qn)
                         else go (k# -# 1#) (W# (indexBigNat# bn# k#)) (finiteBitSize (0 :: Word) - 1) un un1 vn qn
      | testBit w i = go k# w (i-1) u2n1 u2n2 v2n1 q2n1
      | otherwise   = go k# w (i-1) u2n u2n1 v2n q2n
        where
          u2n   = (un*vn) `rem` n
          u2n1  = (un1*vn-qn) `rem` n
          u2n2  = ((un1-q*un)*vn-qn) `rem` n
          v2n   = (vn*vn-2*qn) `rem` n
          v2n1  = ((un1 - (2*q)*un)*vn-qn) `rem` n
          q2n   = (qn*qn) `rem` n
          q2n1  = (qn*qn*q) `rem` n
-- Listed as a precondition of lucasTest
testLucas _ _ _ = error "lucasTest: negative argument"
