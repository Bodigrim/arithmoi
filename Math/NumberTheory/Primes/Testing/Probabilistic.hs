-- |
-- Module:      Math.NumberTheory.Primes.Testing.Probabilistic
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Probabilistic primality tests, Miller-Rabin and Baillie-PSW.
{-# LANGUAGE CPP, MagicHash, BangPatterns #-}
{-# OPTIONS_HADDOCK hide #-}
module Math.NumberTheory.Primes.Testing.Probabilistic
  ( isPrime
  , millerRabinV
  , bailliePSW
  , isStrongFermatPP
  , isFermatPP
  , lucasTest
  ) where

#include "MachDeps.h"

import Math.NumberTheory.Moduli
import Math.NumberTheory.Utils
import Math.NumberTheory.Powers.Squares

import Data.Bits

import GHC.Base

import GHC.Integer.GMP.Internals

-- | @'isPrime' n@ tests whether @n@ is a prime (negative or positive).
--   First, trial division by the primes less than @1200@ is performed.
--   If that hasn't determined primality or compositeness, a Baillie PSW
--   test is performed.
--
--   Since the Baillie PSW test may not be perfect, it is possible that
--   some large composites are wrongly deemed prime, however, no composites
--   passing the test are known and none exist below @2^64@.
isPrime :: Integer -> Bool
isPrime n
  | n < 0       = isPrime (-n)
  | n < 2       = False
  | n < 4       = True
  | otherwise   = go smallPrimes
    where
      go (p:ps)
        | p*p > n   = True
        | otherwise = case n `rem` p of
                        0 -> False
                        _ -> go ps
      go [] = bailliePSW n

-- | A Miller-Rabin like probabilistic primality test with preceding
--   trial division. While the classic Miller-Rabin test uses
--   randomly chosen bases, @'millerRabinV' k n@ uses the @k@
--   smallest primes as bases if trial division has not reached
--   a conclusive result. (Only the primes up to @1200@ are
--   available in this module, so the maximal effective @k@ is @196@.)
millerRabinV :: Int -> Integer -> Bool
millerRabinV k n
  | n < 0       = millerRabinV k (-n)
  | n < 2       = False
  | n < 4       = True
  | otherwise   = go smallPrimes
    where
      go (p:ps)
        | p*p > n   = True
        | otherwise = (n `rem` p /= 0) && go ps
      go [] = all (isStrongFermatPP n) (take k smallPrimes)

-- | @'isStrongFermatPP' n b@ tests whether @n@ is a strong Fermat
--   probable prime for base @b@, where @n > 2@ and @1 < b < n@.
--   The conditions on the arguments are not checked.
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
--   Some notes about the choice of bases: @b@ is a strong Fermat base
--   for @n@ if and only if @n-b@ is, hence one needs only test @b <= (n-1)/2@.
--   If @b@ is a strong Fermat base for @n@, then so is @b^k `mod` n@ for
--   all @k > 1@, hence one needs not test perfect powers, since their
--   base yields a stronger condition. Finally, if @a@ and @b@ are strong
--   Fermat bases for @n@, then @a*b@ is in most cases a strong Fermat
--   base for @n@, it can only fail to be so if @n `mod` 4 == 1@ and
--   the strong Fermat condition is reached at the same step for @a@ as for @b@,
--   so primes are the most powerful bases to test.
isStrongFermatPP :: Integer -> Integer -> Bool
isStrongFermatPP n b = a == 1 || go t a
  where
    m = n-1
    (t,u) = shiftToOddCount m
    a = powerModInteger' b u n
    go 0 _ = False
    go k x = x == m || go (k-1) ((x*x) `rem` n)

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
isFermatPP n b = powerModInteger' b (n-1) n == 1

-- | Primality test after Baillie, Pomerance, Selfridge and Wagstaff.
--   The Baillie PSW test consists of a strong Fermat probable primality
--   test followed by a (strong) Lucas primality test. This implementation
--   assumes that the number @n@ to test is odd and larger than @3@.
--   Even and small numbers have to be handled before. Also, before
--   applying this test, trial division by small primes should be performed
--   to identify many composites cheaply (although the Baillie PSW test is
--   rather fast, about the same speed as a strong Fermat test for four or
--   five bases usually, it is, for large numbers, much more costly than
--   trial division by small primes, the primes less than @1000@, say, so
--   eliminating numbers with small prime factors beforehand is more efficient).
--
--   The Baillie PSW test is very reliable, so far no composite numbers
--   passing it are known, and it is known (Gilchrist 2010) that no
--   Baillie PSW pseudoprimes exist below @2^64@. However, a heuristic argument
--   by Pomerance indicates that there are likely infinitely many Baillie PSW
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
  | square || d == 0    = False
  | d == 1              = True
  | otherwise           = uo == 0 || go t vo qo
    where
      square = isPossibleSquare2 n && r*r == n
      r = integerSquareRoot n
      d = find True 5
      find !pos cd = case jacobi' (n `rem` cd) cd of
                       0 -> if cd == n then 1 else 0
                       1 -> find (not pos) (cd+2)
                       _ -> if pos then cd else (-cd)
      q = (1-d) `quot` 4
      (t,o) = shiftToOddCount (n+1)
      (uo, vo, qo) = testLucas n q o
      go 0 _ _ = False
      go s vn qn = vn == 0 || go (s-1) ((vn*vn-2*qn) `rem` n) ((qn*qn) `rem` n)


-- n odd positive, n > abs q, index odd
testLucas :: Integer -> Integer -> Integer -> (Integer, Integer, Integer)
testLucas n q (S# i#) = look (WORD_SIZE_IN_BITS - 2)
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
#if __GLASGOW_HASKELL__ < 709
testLucas n q (J# s# ba#) = test (s# -# 1#)
  where
    test j# = case indexWordArray# ba# j# of
#else
testLucas n q (Jp# bn#) = test (s# -# 1#)
  where
    s# = sizeofBigNat# bn#
    test j# = case indexBigNat# bn# j# of
#endif
                0## -> test (j# -# 1#)
                w# -> look (j# -# 1#) (W# w#) (WORD_SIZE_IN_BITS - 1)
    look j# w i
      | testBit w i = go j# w (i - 1) 1 1 1 q
      | otherwise   = look j# w (i-1)
    go k# w i un un1 vn qn
      | i < 0       = if isTrue# (k# <# 0#)
                         then (un,vn,qn)
#if __GLASGOW_HASKELL__ < 709
                         else go (k# -# 1#) (W# (indexWordArray# ba# k#)) (WORD_SIZE_IN_BITS - 1) un un1 vn qn
#else
                         else go (k# -# 1#) (W# (indexBigNat# bn# k#)) (WORD_SIZE_IN_BITS - 1) un un1 vn qn
#endif
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
#if __GLASGOW_HASKELL__ >= 709
-- Listed as a precondition of lucasTest
testLucas _ _ _ = error "lucasTest: negative argument"
#endif

smallPrimes :: [Integer]
smallPrimes = 2:3:5:prs
  where
    prs = 7:11:filter isPr (takeWhile (< 1200) . scanl (+) 13 $ cycle [4,2,4,6,2,6,4,2])
    isPr n = td n prs
    td n (p:ps) = (p*p > n) || (n `rem` p /= 0 && td n ps)
    td _ []     = True
