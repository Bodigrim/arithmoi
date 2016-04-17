-- |
-- Module:      Math.NumberTheory.Moduli
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Miscellaneous functions related to modular arithmetic.
--
{-# LANGUAGE CPP, BangPatterns #-}
module Math.NumberTheory.Moduli
    ( -- * Functions with input check
      jacobi
    , invertMod
    , powerMod
    , powerModInteger
    , chineseRemainder
      -- ** Partially checked input
    , sqrtModP
      -- * Unchecked functions
    , jacobi'
    , powerMod'
    , powerModInteger'
    , sqrtModPList
    , sqrtModP'
    , tonelliShanks
    , sqrtModPP
    , sqrtModPPList
    , sqrtModF
    , sqrtModFList
    , chineseRemainder2
    ) where

#include "MachDeps.h"

#if __GLASGOW_HASKELL__ < 709 || WORD_SIZE_IN_BITS == 32
import Data.Word
#endif
import Data.Bits
import Data.Array.Unboxed
import Data.List (nub)
import Control.Monad (foldM, liftM2)

import Math.NumberTheory.Utils (shiftToOddCount, splitOff)
import Math.NumberTheory.GCD (extendedGCD)
import Math.NumberTheory.Primes.Heap (sieveFrom)
import Math.NumberTheory.Unsafe

-- Guesstimated startup time for the Heap algorithm is lower than
-- the cost to sieve an entire chunk.

-- | Invert a number relative to a positive modulus.
--   If @number@ and @modulus@ are coprime, the result is
--   @Just inverse@ where
--
-- >    (number * inverse) `mod` modulus == 1
-- >    0 <= inverse < modulus
--
--   If @number `mod` modulus == 0@ or @gcd number modulus > 1@, the result is @Nothing@.
invertMod :: Integer -> Integer -> Maybe Integer
invertMod k m
  | m <= 0 = error "Math.NumberTheory.Moduli.invertMod: non-positive modulus"
  | otherwise = wrap $ go False 1 0 m k'
  where
    k' | r < 0     = r+m
       | otherwise = r
         where
           r = k `rem` m
    wrap x = case (x*k') `rem` m of
               1 -> Just x
               _ -> Nothing
    -- Calculate modular inverse of k' modulo m by continued fraction expansion
    -- of m/k', say [a_0,a_1,...,a_s]. Let the convergents be p_j/q_j.
    -- Starting from j = -2, the arguments of go are
    -- (p_j/q_j) > m/k', p_{j+1}, p_j, and n, d with n/d = [a_{j+2},...,a_s].
    -- Since m/k' = p_s/q_s, and p_j*q_{j+1} - p_{j+1}*q_j = (-1)^(j+1), we have
    -- p_{s-1}*k' - q_{s-1}*m = (-1)^s * gcd m k', so if the inverse exists,
    -- it is either p_{s-1} or -p_{s-1}, depending on whether s is even or odd.
    go !b _ po _ 0 = if b then po else (m-po)
    go b !pn po n d = case n `quotRem` d of
                        (q,r) -> go (not b) (q*pn+po) pn d r

-- | Jacobi symbol of two numbers.
--   The \"denominator\" must be odd and positive, this condition is checked.
--
--   If both numbers have a common prime factor, the result
--   is @0@, otherwise it is &#177;1.
{-# SPECIALISE jacobi :: Integer -> Integer -> Int,
                         Int -> Int -> Int,
                         Word -> Word -> Int
  #-}
jacobi :: (Integral a, Bits a) => a -> a -> Int
jacobi a b
  | b < 0       = error "Math.NumberTheory.Moduli.jacobi: negative denominator"
  | evenI b     = error "Math.NumberTheory.Moduli.jacobi: even denominator"
  | b == 1      = 1
  | a == 0      = 0
  | a == 1      = 1
  | otherwise   = jacobi' a b   -- b odd, > 1, a neither 0 or 1

-- Invariant: b > 1 and odd
-- | Jacobi symbol of two numbers without validity check of
--   the \"denominator\".
{-# SPECIALISE jacobi' :: Integer -> Integer -> Int,
                          Int -> Int -> Int,
                          Word -> Word -> Int
  #-}
jacobi' :: (Integral a, Bits a) => a -> a -> Int
jacobi' a b
  | a == 0      = 0
  | a == 1      = 1
  | a < 0       = let n | rem4 b == 1 = 1
                        | otherwise   = -1
                      -- Blech, minBound may pose problems
                      (z,o) = shiftToOddCount (abs $ toInteger a)
                      s | evenI z || unsafeAt jac2 (rem8 b) == 1 = n
                        | otherwise                              = (-n)
                  in s*jacobi' (fromInteger o) b
  | a >= b      = case a `rem` b of
                    0 -> 0
                    r -> jacPS 1 r b
  | evenI a     = case shiftToOddCount a of
                    (z,o) -> let r = 2 - (rem4 o .&. rem4 b)
                                 s | evenI z || unsafeAt jac2 (rem8 b) == 1 = r
                                   | otherwise                              = (-r)
                             in jacOL s b o
  | otherwise   = case rem4 a .&. rem4 b of
                    3 -> jacOL (-1) b a
                    _ -> jacOL 1 b a

-- numerator positive and smaller than denominator
{-# SPECIALISE jacPS :: Int -> Integer -> Integer -> Int,
                        Int -> Int -> Int -> Int,
                        Int -> Word -> Word -> Int
  #-}
jacPS :: (Integral a, Bits a) => Int -> a -> a -> Int
jacPS !j a b
  | evenI a     = case shiftToOddCount a of
                    (z,o) | evenI z || unsafeAt jac2 (rem8 b) == 1 ->
                              jacOL (if rem4 o .&. rem4 b == 3 then (-j) else j) b o
                          | otherwise ->
                              jacOL (if rem4 o .&. rem4 b == 3 then j else (-j)) b o
  | otherwise   = jacOL (if rem4 a .&. rem4 b == 3 then (-j) else j) b a

-- numerator odd, positive and larger than denominator
{-# SPECIALISE jacOL :: Int -> Integer -> Integer -> Int,
                        Int -> Int -> Int -> Int,
                        Int -> Word -> Word -> Int
  #-}
jacOL :: (Integral a, Bits a) => Int -> a -> a -> Int
jacOL !j a b
  | b == 1    = j
  | otherwise = case a `rem` b of
                 0 -> 0
                 r -> jacPS j r b

-- | Modular power.
--
-- > powerMod base exponent modulus
--
--   calculates @(base ^ exponent) \`mod\` modulus@ by repeated squaring and reduction. Modulus must be positive.
--   If @exponent < 0@ and @base@ is invertible modulo @modulus@, @(inverse ^ |exponent|) \`mod\` modulus@
--   is calculated. This function does some input checking and sanitation before calling the unsafe worker.
{-# RULES
"powerMod/Integer" powerMod = powerModInteger
  #-}
{-# INLINE [1] powerMod #-}
powerMod :: (Integral a, Bits a) => Integer -> a -> Integer -> Integer
powerMod = powerModImpl

{-# SPECIALISE powerModImpl :: Integer -> Int -> Integer -> Integer,
                               Integer -> Word -> Integer -> Integer
  #-}
powerModImpl :: (Integral a, Bits a) => Integer -> a -> Integer -> Integer
powerModImpl base expo md
  | md <= 0     = error "Math.NumberTheory.Moduli.powerMod: non-positive modulus"
  | md == 1     = 0
  | expo == 0   = 1
  | bse' == 1   = 1
  | expo < 0    = case invertMod bse' md of
                    Just i  -> powerMod'Impl i (negate expo) md
                    Nothing -> error "Math.NumberTheory.Moduli.powerMod: Base isn't invertible with respect to modulus"
  | bse' == 0   = 0
  | otherwise   = powerMod'Impl bse' expo md
    where
      bse' = if base < 0 || md <= base then base `mod` md else base

-- | Modular power worker without input checking.
--   Assumes all arguments strictly positive and modulus greater than 1.
{-# RULES
"powerMod'/Integer" powerMod' = powerModInteger'
  #-}
{-# INLINE [1] powerMod' #-}
powerMod' :: (Integral a, Bits a) => Integer -> a -> Integer -> Integer
powerMod' = powerMod'Impl


{-# SPECIALISE powerMod'Impl :: Integer -> Int -> Integer -> Integer,
                                Integer -> Word -> Integer -> Integer
  #-}
powerMod'Impl :: (Integral a, Bits a) => Integer -> a -> Integer -> Integer
powerMod'Impl base expo md = go expo 1 base
  where
    go 1 !a !s  = (a*s) `rem` md
    go e a s
      | testBit e 0 = go (e `shiftR` 1) ((a*s) `rem` md) ((s*s) `rem` md)
      | otherwise   = go (e `shiftR` 1) a ((s*s) `rem` md)

-- | Specialised version of 'powerMod' for 'Integer' exponents.
--   Reduces the number of shifts of the exponent since shifting
--   large 'Integer's is expensive. Call this function directly
--   if you don't want or can't rely on rewrite rules. Modulus must be positive.
powerModInteger :: Integer -> Integer -> Integer -> Integer
powerModInteger base ex mdl
  | mdl <= 0     = error "Math.NumberTheory.Moduli.powerModInteger: non-positive modulus"
  | mdl == 1    = 0
  | ex == 0     = 1
  | ex < 0      = case invertMod bse' mdl of
                    Just i  -> powerModInteger' i (negate ex) mdl
                    Nothing -> error "Math.NumberTheory.Moduli.powerMod: Base isn't invertible with respect to modulus"
  | bse' == 0   = 0
  | bse' == 1   = 1
  | otherwise   = powerModInteger' bse' ex mdl
    where
      bse' = if base < 0 || mdl <= base then base `mod` mdl else base

-- | Specialised worker without input checks. Makes the same assumptions
--   as the general version 'powerMod''.
powerModInteger' :: Integer -> Integer -> Integer -> Integer
powerModInteger' base expo md = go w1 1 base e1
  where
    w1 = fromInteger expo
    e1 = expo `shiftR` 64
#if WORD_SIZE_IN_BITS == 32
  -- Shifting large Integers is expensive, hence we reduce the
  -- number of shifts by processing in 64-bit chunks.
  -- On 32-bit systems, every testBit on a Word64 would be a C-call,
  -- thus it is faster to split each Word64 into the constituent 32-bit
  -- Words and process those separately.
  -- The code becomes ugly, unfortunately.
    go :: Word64 -> Integer -> Integer -> Integer -> Integer
    go !w !a !s 0  = end a s w
    go w a s e = inner1 a s 0
      where
        wl :: Word
        !wl = fromIntegral w
        wh :: Word
        !wh = fromIntegral (w `shiftR` 32)
        inner1 !au !sq 32 = inner2 au sq 0
        inner1 au sq i
          | testBit wl i = inner1 ((au*sq) `rem` md) ((sq*sq) `rem` md) (i+1)
          | otherwise    = inner1 au ((sq*sq) `rem` md) (i+1)
        inner2 !au !sq 32 = go (fromInteger e) au sq (e `shiftR` 64)
        inner2 au sq i
          | testBit wh i = inner2 ((au*sq) `rem` md) ((sq*sq) `rem` md) (i+1)
          | otherwise    = inner2 au ((sq*sq) `rem` md) (i+1)
    end !a !s w
      | wh == 0   = fin a s wl
      | otherwise = innerE a s 0
        where
          wl :: Word
          !wl = fromIntegral w
          wh :: Word
          !wh = fromIntegral (w `shiftR` 32)
          innerE !au !sq 32 = fin au sq wh
          innerE au sq i
            | testBit wl i = innerE ((au*sq) `rem` md) ((sq*sq) `rem` md) (i+1)
            | otherwise    = innerE au ((sq*sq) `rem` md) (i+1)
    fin :: Integer -> Integer -> Word -> Integer
    fin !a !s 1 = (a*s) `rem` md
    fin a s w
      | testBit w 0 = fin ((a*s) `rem` md) ((s*s) `rem` md) (w `shiftR` 1)
      | otherwise   = fin a ((s*s) `rem` md) (w `shiftR` 1)

#else
  -- WORD_SIZE_IN_BITS == 64, otherwise things wouldn't compile anyway
  -- Shorter code since we need not split each 64-bit word.
    go :: Word -> Integer -> Integer -> Integer -> Integer
    go !w !a !s 0  = end a s w
    go w a s e = inner a s 0
      where
        inner !au !sq 64 = go (fromInteger e) au sq (e `shiftR` 64)
        inner au sq i
          | testBit w i = inner ((au*sq) `rem` md) ((sq*sq) `rem` md) (i+1)
          | otherwise   = inner au ((sq*sq) `rem` md) (i+1)
    end !a !s 1 = (a*s) `rem` md
    end a s w
      | testBit w 0 = end ((a*s) `rem` md) ((s*s) `rem` md) (w `shiftR` 1)
      | otherwise   = end a ((s*s) `rem` md) (w `shiftR` 1)

#endif

-- | @sqrtModP n prime@ calculates a modular square root of @n@ modulo @prime@
--   if that exists. The second argument /must/ be a (positive) prime, otherwise
--   the computation may not terminate and if it does, may yield a wrong result.
--   The precondition is /not/ checked.
--
--   If @prime@ is a prime and @n@ a quadratic residue modulo @prime@, the result
--   is @Just r@ where @r^2 ≡ n (mod prime)@, if @n@ is a quadratic nonresidue,
--   the result is @Nothing@.
sqrtModP :: Integer -> Integer -> Maybe Integer
sqrtModP n 2 = Just (n `mod` 2)
sqrtModP n prime = case jacobi' n prime of
                     0 -> Just 0
                     1 -> Just (sqrtModP' (n `mod` prime) prime)
                     _ -> Nothing

-- | @sqrtModPList n prime@ computes the list of all square roots of @n@
--   modulo @prime@. @prime@ /must/ be a (positive) prime.
--   The precondition is /not/ checked.
sqrtModPList :: Integer -> Integer -> [Integer]
sqrtModPList n prime
    | prime == 2    = [n `mod` 2]
    | otherwise     = case sqrtModP n prime of
                        Just 0 -> [0]
                        Just r -> [r,prime-r] -- The group of units in Z/(p) is cyclic
                        _      -> []

-- | @sqrtModP' square prime@ finds a square root of @square@ modulo
--   prime. @prime@ /must/ be a (positive) prime, and @square@ /must/ be a positive
--   quadratic residue modulo @prime@, i.e. @'jacobi square prime == 1@.
--   The precondition is /not/ checked.
sqrtModP' :: Integer -> Integer -> Integer
sqrtModP' square prime
    | prime == 2    = square
    | rem4 prime == 3 = powerModInteger' square ((prime + 1) `quot` 4) prime
    | otherwise     = tonelliShanks square prime

-- | @tonelliShanks square prime@ calculates a square root of @square@
--   modulo @prime@, where @prime@ is a prime of the form @4*k + 1@ and
--   @square@ is a positive quadratic residue modulo @prime@, using the
--   Tonelli-Shanks algorithm.
--   No checks on the input are performed.
tonelliShanks :: Integer -> Integer -> Integer
tonelliShanks square prime = loop rc t1 generator log2
  where
    (log2,q) = shiftToOddCount (prime-1)
    nonSquare = findNonSquare prime
    generator = powerModInteger' nonSquare q prime
    rc = powerModInteger' square ((q+1) `quot` 2) prime
    t1 = powerModInteger' square q prime
    msqr x = (x*x) `rem` prime
    msquare 0 x = x
    msquare k x = msquare (k-1) (msqr x)
    findPeriod per 1 = per
    findPeriod per x = findPeriod (per+1) (msqr x)
    loop !r t c m
        | t == 1    = r
        | otherwise = loop nextR nextT nextC nextM
          where
            nextM = findPeriod 0 t
            b     = msquare (m - 1 - nextM) c
            nextR = (r*b) `rem` prime
            nextC = msqr b
            nextT = (t*nextC) `rem` prime

-- | @sqrtModPP n (prime,expo)@ calculates a square root of @n@
--   modulo @prime^expo@ if one exists. @prime@ /must/ be a
--   (positive) prime. @expo@ must be positive, @n@ must be coprime
--   to @prime@
sqrtModPP :: Integer -> (Integer,Int) -> Maybe Integer
sqrtModPP n (2,e) = sqM2P n e
sqrtModPP n (prime,expo) = case sqrtModP n prime of
                             Just r -> fixup r
                             _      -> Nothing
  where
    fixup r = let diff' = r*r-n
              in if diff' == 0
                   then Just r
                   else case splitOff prime diff' of
                          (e,q) | expo <= e -> Just r
                                | otherwise -> fmap (\inv -> hoist inv r (q `mod` prime) (prime^e)) (invertMod (2*r) prime)
                      --
    hoist inv root elim pp
        | diff' == 0    = root'
        | expo <= ex    = root'
        | otherwise     = hoist inv root' (nelim `mod` prime) (prime^ex)
          where
            root' = (root + (inv*(prime-elim))*pp) `mod` (prime*pp)
            diff' = root'*root' - n
            (ex, nelim) = splitOff prime diff'

-- dirty, dirty
sqM2P :: Integer -> Int -> Maybe Integer
sqM2P n e
    | e < 2     = Just (n `mod` 2)
    | n' == 0   = Just 0
    | e <= k    = Just 0
    | odd k     = Nothing
    | otherwise = fmap ((`mod` mdl) . (`shiftL` k2)) $ solve s e2
      where
        mdl = 1 `shiftL` e
        n' = n `mod` mdl
        (k,s) = shiftToOddCount n'
        k2 = k `quot` 2
        e2 = e-k
        solve _ 1 = Just 1
        solve 1 _ = Just 1
        solve r p
            | rem4 r == 3   = Nothing  -- otherwise r ≡ 1 (mod 4)
            | p == 2        = Just 1   -- otherwise p >= 3
            | rem8 r == 5   = Nothing  -- otherwise r ≡ 1 (mod 8)
            | otherwise     = fixup r (fst $ shiftToOddCount (r-1))
              where
                fixup x pw
                    | pw >= e2  = Just x
                    | otherwise = fixup x' pw'
                      where
                        x' = x + (1 `shiftL` (pw-1))
                        d = x'*x' - r
                        pw' = if d == 0 then e2 else fst (shiftToOddCount d)

-- | @sqrtModF n primePowers@ calculates a square root of @n@ modulo
--   @product [p^k | (p,k) <- primePowers]@ if one exists and all primes
--   are distinct.
--   The list must be non-empty, @n@ must be coprime with all primes.
sqrtModF :: Integer -> [(Integer,Int)] -> Maybe Integer
sqrtModF _ []  = Nothing
sqrtModF n pps = do roots <- mapM (sqrtModPP n) pps
                    chineseRemainder $ zip roots (map (uncurry (^)) pps)

-- | @sqrtModFList n primePowers@ calculates all square roots of @n@ modulo
--   @product [p^k | (p,k) <- primePowers]@ if all primes are distinct.
--   The list must be non-empty, @n@ must be coprime with all primes.
sqrtModFList :: Integer -> [(Integer,Int)] -> [Integer]
sqrtModFList _ []  = []
sqrtModFList n pps = map fst $ foldl1 (liftM2 comb) cs
  where
    ms :: [Integer]
    ms = map (uncurry (^)) pps
    rs :: [[Integer]]
    rs = map (sqrtModPPList n) pps
    cs :: [[(Integer,Integer)]]
    cs = zipWith (\l m -> map (\x -> (x,m)) l) rs ms
    comb t1@(_,m1) t2@(_,m2) = (chineseRemainder2 t1 t2,m1*m2)

-- | @sqrtModPPList n (prime,expo)@ calculates the list of all
--   square roots of @n@ modulo @prime^expo@. The same restriction
--   as in 'sqrtModPP' applies to the arguments.
sqrtModPPList :: Integer -> (Integer,Int) -> [Integer]
sqrtModPPList n (2,1) = [n `mod` 2]
sqrtModPPList n (2,expo)
    = case sqM2P n expo of
        Just r -> let m = 1 `shiftL` (expo-1)
                  in nub [r, (r+m) `mod` (2*m), (m-r) `mod` (2*m), 2*m-r]
        _ -> []
sqrtModPPList n pe@(prime,expo)
    = case sqrtModPP n pe of
        Just 0 -> [0]
        Just r -> [prime^expo - r, r] -- The group of units in Z/(p^e) is cyclic
        _      -> []

-- | Given a list @[(r_1,m_1), ..., (r_n,m_n)]@ of @(residue,modulus)@
--   pairs, @chineseRemainder@ calculates the solution to the simultaneous
--   congruences
--
-- >
-- > r ≡ r_k (mod m_k)
-- >
--
--   if all moduli are positive and pairwise coprime. Otherwise
--   the result is @Nothing@ regardless of whether
--   a solution exists.
chineseRemainder :: [(Integer,Integer)] -> Maybe Integer
chineseRemainder remainders = foldM addRem 0 remainders
  where
    !modulus = product (map snd remainders)
    addRem acc (_,1) = Just acc
    addRem acc (r,m) = do
        let cf = modulus `quot` m
        inv <- invertMod cf m
        Just $! (acc + inv*cf*r) `mod` modulus

-- | @chineseRemainder2 (r_1,m_1) (r_2,m_2)@ calculates the solution of
--
-- >
-- > r ≡ r_k (mod m_k)
--
--   if @m_1@ and @m_2@ are coprime.
chineseRemainder2 :: (Integer,Integer) -> (Integer,Integer) -> Integer
chineseRemainder2 (r1, md1) (r2,md2)
    = case extendedGCD md1 md2 of
        (_,u,v) -> ((1 - u*md1)*r1 + (1 - v*md2)*r2) `mod` (md1*md2)

-- Utilities

-- For large Integers, going via Int is much faster than bit-fiddling
-- on the Integer, so we do that.
{-# SPECIALISE evenI :: Integer -> Bool,
                        Int -> Bool,
                        Word -> Bool
  #-}
evenI :: Integral a => a -> Bool
evenI n = fromIntegral n .&. 1 == (0 :: Int)

{-# SPECIALISE rem4 :: Integer -> Int,
                       Int -> Int,
                       Word -> Int
  #-}
rem4 :: Integral a => a -> Int
rem4 n = fromIntegral n .&. 3

{-# SPECIALISE rem8 :: Integer -> Int,
                       Int -> Int,
                       Word -> Int
  #-}
rem8 :: Integral a => a -> Int
rem8 n = fromIntegral n .&. 7

jac2 :: UArray Int Int
jac2 = array (0,7) [(0,0),(1,1),(2,0),(3,-1),(4,0),(5,-1),(6,0),(7,1)]

findNonSquare :: Integer -> Integer
findNonSquare n
    | rem8 n == 5 || rem8 n == 3  = 2
    | otherwise = search primelist
      where
        primelist = [3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67]
                        ++ sieveFrom (68 + n `rem` 4) -- prevent sharing
        search (p:ps)
            | jacobi' p n == -1 = p
            | otherwise         = search ps
        search _ = error "Should never have happened, prime list exhausted."
