-- |
-- Module:          Math.NumberTheory.Primes.Sieve.Misc
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
{-# LANGUAGE CPP, BangPatterns, ScopedTypeVariables, MonoLocalBinds #-}
#if __GLASGOW_HASKELL__ >= 700
{-# OPTIONS_GHC -fspec-constr-count=16 #-}
#endif
module Math.NumberTheory.Primes.Sieve.Misc
    ( factorSieve
    , sieveFactor
    , totientSieve
    , carmichaelSieve
    ) where

import Control.Monad.ST
import Data.Array.Base (unsafeRead, unsafeWrite, unsafeAt, unsafeNewArray_)
import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad (when)
import Data.Bits

-- import Math.NumberTheory.Primes.Sieve.Types
import Math.NumberTheory.Powers.Squares (integerSquareRoot)
import Math.NumberTheory.Primes.Sieve.Indexing
import Math.NumberTheory.Utils


data FactorSieve = FS {-# UNPACK #-} !Int {-# UNPACK #-} !(UArray Int Int)

data TotientSieve = TS {-# UNPACK #-} !Int {-# UNPACK #-} !(UArray Int Int)

data CarmichaelSieve = CS {-# UNPACK #-} !Int {-# UNPACK #-} !(UArray Int Int)


factorSieve :: Integer -> FactorSieve
factorSieve bound
  | fromIntegral (maxBound :: Int) < bound  = error "factorSieve: would overflow"
  | bound < 2   = error "factorSieve: bound must be at least 2"
  | bound < 7   = FS bnd (array (0,0) [(0,0)])
  | otherwise   = FS bnd (runSTUArray (spfSieve bnd))
    where
      bnd = fromInteger bound

sieveFactor :: FactorSieve -> Integer -> [(Integer,Int)]
sieveFactor (FS bnd sve) = check
  where
    bound = fromIntegral bnd
    check 0 = error "0 has no prime factorisation"
    check 1 = []
    check n
      | n < 0       = (-1,1) : check (-n)
      | otherwise   = go2 n
    go2 :: Integer -> [(Integer,Int)]
    go2 n = case shiftToOddCount n of
              (0,_) -> go3 n
              (k,m) -> (2,k) : if m == 1 then [] else go3 m
    go3 n = case splitOff 3 n of
              (0,_) -> go5 n
              (k,m) -> (3,k) : if m == 1 then [] else go5 m
    go5 n = case splitOff 5 n of
              (0,_) -> sieveLoop n
              (k,m) -> (5,k) : case m of
                                 1 -> []
                                 j | j < 49 -> [(j,1)]
                                   | otherwise -> sieveLoop j
    sieveLoop n
        | bound < n  = tdLoop n (integerSquareRoot n) 0
        | otherwise = intLoop (fromIntegral n)
    intLoop n = case unsafeAt sve (toIdx n) of
                  !pix -> case toPrim pix of
                            !p -> case splitOff p n of
                                    (k,m) -> (fromIntegral p, k) : case m of
                                                                     1 -> []
                                                                     j -> intLoop j
    lstIdx = snd (bounds sve)
    tdLoop n sr ix
        | lstIdx < ix   = undefined     -- TODO: go for elliptic curve
        | sr < p        = [(n,1)]
        | ix /= pix     = tdLoop n sr (ix+1)    -- not a prime
        | otherwise     = case splitOff p n of
                            (0,_) -> tdLoop n sr (ix+1)
                            (k,m) -> (p,k) : case m of
                                               1 -> []
                                               j | j <= bound -> intLoop (fromIntegral j)
                                                 | otherwise -> tdLoop j (integerSquareRoot j) (ix+1)
          where
            p = toPrim ix
            pix = unsafeAt sve ix

totientSieve :: Integer -> TotientSieve
totientSieve bound
  | fromIntegral (maxBound :: Int) < bound  = error "totientSieve: would overflow"
  | bound < 2   = error "totientSieve: bound must be at least 2"
  | bound < 7   = TS bnd (array (0,0) [(0,0)])
  | otherwise   = TS bnd (totSieve bnd)
    where
      bnd = fromInteger bound

sieveTotient :: TotientSieve -> Integer -> Integer
sieveTotient (TS bnd sve) = check
  where
    bound = fromIntegral bnd
    check n
        | n < 1     = error "Totient only defined for positive numbers"
        | otherwise = go2 n
    go2 n = case shiftToOddCount n of
              (0,_) -> go3 1 n
              (k,m) -> let tt = (shiftL 1 (k-1)) in if m == 1 then tt else go3 tt m
    go3 !tt n = case splitOff 3 n of
                  (0,_) -> go5 tt n
                  (k,m) -> let nt = tt*(2*3^(k-1)) in if m == 1 then nt else go5 nt m
    go5 !tt n = case splitOff 5 n of
                  (0,_) -> sieveLoop tt n
                  (k,m) -> let nt = tt*(4*5^(k-1)) in if m == 1 then nt else sieveLoop nt n

carmichaelSieve :: Integer -> CarmichaelSieve
carmichaelSieve bound
  | fromIntegral (maxBound :: Int) < bound  = error "carmichaelSieve: would overflow"
  | bound < 2   = error "carmichaelSieve: bound must be at least 2"
  | bound < 7   = CS bnd (array (0,0) [(0,0)])
  | otherwise   = CS bnd (carSieve bnd)
    where
      bnd = fromInteger bound


spfSieve :: forall s. Int -> ST s (STUArray s Int Int)
spfSieve bound = do
  let (octs,lidx) = idxPr bound
      !mxidx = 8*octs+lidx
      mxval = 30*octs + rho lidx
      !mxsve = integerSquareRoot mxval
      (kr,r) = idxPr mxsve
      !svbd = 8*kr+r
  ar <- unsafeNewArray_ (0,mxidx) :: ST s (STUArray s Int Int)
  let fill i
        | mxidx < i = return ()
        | otherwise = do
          unsafeWrite ar i i
          fill (i+1)
      start k i = 8*(k*(30*k+2*rho i) + byte i) + idx i
      tick p stp off j ix
        | mxidx < ix    = return ()
        | otherwise = do
          s <- unsafeRead ar ix
          when (s == ix) (unsafeWrite ar ix p)
          tick p stp off ((j+1) .&. 7) (ix + stp*delta j + tau (off+j))
      sift ix
        | svbd < ix = return ar
        | otherwise = do
          p <- unsafeRead ar ix
          when (p == ix)  (do let i = ix .&. 7
                                  k = ix `shiftR` 3
                                  !off = i `shiftL` 3
                                  !stp = ix - i
                              tick ix stp off i (start k i))
          sift (ix+1)
  fill 0
  sift 0

totSieve :: Int -> UArray Int Int
totSieve bound = runSTUArray $ do
    ar <- spfSieve bound
    (_,lst) <- getBounds ar
    let tot ix
          | lst < ix    = return ar
          | otherwise   = do
            spf <- unsafeRead ar ix
            if spf == ix
                then unsafeWrite ar ix (toPrim ix - 1)
                else do let !p = toPrim spf
                            !n = toPrim ix
                            (tp,m) = unFact p (n `quot` p)
                        case m of
                          1 -> unsafeWrite ar ix tp
                          _ -> do
                            tm <- unsafeRead ar (toIdx m)
                            unsafeWrite ar ix (tp*tm)
            tot (ix+1)
    tot 0

carSieve :: Int -> UArray Int Int
carSieve bound = runSTUArray $ do
    ar <- spfSieve bound
    (_,lst) <- getBounds ar
    let car ix
          | lst < ix    = return ar
          | otherwise   = do
            spf <- unsafeRead ar ix
            if spf == ix
                then unsafeWrite ar ix (toPrim ix - 1)
                else do let !p = toPrim spf
                            !n = toPrim ix
                            (tp,m) = unFact p (n `quot` p)
                        case m of
                          1 -> unsafeWrite ar ix tp
                          _ -> do
                            tm <- unsafeRead ar (toIdx m)
                            unsafeWrite ar ix (lcm tp tm)
            car (ix+1)
    car 0

-- Find the p-part of the totient of (p*m) and the cofactor
-- of the p-power in m.
{-# INLINE unFact #-}
unFact :: Int -> Int -> (Int,Int)
unFact p m = go (p-1) m
  where
    go !tt k = case k `quotRem` p of
                 (q,0) -> go (p*tt) q
                 _ -> (tt,k)
