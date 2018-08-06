-- |
-- Module:      Math.NumberTheory.Moduli.Sqrt
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Modular square roots.
--

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP          #-}

module Math.NumberTheory.Moduli.Sqrt
  ( -- | Constructor is exported in order to do pattern-matching.
    --   Should not be used ideally to contruct because it does not check preconditions.
    --   Use @'toFieldCharacteristic'@ instead.
    FieldCharacteristic(..)
  , toFieldCharacteristic
  --  | Constructor is exported in order to do pattern-matching.
  --   Should not be used ideally to contruct because it does not check preconditions.
  --   Use @'toResidue'@ instead.
  , QuadraticResidue(..)
  , toQuadraticResidue
  , sqrtModList
  , sqrtModMaybe
  , sqrtModExact
  , sqrtModF
  , sqrtModFList
  ) where

import Control.Arrow (first)
import Control.Monad (liftM2)
import Data.Bits
import Data.Coerce
import Data.List (nub)
import GHC.Integer.GMP.Internals

import Math.NumberTheory.Moduli.Chinese
import Math.NumberTheory.Moduli.Jacobi
import Math.NumberTheory.Powers.General (highestPower)
import Math.NumberTheory.Primes.Types
import Math.NumberTheory.Primes.Sieve (sieveFrom)
import Math.NumberTheory.Primes.Testing.Probabilistic (isPrime)
import Math.NumberTheory.Utils (shiftToOddCount, splitOff)

import Math.NumberTheory.Utils.FromIntegral

-- | Extended flag structure to check if square root is computed in finite field.
data FieldCharacteristic = FieldCharacteristic (Prime Integer) Int
  deriving Show

-- | Flag structure to check if residue is quadratic by modulo.
--   Condition is checked during construction.
data QuadraticResidue = QuadraticResidue (Prime Integer) Integer deriving Show

-- | Duplicate functions from @'instance UniqueFactorisation Integer'@. They cannot be imported because of cycle imports.
unPrime :: PrimeNat -> Integer
unPrime = coerce naturalToInteger

primeF :: Integer -> PrimeNat
primeF = coerce integerToNatural

-- | Check if value is power of some prime.
checkPrimePower :: Integer -> Maybe (Prime Integer, Int)
checkPrimePower p
  | isPrime . fst . highestPower $ p = Just . first primeF . highestPower $ p
  | otherwise = Nothing

-- | Check if argument is valid characteristic of finite field.
--   It's a precondition for computations in functions like @'sqrtModP'@ and @'sqrtModPP'@
toFieldCharacteristic :: Integer -> Maybe FieldCharacteristic
toFieldCharacteristic p
  | isPrime p = Just $ FieldCharacteristic (primeF p) 1
  | otherwise = uncurry FieldCharacteristic <$> checkPrimePower p

-- | Check if second argument is quadratic resudue modulo @'p'@ or not.
toQuadraticResidue :: Prime Integer -> Integer -> Maybe QuadraticResidue
toQuadraticResidue p'@(unPrime -> p) n
  | n^((p-1) `div` p) `mod` p == 1 = Just $ QuadraticResidue p' n
  | otherwise = Nothing

-- * Interface functions with preconditions checking.

-- | Gets quadratic residue modulo prime and computes it's square root.
--   Preconditions: @'n'@ is quadratic residue modulo @'p'@, @'p'@ is prime.
--   All preconditions are cheched earlier and reflected to signature.
sqrtModExact :: QuadraticResidue -> Integer
sqrtModExact (QuadraticResidue (unPrime -> p) n) = sqrtModP' n p

-- | Gets integer and computes its square root in field with given characteristic - because it exists only in field.
--   Returns all roots.
--   Preconditions: p is prime and second argument itself is valid characteristic (it's guaranteed by type).
--   All preconditions are cheched earlier and reflected to signature.
sqrtModList :: Integer -> FieldCharacteristic -> [Integer]
sqrtModList n (FieldCharacteristic (unPrime -> p) 1) = sqrtModPList n p
sqrtModList n (FieldCharacteristic (unPrime -> p) pow) = sqrtModPPList n (p,pow)

-- | Gets integer and computes its square root in field with given characteristic - because it exists only in field.
--   Returns only one root or none.
--   Preconditions: p is prime and second argument itself is valid characteristic (it's guaranteed by type).
--   All preconditions are cheched earlier and reflected to signature.
sqrtModMaybe :: Integer -> FieldCharacteristic -> Maybe Integer
sqrtModMaybe n (FieldCharacteristic (unPrime -> p) 1) = sqrtModP n p
sqrtModMaybe n (FieldCharacteristic (unPrime -> p) pow) = sqrtModPP n (p,pow)

-- | @sqrtModP n prime@ calculates a modular square root of @n@ modulo @prime@
--   if that exists. The second argument /must/ be a (positive) prime, otherwise
--   the computation may not terminate and if it does, may yield a wrong result.
--   The precondition is checked in @'sqrtModMaybe'@ interface.
--
--   If @prime@ is a prime and @n@ a quadratic residue modulo @prime@, the result
--   is @Just r@ where @r^2 ≡ n (mod prime)@, if @n@ is a quadratic nonresidue,
--   the result is @Nothing@.
sqrtModP :: Integer -> Integer -> Maybe Integer
sqrtModP n 2 = Just (n `mod` 2)
sqrtModP n prime = case jacobi n prime of
                     MinusOne -> Nothing
                     Zero     -> Just 0
                     One      -> Just (sqrtModP' (n `mod` prime) prime)

-- | @sqrtModPList n prime@ computes the list of all square roots of @n@
--   modulo @prime@. @prime@ /must/ be a (positive) prime.
--   The precondition is checked in @'sqrtModList'@ interface.
sqrtModPList :: Integer -> Integer -> [Integer]
sqrtModPList n 2 = [n `mod` 2]
sqrtModPList n prime = case sqrtModP n prime of
                         Just 0 -> [0]
                         Just r -> [r,prime-r] -- The group of units in Z/(p) is cyclic
                         _      -> []

-- | @sqrtModP' square prime@ finds a square root of @square@ modulo
--   prime. @prime@ /must/ be a (positive) prime, and @square@ /must/ be a positive
--   quadratic residue modulo @prime@, i.e. @'jacobi square prime == 1@.
--   The precondition is checked with Euler's criterion in @'sqrtModExact'@ interface.
sqrtModP' :: Integer -> Integer -> Integer
sqrtModP' square prime
    | prime == 2    = square
    | rem4 prime == 3 = powModInteger square ((prime + 1) `quot` 4) prime
    | square `mod` prime == prime - 1
                    = sqrtOfMinusOne prime
    | otherwise     = tonelliShanks square prime

-- | p must be of form 4k + 1
sqrtOfMinusOne :: Integer -> Integer
sqrtOfMinusOne p
  = head
  $ filter (\n -> n /= 1 && n /= p - 1)
  $ map (\n -> powModInteger n k p)
    [2..p-2]
  where
    k = (p - 1) `quot` 4

-- | @tonelliShanks square prime@ calculates a square root of @square@
--   modulo @prime@, where @prime@ is a prime of the form @4*k + 1@ and
--   @square@ is a positive quadratic residue modulo @prime@, using the
--   Tonelli-Shanks algorithm.
--   No checks on the input are performed. But this function is internal.
tonelliShanks :: Integer -> Integer -> Integer
tonelliShanks square prime = loop rc t1 generator log2
  where
    (log2,q) = shiftToOddCount (prime-1)
    nonSquare = findNonSquare prime
    generator = powModInteger nonSquare q prime
    rc = powModInteger square ((q+1) `quot` 2) prime
    t1 = powModInteger square q prime
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
                                | otherwise -> fmap (\inv -> hoist inv r (q `mod` prime) (prime^e)) (recipMod (2*r) prime)

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
        solve r _
            | rem4 r == 3   = Nothing  -- otherwise r ≡ 1 (mod 4)
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
sqrtModF :: Integer -> [FieldCharacteristic] -> Maybe Integer
sqrtModF _ []  = Nothing
sqrtModF n pps = do roots <- mapM (sqrtModPP n . (\(FieldCharacteristic (unPrime -> p) pow) -> (p,pow))) pps
                    chineseRemainder $ zip roots (map (uncurry (^) . (\(FieldCharacteristic (unPrime -> p) pow) -> (p,pow))) pps)

-- | @sqrtModFList n primePowers@ calculates all square roots of @n@ modulo
--   @product [p^k | (p,k) <- primePowers]@ if all primes are distinct.
--   The list must be non-empty, @n@ must be coprime with all primes.
sqrtModFList :: Integer -> [FieldCharacteristic] -> [Integer]
sqrtModFList _ []  = []
sqrtModFList n pps = map fst $ foldl1 (liftM2 comb) cs
  where
    ms :: [Integer]
    ms = map (uncurry (^) . (\(FieldCharacteristic (unPrime -> p) pow) -> (p,pow))) pps
    rs :: [[Integer]]
    rs = map (sqrtModPPList n . (\(FieldCharacteristic (unPrime -> p) pow) -> (p,pow))) pps
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


-- Utilities

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

findNonSquare :: Integer -> Integer
findNonSquare n
    | rem8 n == 5 || rem8 n == 3  = 2
    | otherwise = search primelist
      where
        primelist = [3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67]
                        ++ sieveFrom (68 + n `rem` 4) -- prevent sharing
        search (p:ps) = case jacobi p n of
          MinusOne -> p
          _        -> search ps
        search _ = error "Should never have happened, prime list exhausted."

recipMod :: Integer -> Integer -> Maybe Integer
recipMod x m = case recipModInteger x m of
  0 -> Nothing
  y -> Just y
