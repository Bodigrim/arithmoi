-- |
-- Module:      Math.NumberTheory.Primes.Heap
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     MIT
-- Maintainer:  Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Prime generation using a priority queue for the composites.
-- The algorithm is basically the one described in
-- <http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf>, but
-- it uses a more efficient heap for the priority queue and a
-- larger wheel, thus it is faster (in particular, the speed
-- penalty for @'Integer'@ is much smaller) and uses less memory.
-- It is nevertheless very slow compared to a bit sieve.
-- This module is mainly intended for comparison and verification.
{-# LANGUAGE BangPatterns, CPP, MonoLocalBinds #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_GHC -fno-float-in -fno-spec-constr -fno-full-laziness #-}
module Math.NumberTheory.Primes.Heap (primes, sieveFrom) where

import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST
import Data.List (foldl')
#if __GLASGOW_HASKELL__ < 709
import Data.Word
#endif

import Math.NumberTheory.Unsafe

#ifndef SH_SIZE
#define SH_SIZE 31
#endif

-- Composites to eliminate, components are
-- composite, multiple of prime
-- prime
-- index of step to find next multiple of prime
data Del a = D !a !a {-# UNPACK #-} !Int

step :: Integral a => Int -> a
-- {-# INLINE step #-}
step i = fromIntegral (steps `unsafeAt` i)

-- Priority queue as baby heap
-- Invariant: left subheap one larger than right or both
-- have the same size (and of course, heap property)
data Hipp a
    = E
    | H !a !a {-# UNPACK #-} !Int !(Hipp a) !(Hipp a)


-- push composite-data down the heap
{-# SPECIALISE push :: Int -> Int -> Int -> Hipp Int -> Hipp Int #-}
{-# SPECIALISE push :: Word -> Word -> Int -> Hipp Word -> Hipp Word #-}
{-# SPECIALISE push :: Integer -> Integer -> Int -> Hipp Integer -> Hipp Integer #-}
push :: Integral a => a -> a -> Int -> Hipp a -> Hipp a
push !c !p !w = go
  where
    less = (< c)
    go (H hc hp hw l r)
        | less hc   = H hc hp hw (go r) l
        | otherwise = H c p w (push hc hp hw r) l
    go _ = H c p w E E

-- bubble down increased top to regain heap invariant
{-# SPECIALISE bubble :: Hipp Int -> Hipp Int #-}
{-# SPECIALISE bubble :: Hipp Word -> Hipp Word #-}
{-# SPECIALISE bubble :: Hipp Integer -> Hipp Integer #-}
bubble :: Integral a => Hipp a -> Hipp a
bubble h@(H c p w l r) =
    case r of
        E -> case l of
                E -> h
                H lc lp lw ll lr
                    | lc < c -> H lc lp lw (H c p w ll lr) r
                    | otherwise -> h
        H rc rp rw rl rr ->
          case l of
            H lc lp lw ll lr
                | lc < c -> if lc < rc
                              then H lc lp lw (mkHipp c p w ll lr) r
                              else H rc rp rw l (mkHipp c p w rl rr)
                | rc < c -> H rc rp rw l (mkHipp c p w rl rr)
                | otherwise -> h
            _ -> error "Heap invariant violated, left smaller than right!"
bubble h = h

-- join two heaps and composite-data
{-# SPECIALISE
    mkHipp :: Int -> Int -> Int -> Hipp Int -> Hipp Int -> Hipp Int,
              Integer -> Integer -> Int -> Hipp Integer -> Hipp Integer -> Hipp Integer,
              Word -> Word -> Int -> Hipp Word -> Hipp Word -> Hipp Word
  #-}
mkHipp :: Integral a => a -> a -> Int -> Hipp a -> Hipp a -> Hipp a
mkHipp !c !p !w = go
  where
    less = (< c)
    go l r =
      case r of
        E -> case l of
                E -> H c p w l r
                H lc lp lw _ _
                    | less lc -> H lc lp lw (H c p w E E) E
                    | otherwise -> H c p w l r
        H rc rp rw rl rr ->
          case l of
            H lc lp lw ll lr
                | less lc -> if lc < rc
                                then H lc lp lw (go ll lr) r
                                else H rc rp rw l (go rl rr)
                | less rc -> H rc rp rw l (go rl rr)
                | otherwise -> H c p w l r
            _ -> error "Heap invariant violated, left smaller than right!"

-- increase the top of the heap and re-heap
{-# SPECIALISE inc :: Hipp Int -> Hipp Int #-}
{-# SPECIALISE inc :: Hipp Word -> Hipp Word #-}
{-# SPECIALISE inc :: Hipp Integer -> Hipp Integer #-}
inc :: Integral a => Hipp a -> Hipp a
inc (H c p i l r)
  = {-# SCC "incBubble" #-} bubble (H (c+p*step i) p (nextIndex i) l r)
inc h   = h

-- while top of heap equals composite, increase and re-heap
{-# SPECIALISE adjust :: Int -> Hipp Int -> Hipp Int #-}
{-# SPECIALISE adjust :: Word -> Hipp Word -> Hipp Word #-}
{-# SPECIALISE adjust :: Integer -> Hipp Integer -> Hipp Integer #-}
adjust :: Integral a => a -> Hipp a -> Hipp a
adjust cm h@(H v _ _ _ _)
    | cm == v   = adjust cm (inc h)
adjust _ h      = h

-- build a heap from a sorted list of Del's
{-# SPECIALISE buildH :: [Del Int] -> Hipp Int #-}
{-# SPECIALISE buildH :: [Del Word] -> Hipp Word #-}
{-# SPECIALISE buildH :: [Del Integer] -> Hipp Integer #-}
buildH :: Integral a => [Del a] -> Hipp a
buildH [] = E
buildH (D s p w : tl) = H s p w l r
      where
        (ll,rl) = goSplit [] [] tl
        goSplit xs ys [] = (reverse ys, reverse xs)
        goSplit xs ys (d:ds) = goSplit ys (d:xs) ds
        l = buildH ll
        r = buildH rl

-- Simple sieve pushing each prime immediately onto the heap,
-- feeds the feeder, runs at about fourth root of the main sieve.
{-# SPECIALISE simpleSieve :: Hipp Int -> Int -> Int -> [Del Int] #-}
{-# SPECIALISE simpleSieve :: Hipp Word -> Word -> Int -> [Del Word] #-}
{-# SPECIALISE simpleSieve :: Hipp Integer -> Integer -> Int -> [Del Integer] #-}
simpleSieve :: Integral a => Hipp a -> a -> Int -> [Del a]
simpleSieve h@(H nc _ _ _ _) cd !i
  | cd < nc   = D s cd i : simpleSieve ({-# SCC "simplePush" #-} push s cd i h) (cd + step i) (nextIndex i)
    | otherwise = simpleSieve (adjust cd h) (cd + step i) (nextIndex i)
      where
        s = cd*cd
simpleSieve _ _ _ = []  -- would violate an invariant

-- Feeder sieve, produces composites at the rate of the progress of the main sieve,
-- hence primes at about the square root of it, thus needs about fourth root heap
-- space. The two-step feeding makes the feeder produce faster and hence the main
-- sieve (since we have only one O(n^0.5) heap and not two.
-- Using two heaps, one small for multiples of small primes which change often
-- and one for multiples of larger primes which are less frequently updated
-- speeds things up.
{-# SPECIALISE feederSieve :: [Del Int] -> Hipp Int -> Hipp Int -> Int -> Int -> [Del Int] #-}
{-# SPECIALISE feederSieve :: [Del Word] -> Hipp Word -> Hipp Word -> Word -> Int -> [Del Word] #-}
{-# SPECIALISE feederSieve :: [Del Integer] -> Hipp Integer -> Hipp Integer -> Integer -> Int -> [Del Integer] #-}
feederSieve :: Integral a => [Del a] -> Hipp a -> Hipp a -> a -> Int -> [Del a]
feederSieve dls@((D s p u):ds) sh@(H sc _ _ _ _) lh@(H lc _ _ _ _) cd i
    | cd == sc  = feederSieve dls (adjust cd (inc sh)) (adjust cd lh) cd' j
    | cd == lc  = feederSieve dls sh (adjust cd (inc lh)) cd' j
    | cd == s   = feederSieve ds sh (push (s + p*step u) p (nextIndex u) lh) cd' j
    | otherwise = D (cd*cd) cd i : feederSieve dls sh lh cd' j
      where
        !cd' = cd + step i
        !j   = nextIndex i
feederSieve _ _ _ _ _ = []  -- invariant violated

-- Build the feeder sieve, arguments are
-- first prime whose multiples have to be eliminated
-- index of step for this prime.
{-# SPECIALISE feeder :: Int -> Int -> [Del Int] #-}
{-# SPECIALISE feeder :: Word -> Int -> [Del Word] #-}
{-# SPECIALISE feeder :: Integer -> Int -> [Del Integer] #-}
feeder :: Integral a => a -> Int -> [Del a]
feeder p i = feederSieve lrg sh lh p i
      where
        (sml,D s lp w : lrg) = splitAt SH_SIZE (D q p i : {-# SCC "simple" #-} simpleSieve (H q p i E E) (p+step i) (nextIndex i))
        sh = buildH sml
        lh = H s lp w E E
        q  = p*p

-- The main sieve. Code almost identical to feederSieve, but we don't construct the Del,
-- which gains some performance.
{-# SPECIALISE primeSieve :: [Del Int] -> Hipp Int -> Hipp Int -> Int -> Int -> [Int] #-}
{-# SPECIALISE primeSieve :: [Del Word] -> Hipp Word -> Hipp Word -> Word -> Int -> [Word] #-}
{-# SPECIALISE primeSieve :: [Del Integer] -> Hipp Integer -> Hipp Integer -> Integer -> Int -> [Integer] #-}
primeSieve :: Integral a => [Del a] -> Hipp a -> Hipp a -> a -> Int -> [a]
primeSieve dls@((D s p u):ds) sh@(H sc _ _ _ _) lh@(H lc _ _ _ _) cd i
    | cd == sc  = primeSieve dls ({-# SCC "adjSmall" #-} adjust cd (inc sh)) ({-# SCC "adjLarge" #-}adjust cd lh) cd' j
    | cd == lc  = primeSieve dls sh (adjust cd (inc lh)) cd' j
    | cd == s   = primeSieve ds sh (push (s + p*step u) p (nextIndex u) lh) cd' j
    | otherwise = cd : primeSieve dls sh lh cd' j
      where
        !cd' = cd + step i
        !j   = nextIndex i
primeSieve _ _ _ _ _ = []   -- invariant violated

-- | A list of primes. The sieve does not handle overflow, hence for
--   bounded types, garbage occurs near @'maxBound'@. If primes that
--   large are requested, use
--
-- @
--   'map' 'fromInteger' $ 'takeWhile' (<= 'fromIntegral' 'maxBound') 'primes'
-- @
--
--   instead. Checking for overflow would be slower. The sieve is specialised
--   for @'Int'@, @'Word'@ and @'Integer'@, since these are the most commonly
--   used. For the fixed-width @Int@ or @Word@ types, sieving at one of the
--   specialised types and converting is faster.
--   To ensure sharing of the list of primes, bind it to a monomorphic variable,
--   to make sure that it is not shared, use @'sieveFrom'@ with different
--   arguments.
{-# SPECIALISE primes :: [Int] #-}
{-# SPECIALISE primes :: [Word] #-}
{-# SPECIALISE primes :: [Integer] #-}
primes :: Integral a => [a]
primes = 2:3:5:7:11:13:sieve 17 0

-- | @'sieveFrom' n@ generates the list of primes @>= n@.
--   The remarks about overflow and performance in the documentation
--   of @'primes'@ apply here too.
{-# SPECIALISE sieveFrom :: Int -> [Int] #-}
{-# SPECIALISE sieveFrom :: Word -> [Word] #-}
{-# SPECIALISE sieveFrom :: Integer -> [Integer] #-}
sieveFrom :: Integral a => a -> [a]
sieveFrom from
    | fromIntegral from < (32768 :: Integer)
        = dropWhile (< from) (foldr ((:) . fromIntegral) (sieve sp si) wheelPrimes)
    | otherwise
        = primeSieve dls sh lh start (nextIndex i0)
      where
        -- trick the compiler into not CAFing feeder 17 0
        sp  | odd from  = 17
            | otherwise = fromIntegral (remainders `unsafeAt` 0)
        si  | even from = 0
            | otherwise = (steps `unsafeAt` 0)-2
        (q, r)      = (from - 18) `quotRem` 30030
        i0          = findIx (fromIntegral r + 17)
        -- last number coprime to all wheel primes < from
        before      = 30030*q + fromIntegral (remainders `unsafeAt` i0)
        -- first candidate
        !start       = before + step i0
        (sml, lrg)  = splitAt SH_SIZE (feeder sp si)
        !sh          = foldl' pushD E [findMulIx p | D _ p _ <- sml]
        (lh, dls)   = {-# SCC "munch" #-} munch E lrg
        pushD h (c, p, i) = push c p i h
        findMulIx p = ((p*mp), p, (nextIndex ip))
          where
            fpq         = before `quot` p
            (qq, qr)    = (fpq-17) `quotRem` 30030
            !ip         = findIx (fromIntegral qr + 17)
            !mp         = 30030*qq + fromIntegral (remainders `unsafeAt` ip) + step ip
        munch !h dels@(D s p _ : ds)
            | before < s    = (h,dels)
            | otherwise     = munch h' ds
              where
                !(!c, pr, i)    = findMulIx p
                h'          = push c pr i h
        munch h [] = (h,[])

-- Build main sieve.
{-# SPECIALISE sieve :: Int -> Int -> [Int] #-}
{-# SPECIALISE sieve :: Word -> Int -> [Word] #-}
{-# SPECIALISE sieve :: Integer -> Int -> [Integer] #-}
sieve :: Integral a => a -> Int -> [a]
sieve p i = primeSieve lrg sh lh p i
      where
        (sml,D s lp j : lrg) = splitAt SH_SIZE (feeder p i)
        !sh = buildH sml
        lh = H s lp j E E

-- next step index, we have 5760 numbers coprime to all wheel
-- primes in [1 .. product wheelPrimes]
{-# INLINE nextIndex #-}
nextIndex :: Int -> Int
nextIndex 5759 = 0
nextIndex i = i+1

-- The six smallest primes, that makes the supporting arrays small enough
-- and avoids enough composites to get acceptable speed (for sufficiently
-- generous values of acceptable).
wheelPrimes :: [Int]
wheelPrimes = 2:3:5:7:11:13:[]

-- index of largest coprime <= r
findIx :: Int -> Int
findIx r
    | 30030 < r = 5759
    | r == m    = a
    | r < m     = down (a-1)
    | otherwise = up a
      where
        a = max 0 (min 5758 ((192*r) `quot` 1001 - 1))
        m = remainders `unsafeAt` a
        down k
            | k < 0                         = 0
            | r < (remainders `unsafeAt` k) = down (k-1)
            | otherwise                     = k
        up k
            | k+1 > 5759                        = 5759
            | r < (remainders `unsafeAt` (k+1)) = k
            | otherwise                         = up (k+1)

-- array of numbers coprime to all wheel primes in wheel range
remainders :: UArray Int Int
remainders = runSTUArray $ do
    sar <- newArray (0,30029) True :: ST s (STUArray s Int Bool)
    let n2 30030 = return ()
        n2 i = unsafeWrite sar i False >> n2 (i+2)
        n3 30033 = return ()
        n3 i = unsafeWrite sar i False >> n3 (i+6)
        n5 30035 = return ()
        n5 i = unsafeWrite sar i False
                >> unsafeWrite sar (i+20) False >> n5 (i+30)
        n7 30037 = return ()
        n7 i = unsafeWrite sar i False >> n7 (i+14)
        n11 30041 = return ()
        n11 i = unsafeWrite sar i False >> n11 (i+22)
        n13 30043 = return ()
        n13 i = unsafeWrite sar i False >> n13 (i+26)
    n2 0
    n3 3
    n5 5
    n7 7
    n11 11
    n13 13
    rar <- newArray_ (0,5759) :: ST s (STUArray s Int Int)
    let loop 30031 _ = unsafeWrite rar 5759 30031 >> return rar
        loop i !r = do
            c <- unsafeRead sar i
            if c
                then do
                    unsafeWrite rar r i
                    loop (i+2) (r+1)
                else loop (i+2) r
    loop 17 0

-- distance from one coprime remainder to the next
steps :: UArray Int Int
steps = runSTUArray $ do
    sar <- newArray_ (0,5759) :: ST s (STUArray s Int Int)
    let loop 5759 p = do
            unsafeWrite sar 5759 (30047-p)
            return sar
        loop i p = do
            let !j = i+1
                !n = remainders `unsafeAt` j
            unsafeWrite sar i (n-p)
            loop j n
    loop 0 17

