-- |
-- Module:      Math.NumberTheory.SmoothNumbers
-- Copyright:   (c) 2018 Frederick Schneider
-- Licence:     MIT
-- Maintainer:  Frederick Schneider <frederick.schneider2011@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Functions for generating smooth numbers
--

module Math.NumberTheory.SmoothNumbers
  ( smooth
  , smoothOverSet
  , smoothOverList
  , smoothInRange
  , smoothOverSetInRange
  , smoothOverListInRange 
  , smoothInRangeBF
  , smoothOverSetInRangeBF
  , smoothOverListInRangeBF
  ) where

import Data.List (sort, nub)
import qualified Data.Set as S
import Math.NumberTheory.Primes.Sieve (primes)


-- | Generates numbers whose greatest prime factor <= n
smooth :: (Integral a) => a -> [a]
smooth n = smoothU (getPrimes' n) where nI = toInteger n


-- | These Set and List functions generate numbers which are products of powers of the input
--   Note: composite numbers are allowed but the inputs are required to be pairwise relatively prime 
--   in order to get sensible results.
smoothOverSet :: (Integral a) => S.Set a -> [a]
smoothOverSet s = if (validU l) then smoothU l else error invalidInputMsg where l = S.elems s

smoothOverList :: (Integral a) => [a] -> [a]
smoothOverList l = if (validU l') then smoothU l' else error invalidInputMsg where l' = nub $ sort l


-- | These 3 smooth range functions are for limiting the results from their resp. underlying functions to a range
--   At this point, they are just for convenience.  There is no optimization going on based on the range.
smoothInRange :: (Integral a) => a -> a -> a -> [a] 
smoothInRange n lo hi = range' (smooth n) lo hi

smoothOverSetInRange :: (Integral a) => S.Set a -> a -> a -> [a]
smoothOverSetInRange s lo hi = range' (smoothOverSet s) lo hi

smoothOverListInRange :: (Integral a) => [a] -> a -> a -> [a]
smoothOverListInRange l lo hi = range' (smoothOverList l) lo hi

-- | Inclusive range: >= lo and <= hi
range' :: (Integral a) => [a] -> a -> a -> [a]
range' res lo hi = takeWhile (<= hi) $ dropWhile(< lo) res

-- | BF stands for Brute Force.  These three functions will manually factor over the input list. 
--   It might be wiser to use one of these functions for a large but narrow range
smoothInRangeBF :: (Integral a) => a -> a -> a -> [a] 
smoothInRangeBF n lo hi = smoothBF' (getPrimes' n) lo hi

smoothOverSetInRangeBF :: (Integral a) => S.Set a -> a -> a -> [a]
smoothOverSetInRangeBF s lo hi = if (validU l) then (smoothBF' l lo hi) else error invalidInputMsg where l = S.elems s

smoothOverListInRangeBF :: (Integral a) => [a] -> a -> a -> [a]
smoothOverListInRangeBF l lo hi = if (validU l') then (smoothBF' l' lo hi) else error invalidInputMsg where l' = nub $ sort l

smoothBF' :: (Integral a) => [a] -> a -> a -> [a]
smoothBF' prs lo hi = filter (mf prs) [lo..hi]
                      where mf []     n    = (n == 1) -- mf means manually factor
                            mf pl@(p:ps) n = if (mod n p == 0) 
                                             then mf pl (div n p)
                                             else mf ps n


-- | Returns a list of primes <= the input param
getPrimes' :: (Integral a) => a -> [a] 
getPrimes' n = if (n < 2) 
               then error "The input to getPrimes' must be >= 2"
               else (map fromInteger $ takeWhile (<= nI) primes)
               where nI = toInteger n  


-- | validU assumes that the list is sorted and unique and then checks if the list is further suitable for smoothU (see below). 
--   U is for unsafe.  It should not be called directly.
validU :: (Integral a) => [a] -> Bool
validU pl = if (length pl == 0) then False else v' pl
            where v' []        = True
                  v' (x:xs)    = if (x < 2 || (not $ rpl x xs)) then False else v' xs
                  rpl _ []     = True  -- rpl means relatively prime to the rest of the list
                  rpl n (x:xs) = if (gcd n x > 1) then False else rpl x xs

invalidInputMsg = "The list param must be non-empty, pairwise relatively prime and have all elements > 1"


-- | smoothU (U is for unsafe) is the workhorse for this module.  It assumes that the input it has effectively passed validation.  So, it ..
--   1) is a non-empty list in ascending order without duplicates
--   2) is pairwise relatively prime
--   3) has no members less than 2
smoothU :: (Integral a) => [a] -> [a]
smoothU pl = foldr (\p l -> mergeListLists $ iterate (map (p*)) l) [1] pl
             where 
                   {-# INLINE mergeListLists #-}
                   mergeListLists      = foldr go1 []
                     where go1 (h:t) b = h:(go2 t b)
                           go1 _     b = b

                           go2 a@(ah:at) b@(bh:bt)
                             | bh < ah   = bh : (go2 a bt)
                             | otherwise = ah : (go2 at b) -- no possibility of duplicates
                           go2 a b = if null a then b else a

