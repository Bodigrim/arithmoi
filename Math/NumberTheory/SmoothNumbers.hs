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
  ( SmoothBasis
  , smoothOver
  , smoothOverInRange
  , smoothOverInRangeBF
  , fromSet
  , fromList
  , fromSmoothUpperBound
  ) where

import Data.List (sort, nub)
import qualified Data.Set as S
import Math.NumberTheory.Primes.Sieve (primes)

newtype SmoothBasis a = SmoothBasis [a] deriving Show

-- Returns a SmoothBasis from a Set if it is valid.
fromSet :: Integral a => S.Set a -> Maybe (SmoothBasis a)
fromSet s = if isValid l then Just (SmoothBasis l) else Nothing where l = S.elems s

-- Returns a SmoothBasis from a list if it is valid.
fromList :: Integral a => [a] -> Maybe (SmoothBasis a)
fromList l = if isValid l' then Just (SmoothBasis l') else Nothing where l' = nub $ sort l

-- | Return the primes <= the upper bound param.
fromSmoothUpperBound :: Integral a => a -> Maybe (SmoothBasis a)
fromSmoothUpperBound n = if (n < 2)
                         then Nothing
                         else Just $ SmoothBasis $ map fromInteger $ takeWhile (<= nI) primes
                         where nI = toInteger n

-- | Return the list from a SmoothBasis.
fromSmoothBasis :: Integral a => SmoothBasis a -> [a]
fromSmoothBasis (SmoothBasis l) = l

-- | smoothOver is the workhorse for this module.  
smoothOver :: Integral a => SmoothBasis a -> [a]
smoothOver pl = foldr (\p l -> mergeListLists $ iterate (map (p*)) l) [1] (fromSmoothBasis pl)
                where
                      {-# INLINE mergeListLists #-}
                      mergeListLists      = foldr go1 []
                        where go1 (h:t) b = h:(go2 t b)
                              go1 _     b = b
   
                              go2 a@(ah:at) b@(bh:bt)
                                | bh < ah   = bh : (go2 a bt)
                                | otherwise = ah : (go2 at b) -- no possibility of duplicates
                              go2 a b = if null a then b else a

smoothOverInRange   :: Integral a => SmoothBasis a -> a -> a -> [a]
smoothOverInRange s lo hi = takeWhile (<= hi) $ dropWhile (< lo) (smoothOver s)

-- | BF stands for Brute Force.  This function will manually factor over the input list.
smoothOverInRangeBF :: Integral a => SmoothBasis a -> a -> a -> [a]
smoothOverInRangeBF prs lo hi = filter (mf prs') [lo..hi]
                                where mf []     n    = (n == 1) -- mf means manually factor
                                      mf pl@(p:ps) n = if (mod n p == 0) 
                                                       then mf pl (div n p)
                                                       else mf ps n
                                      prs'           = fromSmoothBasis prs

-- isValid assumes that the list is sorted and unique and then checks if the list is suitable to be a SmoothBasis. 
isValid :: (Integral a) => [a] -> Bool
isValid pl = if (length pl == 0) then False else v' pl
             where v' []        = True
                   v' (x:xs)    = if (x < 2 || (not $ rpl x xs)) then False else v' xs
                   rpl _ []     = True  -- rpl means relatively prime to the rest of the list
                   rpl n (x:xs) = if (gcd n x > 1) then False else rpl n xs

