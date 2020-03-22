import System.IO
import Data.Ratio


mylog :: Integer -> Integer -> Integer
mylog a b 
	| ((a `mod` b) /= 0)  = 0
	| otherwise = (1 + mylog (a `div` b) b)


getfactors :: Integer -> Integer -> [(Integer,Integer)]
getfactors 1 _ = []
getfactors n st
		| lst == []  = [(n,1)]
		| otherwise	= (((head lst), (mylog n (head lst))):getfactors ( n `div` ((head lst) ^ (mylog n (head lst) ))) ((head lst)+1))
			where lst = (take 1 $ filter (\y -> (n `mod` y) == 0) [st .. (floor (sqrt (fromIntegral n)))])
				

precalc :: Integer -> [(Integer,Integer)] -> [Integer]
precalc n [] = []
precalc n (x:[]) = [n `div` (fst x ^ snd x )] 
precalc n (x:xs) = (n `div` (fst x ^ snd x )): (precalc (n `div` ((fst x) ^ (snd x) )) xs)


func :: Integer -> Integer -> [(Integer,Integer)] -> Int -> Integer -> Integer -> [Integer] -> [Integer]
func i temp fact ind a b arr 
	| (ind ==  (length fact)) = if (temp <= b && temp >= a) then [temp] else []
	| ( (i <= (snd (fact!!ind))) && temp > b) = []
	| ( (i <= (snd (fact!!ind))) && ((temp*(arr!!ind)) <= a)) = ((func (i+1) (temp * (fst (fact!!ind))) fact ind a b arr)  )
	| (i <= (snd (fact!!ind))) = ( (func 0 (temp) fact (ind+1) a b arr) ++ (func (i+1) (temp * (fst (fact!!ind))) fact ind a b arr)  ) 
	| otherwise = []

-- divisorsInRange :: (Ord a, UniqueFactorisation a) => a -> a -> a -> [a]
divisorsInRange :: Integer -> Integer -> Integer -> [Integer]
divisorsInRange from to n = func 0 1 (getfactors n 2) 0 from to (precalc n (getfactors n 2))
	
-- if sorted list required
quicksort :: [Integer] -> [Integer]
quicksort (x:xs) = lft ++ [x] ++ rgt
	where 
		lft = quicksort [a | a <- xs, a <= x]
		rgt = quicksort [a | a <- xs, a > x]
quicksort [] = []



