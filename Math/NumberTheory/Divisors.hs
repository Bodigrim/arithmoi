import System.IO
import Data.Ratio
-- import Math.NumberTheory.Primes.factorise
import Data.List




headInt :: [Integer] -> Integer
headInt (x:xs) =x


-- mylog recTakeEachPrimetion here take two numbers a and b and gives the maximum 
-- power of b that appears in a
mylog :: Integer -> Integer -> Integer
mylog a b 
	| ((a `mod` b) /= 0)  = 0
	| otherwise = (1 + mylog (a `div` b) b)




-- getfactors takes two arguments the number 'n' that you want to factorise 
-- and a number 'st' that should be kept to '2' while calling the recTakeEachPrimetion so 
-- that prime factorization starts from 2 and goes upto sqrt(n) 



getfactors :: Integer -> Integer -> [(Integer,Integer)]
getfactors 1 _ = []
getfactors n st
		| lst == [] = [(n,1)]
		| otherwise	= 
			(((headInt lst), (mylog n (headInt lst))):getfactors ( n `div` ((headInt lst) ^ (mylog n (headInt lst) ))) ((headInt lst)+1))
			where lst = (take 1 $ filter (\y -> (n `mod` y) == 0) [st .. (floor (sqrt (fromIntegral n)))])
				

-- precalc if use for some precalculations that are needed to be done 
-- to find factors of a number n. you need to pass n and the list of primefactors of n with 
-- its count and this will produce a list of same length as the list of prime factors which contains the 
-- residue of n after taking out all the primefactors of it which appear before the current index
-- For example:
-- 	precalc 150 [(2 1), (3 1), (5 2)] = [150/2 , 150/2/3 , 150/2/3/25]= [75 , 25 , 1]

precalc :: Integer -> [(Integer,Integer)] -> [Integer]
precalc n [] = []
precalc n ((x,y):[]) = [n `div` (x ^ y )] 
precalc n ((x,y):xs) = (n `div` (x ^ y )): (precalc (n `div` (x ^ y)) xs)



-- recTakeEachPrime function takes index i denoting count of prime present at index 'ind' in 
-- the prime list that are taken and multiplied to temp at the current pint 
-- in recursion tree. 

-- Initially i and ind need to be set to 0 

-- temp denotes the value of the number formed in the function by now taking all combinations 
-- of primes till index 'idx'.

-- fact is the list of prime factors with its count and a and b are such 
-- that [a,b] represents range in which factors are needed.

-- arr is the list returned bt precalc function and is used at each point to check if going 
-- down in that branch of the tree will fetch me some factor greater that a or not.
-- Basically it checks that by multiplying temp to arr[ind] and checking that even after multiplying 
-- the highest powers of the remaining primes with temp can we get a number greater than a or not. 


-- For example take n = 15 = 2* 3 *5^2    a= 26 , b= 100


--                            1



--                      /           \
--                     1             2


-- 				        \         /      \
-- 				         3        2       6
            
            
--     		  	       / |  \     /  | \    / |  \  
--    			 	        75         50     30  
 

--  30 50 and 75 will be the required factors 
-- So the deapth of the recursion tree will be equal to number of distinct prime factors and the
-- width would be the number of factors needed.


recTakeEachPrime :: Integer -> Integer -> [(Integer,Integer)] -> Integer -> Integer -> [Integer] -> [Integer]
recTakeEachPrime i temp [] a b [] 
	= if (temp <= b && temp >= a) then [temp] else []
recTakeEachPrime i temp ((x,x'):xs) a b (y:ys) 

	| i <= (x') 
	, temp > b 
	= []

	| i <= (x')
	, (temp*(y)) <= a
	= ((recTakeEachPrime (i+1) (temp * (x)) ((x,x'):xs) a b (y:ys)) )

	| i <= (x') 
	= ( (recTakeEachPrime 0 (temp) xs a b ys) ++ (recTakeEachPrime (i+1) (temp * (x)) ((x,x'):xs) a b (y:ys))  ) 

	| otherwise 
	= []


-- divisorsInRange takes three argument from to and n .
-- The first two takes the range [from , to] of the factors needed 
-- and n denotes the number whose factore we need

-- divisorsInRange :: (Ord a, UniqueFactorisation a) => a -> a -> a -> [a]
divisorsInRange :: Integer -> Integer -> Integer -> [Integer]
divisorsInRange from to n = 
	recTakeEachPrime 0 1 (getfactors n 2) from to (precalc n (getfactors n 2))
	


