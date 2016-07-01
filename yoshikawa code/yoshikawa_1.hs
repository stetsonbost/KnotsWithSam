--Haskell Time!
--This will work for n=2, then we should expand functionality 
import Data.List
import qualified Data.Set as Set

--firstList generates all of the possible arrows 
firstList :: Int -> [[Int]]
firstList n = [[a,b,c,d,e] | a <- [0,1,(-1)], b <- [1..n], c <- [1..(2*n)], d <- [1..n], e <- [1..(2*n)]]



--secondList makes all possible combinations of the first yoshikawa move and a second
--arrow. There are still many forbidden combinations which we must cull

--presently this only works for n=2
secondList :: Int -> [[[Int]]]
secondList n = delete a (foldr (\x acc -> [[1,1,1,1,2],x]:acc) [] (firstList n))
	where a = [[1,1,1,1,2],[1,1,1,1,2]]


--thirdList prevents arrowheads from starting/stopping at the same spot
thirdList n = [a | a <- (secondList n), p a, q a, r a, s a, t a, u a]
				where p = \a -> not ((a!!0!!2 == a!!0!!4) && (a!!0!!1 == a!!0!!3));
					  q = \a -> not ((a!!1!!2 == a!!1!!4) && (a!!1!!1 == a!!1!!3));
					  r = \a -> not ((a!!0!!2 == a!!1!!2) && (a!!0!!1 == a!!1!!1));
					  s = \a -> not ((a!!0!!2 == a!!1!!4) && (a!!0!!1 == a!!1!!3));
					  t = \a -> not ((a!!0!!4 == a!!1!!2) && (a!!0!!3 == a!!1!!1));
					  u = \a -> not ((a!!0!!4 == a!!1!!4) && (a!!0!!3 == a!!1!!3));

--floog, gloof, bigFloog and bigGloof are helper functions for pairs. they have silly names
--because their names don't matter
floog :: [Int] -> [Int]
floog [a,b,c,d,e]
	| (b==1) && (d==1) = [c,e]
	| b==1 = [c]
	| d==1 = [e]
	| otherwise = []

gloof :: [Int] -> [Int]
gloof [a,b,c,d,e]
	| (b==2) && (d==2) = [c,e]
	| b==2 = [c]
	| d==2 = [e]
	| otherwise = []

bigFloog :: [[Int]] -> [Int]
bigFloog [] = []
bigFloog (x:xs) = (floog x) ++ bigFloog xs 

bigGloof :: [[Int]] -> [Int]
bigGloof[] = []
bigGloof (x:xs) = (gloof x) ++ bigGloof xs 

--looks at the numbering of arrowheads/tails on a given component to ensure
--that there are no holes 

--a represents arrowhead/tail values on the first component
--b represents arrowhead/tail values on the second component
pairs :: [[Int]] -> ([Int],[Int])
pairs xs = (a,b)
	where a = sort $ bigFloog xs;
		  b = sort $ bigGloof xs;

--check to see that there are no holes. will be used to
--create fourthList by filtering thirdList
acceptablePair :: [[Int]] -> Bool
acceptablePair xs = (a == c ) && (b == d)
	where (a,b) = pairs xs;
		   c    = if null a then [] else [1..(maximum a)];
		   d    = if null b then [] else [1..(maximum b)]


fourthList n = [a | a <- (thirdList n), acceptablePair a]
--this gives us what we want. there are duplicates, of a sort, 
--but when we break these down using inner product, we can weed
--the duplicates out. otherwise we should be good.



















