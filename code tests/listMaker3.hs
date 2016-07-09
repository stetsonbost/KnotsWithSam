
import Data.List
import qualified Data.Set as S


main = do print $ move1 3

--listMaker :: Int -> [a] -> Set.Set[a]
--listMaker n list = Set.fromList $ (iterate (listMakerHelper' list) list)!!(n-1)

--makes the first list of tuples
listMakerHelper :: (Ord a) => [a] -> [a] -> [[a]]
listMakerHelper list1 list2 = [[a]++[b] | a <- list1, b <- list2, a <= b]

--make every subsequent list
listMakerHelper' :: [a] -> [[a]] -> [[a]]
listMakerHelper' smallList bigList = [b:a | a <- bigList, b <- smallList]

--use listMakerHelper' to make the new list, except filtered
--listMakerHelper'' :: [a] -> [[a]] -> ([a] -> Bool) -> [[a]]
--listMakerHelper'' smallList bigList condition = filter condition $ listMakerHelper' bigList smallList


--this function produces a (completely unfiltered) list of all possible 
--(valid, invalid, and duplicate) combinations of n arrows
listMakerHelper''' :: Int -> [[[Int]]]
listMakerHelper''' n = (iterate (listMakerHelper' a) b)!!(n-2)
	where a = tupleGenerator n;
		  b = listMakerHelper a a


tupleGenerator :: Int -> [[Int]]
tupleGenerator n = [[a,b,c,d,e] | a <- [0,1,(-1)], b <- [1..n], c <- [1..(2*n)], d <- [1..n], e <- [1..(2*n)]]

--this nub is more efficient for very large lists
betterNub :: (Ord a) => [a] -> [a]
betterNub list = S.toList . S.fromList $ list

goodFilter :: [[Int]] -> Bool
goodFilter diagram = (diagramFilter diagram) && (arrowFilter diagram) && (arrowFilter' diagram)



--the sample diagram I'm gonna use for testing
sampleDiagram :: [[Int]]
sampleDiagram = [[0,1,1,1,1],[0,1,1,1,1],[0,1,1,1,1],[0,1,3,4,3]]



--this helper function basically returns a list that holds the components included
--in a particular diagram. To avoid duplication, we should only need to include
--diagrams which have no gaps (i.e. a 1st component, a 2nd, but then a 4th and no 3rd)
diagramKeeper :: [[Int]] -> [Int]
diagramKeeper diagram =transform . foldl' (\acc [_,x,_,y,_] -> x:y:acc) [] $ diagram
	where transform = S.toList . S.fromList . sort

--apply diagram filter first
diagramFilter :: [[Int]] -> Bool
diagramFilter diagram = noGaps $ diagramKeeper diagram


--keeps a list where the (n-1)th element contains all of the arrow head/tails
--on the nth diagram

--uses the oddly-named getRekt twins to create a list where the (n-1)th interior
--list contains all of the heads and tails that reside on the nth component
arrowKeeper :: [[Int]] -> [[Int]]
arrowKeeper diagram = foldl getRekt2 foldee diagram
	where foldee = foldl' getRekt1 (templateList diagram) diagram

--like arrowKeeper, except we sort the entrees and remove the -1
arrowKeeper' :: [[Int]] -> [[Int]]
arrowKeeper' diagram = map tail mapee
	where mapee = map sort (arrowKeeper diagram)

--filters out diagrams which have overlapping arrowhead/tails
--apply this second
arrowFilter :: [[Int]] -> Bool
arrowFilter diagram = arrowKeeper' diagram == map (\x -> nub x) (arrowKeeper' diagram)

--unlike arrowFilter, arrowFilter' filters out diagrams with holes
--apply this third
arrowFilter' :: [[Int]] -> Bool
arrowFilter' diagram = foldl' (\acc x -> if not (noGaps x) then False else acc) True stuff
	where stuff = arrowKeeper' diagram

--these next two are silly helper functions
getRekt1 :: [[Int]] -> [Int] -> [[Int]]
getRekt1 acc [_,x,y,_,_] = first ++ second
	where (preFirst, second) = splitAt x acc
	      changed = y:(last preFirst)
	      first = (init preFirst) ++ [changed]

getRekt2 :: [[Int]] -> [Int] -> [[Int]]
getRekt2 acc [_,_,_,x,y] = first ++ second
	where (preFirst, second) = splitAt x acc
	      changed = y:(last preFirst)
	      first = (init preFirst) ++ [changed]

--creates a list of n tuples that we can "modify" to get a list of the head/tail
--numbers for each diagram
templateList :: [[Int]] -> [[Int]]
templateList diagram = foldl (\acc x -> [-1]:acc) [] diagram


--helper function that checks for gaps in a list
noGaps :: [Int] -> Bool
noGaps list = list == [1..length(list)]

--produces a list where all forbidden diagrams are filtered out. now, the only thing
--left to do is create a custom filter for each move. We'll start with 1
almostThere :: Int -> [[[Int]]]
almostThere n = filter arrowFilter' thing2
	where thing1 = filter diagramFilter $ listMakerHelper''' n
	      thing2 = filter arrowFilter thing1

--i think this works (at least for n=2, but it should work generally)
move1 :: Int -> [[[Int]]]
move1 n = betterNub $ foldl' (\acc x -> if [1,1,1,1,2] `elem` x then x:acc else acc) [] (almostThere n)



