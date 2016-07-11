module ListMaker
(move1
, almostThere
) where

import Control.Monad
import Data.List
import qualified Data.Set as S

tupleGenerator :: Int -> [[Int]]
tupleGenerator n = [[a,b,c,d,e] | a <- [0,1,(-1)], b <- [1..n], c <- [1..(2*n)], d <- [1..n], e <- [1..(2*n)]]

listMaker :: Int -> [[[Int]]]

listMaker 1 = [[[1,1,1,1,2]]]

listMaker 2 = do
	a <- [[1,1,1,1,2]]
	b <- tupleGenerator 2
	return [a,b]

listMaker 3 = do
	a <- [[1,1,1,1,2]]
	b <- tupleGenerator 3
	c <- tupleGenerator 3
	guard (b <= c)
	return [a,b,c]

listMaker 4 = do
	a <- [[1,1,1,1,2]]
	b <- tupleGenerator 4
	c <- tupleGenerator 4
	d <- tupleGenerator 4
	guard (b <= c)
	guard (c <= d)
	return [a,b,c,d]




betterNub :: (Ord a) => [a] -> [a]
betterNub list = S.toList . S.fromList $ list

diagramKeeper :: [[Int]] -> [Int]
diagramKeeper diagram =transform . foldl' (\acc [_,x,_,y,_] -> x:y:acc) [] $ diagram
    where transform = sort . betterNub

diagramFilter :: [[Int]] -> Bool
diagramFilter diagram = noGaps $ diagramKeeper diagram

noGaps :: [Int] -> Bool
noGaps list = list == [1..length(list)]

arrowKeeper :: [[Int]] -> [[Int]]
arrowKeeper diagram = foldl getRekt2 foldee diagram
	where foldee = foldl getRekt1 (templateList diagram) diagram

arrowKeeper' :: [[Int]] -> [[Int]]
arrowKeeper' diagram = map tail mapee
	where mapee = map sort (arrowKeeper diagram)

arrowFilter :: [[Int]] -> Bool
arrowFilter diagram = arrowKeeper' diagram == map (\x -> betterNub x) (arrowKeeper' diagram)

arrowFilter' :: [[Int]] -> Bool
arrowFilter' diagram = foldl (\acc x -> if not (noGaps x) then False else acc) True stuff
	where stuff = arrowKeeper' diagram

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

templateList :: [[Int]] -> [[Int]]
templateList diagram = foldl (\acc x -> [-1]:acc) [] diagram

almostThere :: Int -> [[[Int]]]
almostThere n = filter arrowFilter' thing2
	where thing1 = filter diagramFilter $ listMaker n
	      thing2 = filter arrowFilter thing1

move1 :: Int -> [[[Int]]]
move1 n = betterNub $ foldl (\acc x -> if [1,1,1,1,2] `elem` x then x:acc else acc) [] (almostThere n)


