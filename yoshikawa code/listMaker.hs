
import Data.List
import qualified Data.Set as Set

--listMaker :: Int -> [a] -> Set.Set[a]
--listMaker n list = Set.fromList $ (iterate (listMakerHelper' list) list)!!(n-1)


listMakerHelper :: (Ord a) => [a] -> [a] -> [[a]]
listMakerHelper list1 list2 = [sort([a]++[b]) | a <- list1, b <- list2]

listMakerHelper' :: [[a]] -> [a] -> [[a]]
listMakerHelper' bigList smallList = [a ++ [b] | a <- bigList, b <- smallList]

tupleGenerator :: Int -> [[Int]]
tupleGenerator n = [[a,b,c,d,e] | a <- [0,1,(-1)], b <- [1..n], c <- [1..(2*n)], d <- [1..n], e <- [1..(2*n)]]


--first, generate the tuple using tupleGenerator n (we've been testing with n=2), call it 'a'

--second, use listMakerHelper to get a list, call it 'b'

--third, use listMakerHelper' with arguments 'b' and 'a' (in that order), call it 'c'

--now, we can call listMakerHelper' with 'c' and 'a' to add another tuple on.
-- this is the step we should automate using 'iterate',
-- since this step keep being done using listMakerHelper' with a,
-- with the other variable ('c' in the first instance)
--getting updated.



--next order of business is filtering each time we use listMakerHelper' so that
--our lists don't get unwieldly long, and because we'll need to filter at
--some point anyways.



--under construction
move_1 :: Int -> [[Int]]
move_1 n = 






















