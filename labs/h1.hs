--Carl Österberg

revList::[t]->[t]
revList [] = []
revList a = revList (tail a) ++ [head a]

rmLast::[t]->[t]
--rmLast = revList.tail.revList
rmLast l = init l

--Gets all the sublists with the head at the head
getAll::[t] -> [[t]]
getAll [x] = [[x]]
getAll l = [l] ++ getAll (rmLast l)

--Gets all the sublists
getAllSubsets::[t] -> [[t]]
getAllSubsets [x] = [[x]]
getAllSubsets l = getAll l ++ getAllSubsets (tail l)

getAllSums::[Int] -> [Int]
getAllSums l = map sum (getAllSubsets l)

quicksort::[([(Int,Int)],Int)] -> [([(Int,Int)],Int)]
quicksort [] = []
quicksort (h:t) = 
    quicksort [x|x<-t, snd x<snd h] ++ [h] ++ quicksort [x|x<-t, snd x>= snd h]

withIndex::[t] -> Int -> [(t,Int)]
withIndex [x] i = [(x,i)]
withIndex n i = (head n, i) : withIndex (tail n) (i+1)

--Format [([(Värde,Index)],Summa)]
sortedFormatting::[Int] -> [([(Int,Int)],Int)]
sortedFormatting [] = error "Empty list"
sortedFormatting n = quicksort (zip (getAllSubsets (withIndex n 0)) (getAllSums n))

cutLastNTimes:: [t] -> Int -> [t]
cutLastNTimes [] i = []
cutLastNTimes l 0 = l
cutLastNTimes l i = cutLastNTimes (rmLast l) (i-1)

getFirstK:: [t] -> Int -> [t]
getFirstK l i = cutLastNTimes l ((length l) - i)

smallestKsets::[Int] ->  Int ->  String
smallestKsets n k= makeHead ++ formatString (getFirstK x k)
    where x = sortedFormatting n

formatString::[([(Int,Int)],Int)] -> String 
formatString [] = []
formatString n = makeString (head n) ++ formatString (tail n)

makeHead:: String
makeHead = "size\ti\tj\tsublist\n"

makeString::([(Int,Int)],Int) -> String
makeString n = show (snd n) ++ "\t" ++ show(snd ((fst n)!!0)) ++ "\t" 
    ++ show(snd ((fst n)!!(length (fst n)-1))) ++ "\t" ++ show (map fst (fst n)) ++ "\n"

testCase1::String 
testCase1 = smallestKsets [x*(-1)^x | x <- [1..100]] 15

testCase2::String 
testCase2 = smallestKsets [24,-11,-34,42,-24,7,-19,21] 6

testCase3::String 
testCase3 = smallestKsets [3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3] 8
