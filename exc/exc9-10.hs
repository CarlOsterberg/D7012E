--9.2
len::[a] -> Int
len a = sum (map (\x -> 1) a)

--9.6
squareList::[Int] -> [Int]
squareList ns = map (\x->x^2) ns

sumSquareList::[Int] -> Int
sumSquareList ns = foldr (+) 0 (squareList ns)

checkNonZero::[Int] -> Bool
checkNonZero [x] = x > 0
checkNonZero ns = head ns > 0 && checkNonZero (tail ns)

--9.9
iter::Int -> (b -> b) -> b -> b
iter 0 f x = x
iter i f x = iter (i-1) f (f x)

addOne:: Int -> Int
addOne x = x + 1

addDot:: String -> String 
addDot s = s ++ "."

--9.10
double:: Int -> Int
double n = n * 2

--9.11
sumSqNat::Int -> Int
sumSqNat n = foldr (+) 0 (map (\x-> x^2) [x | x <- [1..n]])

--9.16
filterFirst::(a -> Bool) -> [a] -> [a]
filterFirst p [] = []
filterFirst p l = 
    if p (head l) 
        then [head l] ++ filterFirst p (tail l)
    else tail l 

isDiv2:: Int -> Bool 
isDiv2 n = n `mod` 2 == 0

--9.17
revList::[a]->[a]
revList [] = []
revList a = revList (tail a) ++ [head a]

filterLast::(a -> Bool) -> [a] -> [a]
filterLast p l = revList (filterFirst p (revList l))