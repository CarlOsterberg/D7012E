import Data.List

perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs = [ x:ps | x <- xs , ps <- perms (xs \\ [x]) ]

-- the infinite list of ones
ones = 1 : ones

nat :: [Int] -- all natural numbers
nat = 0 : [ x+1 | x <- nat ]

int :: [Int] -- all integers, listed from 0
int = 0 : (concat.tail) [ [x,-x] | x <- nat ]

powers :: Int -> [Int]
powers n = [ n^k | k <- [0,1 .. ]]

primes :: [Int]
primes  =  sieve [2 .. ]
  where
     sieve (x:xs)  =  
        x : sieve [ y | y <- xs , y `mod` x > 0]


rnd :: Int -> [Int]
rnd seed = next : rnd next
  where
    next = (multiplier * seed + increment) `mod` modulus
    modulus = 31
    multiplier = 17
    increment = 23

fibs = 0:1:zipWith (+) fibs (tail fibs)

pascal :: [[Int]]
pascal = [1] : map (\xs -> zipWith (+) ([0] ++ xs) (xs ++ [0])) pascal

cyclic = let x = 0 : y
             y = 1 : x
         in  x

nat1 = 1 : map (+1) nat1

-- iterate f x = x : iterate f (f x)
nat2 = iterate (+1) 0

-- generate "matrices" with r rows and c columns
mat r c = map (take c) $ take r [ nat1 | x <- nat1 ]

merge' [] ys = ys
merge' xs [] = xs
merge' (x:a) (y:b) | x<y   = x : merge' a (y:b)
                   | x==y  = y : merge' a b 
                   | x>y   = y : merge' (x:a) b

-- all numbers whose prime factors are only (1,) 2, 3, and 5

twos = 1:map (*2) [1..]
threes = 1:map (*3) [1..]
fives = 1:map (*5) [1..]

ham = merge' twos (merge' threes fives)

-- all with their prime factors in xs (using nat1 == [1..])
fg :: [Int] -> [Int]
fg xs = foldr merge' [] (map (\i->map (*i) nat1) xs)

allFreq :: [Int] -> [(Int,Int)]
allFreq [] = []
allFreq (x:xs) = freq x (allFreq xs) 

freq :: Int -> [(Int,Int)] -> [(Int,Int)] 
freq v [] = [(v,1)]
freq v ((w,f):xs)
  | v == w = (w,f+1) : xs
  | otherwise = (w,f) : freq v xs


