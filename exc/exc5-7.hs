
--5.10
divisors::Int -> [Int]
divisors n = [y | y <- [1..n], n `mod` y == 0]

isPrime::Int -> Bool
isPrime n = length (divisors n) == 2

--5.11
matches::Int -> [Int] -> [Int]
matches a b = [n | n <- b, n == a]

eleme::Int -> [Int] -> Bool
eleme a b = not (null (matches a b))

--5.22
onSeperateLines:: [String] -> String
onSeperateLines n = case n of
    [] -> []
    _ -> head n ++ "\n" ++ onSeperateLines (tail n)

--5.23
duplicate::String -> Int -> String
duplicate s i
    | i <= 0 = ""
    | otherwise = s ++ duplicate s (i - 1)

--5.24
pushRight:: String -> String
pushRight s =
    if length s < 12
        then pushRight (" "++s)
    else s

--6.29
tBs:: [Int] -> [String]
tBs bar
    | null bar = []
    | otherwise = tB (head bar) : tBs (tail bar)

tB:: Int -> String
tB i
    | i == 1234 = "Dry Sherry, 1lt"
    | i == 4719 = "Fish Fingers"
    | i == 3814 = "Orange Jelly"
    | i == 1112 = "Hula Hoops (Giant)"
    | otherwise = "Unknown Item"

tPs::[Int] -> [String]
tPs bar
    | null bar = []
    | otherwise = show (tP (head bar)) : tPs (tail bar)

tP::Int -> Float
tP i
    | i == 1234 = 5.40
    | i == 4719 = 1.21
    | i == 3814 = 0.56
    | i == 1112 = 1.33
    | otherwise = 0.00

withDots:: [(String,String)] -> [String]
withDots s
    | null s = []
    | otherwise = [formatDots (head s)] ++ withDots (tail s)

formatDots::(String,String) -> String
formatDots s
    | length (fst s ++ snd s) < 30 = formatDots (fst s ++ ".", snd s)
    | otherwise = fst s ++ snd s

formatHead:: String
formatHead = "        Haskell Stores\n\n"

formatPrice:: [Int] -> String
formatPrice price = formatDots ("Total",show (makePrice price))

makePrice::[Int] -> Float
makePrice bar
    | null bar = 0.00
    | otherwise = tP (head bar) + makePrice (tail bar)

formatBill:: [Int]-> String
formatBill bar = formatHead ++ onSeperateLines (withDots (zip (tBs bar) (tPs bar))) ++ "\n" ++ formatPrice bar

--7.7
unique:: [Int] -> [Int]
unique l = [n | n <- l, length (matches n l) == 1]

--7.25
isPalin :: String -> Bool
isPalin s 
    | length s == 1 = True
    | null s = True 
    | otherwise = (head s == getLast s) && isPalin (init (tail s))

getLast:: String -> Char
getLast s
    | length (tail s) == 1 = head (tail s)
    | otherwise = getLast (tail s)

insert:: Int -> [Int] -> [Int]
insert x [] = [x]
insert x n
    | x < head n = x : n
    | otherwise = [head n] ++ insert x (tail n)