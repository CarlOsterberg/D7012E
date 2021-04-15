square :: Int -> Int
square n = n * n

double :: Int -> Int
double n = 2*n

ds :: Int -> Int
ds = double . square

--3.7
threeDiff :: Int -> Int -> Int  -> Bool
threeDiff a b c = a /= b && b /= c && a /= c

--3.8
fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual a b c d = a == b && b == c && c == d

--3.15
numberNDroots :: Float -> Float -> Float -> Int
numberNDroots a b c
    | b * b > rs a c = 2
    | b * b == rs a c = 1
    | b * b < rs a c = 0

rs :: Float -> Float -> Float
rs a c = 4.0 * a * c

--3.16
numberRoots :: Float -> Float -> Float -> Int
numberRoots a b c
    | a /= 0.0 = numberNDroots a b c
    | b /= 0.0 = 1
    | b == 0.0 && c /= 0.0 = 0
    | b == 0.0 && c == 0.0 = 3

--3.17
smallerRoot, largerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c =
    if numberRoots a b c == 1 || numberRoots a b c == 2
    then (-b - sqrt(b^2 - 4*a*c))/(2*a)
    else 0.0
largerRoot a b c =
    if numberRoots a b c == 1 || numberRoots a b c == 2
    then (-b + sqrt(b^2 - 4*a*c))/(2*a)
    else 0.0

--4.7
mult :: Int -> Int -> Int
mult a b =
    if a == 0 then 0
    else b + mult (a-1) b

--4.8
intSqrt :: Int -> Int -> Int
intSqrt a b
    | b^2 == a = b
    | b^2 > a = b-1
    | b^2 < a = intSqrt a (b+1)

--4.9
maxF :: Int -> Int
maxF n =
    if n == 0 then f 0
    else max (maxF (n-1)) (f n)

f 0 = 1
f 1 = 33
f 2 = 12
f 3 = 3
f 4 = 19
f 5 = 110
f 6 = 11
f 7 = 9
f 8 = 88
f 9 = 51
f _ = 0

--4.14
power :: Int -> Int
power n
    | n == 0 = 1
    | even n = power (div n 2)^2
    | odd n = power (div (n-1) 2)^2*2