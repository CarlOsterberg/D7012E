--12.2
numEqual:: Eq a => [a] -> a -> Int
numEqual [] x = 0
numEqual xs x 
    | head xs == x = 1 + numEqual (tail xs) x
    | otherwise = numEqual (tail xs) x

member:: Eq a => [a] -> a -> Bool 
member xs x = numEqual xs x > 0

--12.3
oneLookupFirst::Eq a => [(a,b)] -> a -> b
oneLookupFirst [] z = error "No such element"
oneLookupFirst xs x 
    | fst (head xs) == x = snd (head xs) 
    | otherwise = oneLookupFirst (tail xs) x

oneLookupSecond::Eq b => [(a,b)] -> b -> a
oneLookupSecond [] z = error "No such element"
oneLookupSecond xs x 
    | snd (head xs) == x = fst (head xs) 
    | otherwise = oneLookupSecond (tail xs) x

--12.6
compare::[a] -> [b] -> Bool
compare x y = length x <= length y

--12.8


f :: [a]->[b]->a->b
f (x:_) (y:_) z = y