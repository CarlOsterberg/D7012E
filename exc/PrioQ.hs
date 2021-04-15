--16.11
module PrioQ
    (
        PrioQ,
        emptyPQ,
        isEmptyPQ,
        insertWithPrio,
        pullHPrio
    ) where

data PrioQ a = PQ [(a,Int)] deriving (Eq, Ord, Show)

emptyPQ :: PrioQ a
emptyPQ = PQ []

isEmptyPQ :: PrioQ a -> Bool
isEmptyPQ (PQ []) = True
isEmptyPQ _ = False

insertWithPrio :: PrioQ a -> (a, Int) -> PrioQ a
insertWithPrio (PQ []) e = PQ [e]
insertWithPrio (PQ (x:xs)) p
    | snd p <= snd x = PQ ([p] ++ [x] ++ xs)
    | otherwise = PQ (x : extractList (insertWithPrio (PQ xs) p))

extractList :: PrioQ a -> [(a,Int)]
extractList (PQ a) = a

pullHPrio :: PrioQ a -> (a, PrioQ a)
pullHPrio (PQ []) = error "Empty PQ"
pullHPrio (PQ (x:xs)) = (fst x,PQ xs)



