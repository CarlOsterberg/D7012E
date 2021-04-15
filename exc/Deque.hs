--16.9
module Deque
    ( Deque,
        emptyDQ,
        isEmptyDQ,
        addFront,
        addBack,
        removeFront ,
        removeBack
    )   where

data Deque a = DQ [a] [a] deriving (Eq, Ord, Show)

emptyDQ :: Deque a
emptyDQ = DQ [] []

isEmptyDQ :: Deque a -> Bool
isEmptyDQ (DQ [] []) = True
isEmptyDQ _ = False

addFront :: a -> Deque a -> Deque a
addFront a (DQ xs ys) = DQ (a:xs) ys

addBack:: a -> Deque a -> Deque a
addBack b (DQ x y) = DQ x (b:y)

removeFront::Deque a -> (a,Deque a)
removeFront (DQ [] []) = error "Empty Deque" 
removeFront (DQ [] x) = removeFront (DQ (reverse x) [])
removeFront (DQ (x:xs) y) = (x, DQ xs y)

removeBack::Deque a -> (a,Deque a)
removeBack (DQ [] []) = error "Empty Deque"
removeBack (DQ x []) = removeBack (DQ [] (reverse x))
removeBack (DQ x (y:ys)) = (y, DQ x ys)