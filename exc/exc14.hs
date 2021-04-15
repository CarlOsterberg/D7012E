--14.1
weather :: Season -> Temp
--weather Summer = Hot
--weather _ = Cold
weather x
    | x == Summer = Hot
    | otherwise = Cold

data Temp = Cold | Hot
data Season = Spring | Summer | Autumn | Winter
    deriving (Eq)

--14.4
data Shape =
    Circle Float | Rectangle Float Float | Triangle Float Float Float
    deriving (Ord, Show, Read)

instance Eq Shape where
    Circle x == Circle y = x<0 && y<0 || x == y
    Rectangle x y == Rectangle a b = x == b && y == b
    Triangle x y z == Triangle a b c = x == a && y == b && z == c


perimeter :: Shape -> Float
perimeter (Rectangle x y) = x*2 + y*2
perimeter (Circle x) = 3.14 * x * 2
perimeter (Triangle x y z) = x + y + z

--14.5
isRound :: Shape -> Bool
isRound (Circle x) = True
isRound _ = False

area :: Shape -> Float
area (Rectangle x y) = x * y
area (Circle x) = 3.14 * x * x
area (Triangle x y z) = sqrt (4*x^2*y^2 - (x^2 + y^2 - z^2)^2)/4

isRegular :: Shape -> Bool
isRegular (Rectangle x y) = x == y
isRegular (Triangle x y z) = x == y && y == z
isRegular (Circle x) = True

--14.15
data Expr = Lit Int |
    Add Expr Expr |
    Sub Expr Expr |
    Mult Expr Expr |
    Div Expr Expr
    deriving (Show, Read)

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mult e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2

--14.16
size :: Expr -> Int
size (Lit a) = 0
size (Add a b) = 1 + size a + size b
size (Sub a b) = 1 + size a + size b
size (Mult a b) = 1 + size a + size b
size (Div a b) = 1 + size a + size b

--14.21 - 14.24
data NTree = NilT | Node Int NTree NTree
    deriving (Show,Read)

getSubtrees:: NTree -> (NTree, NTree)
getSubtrees (Node i l r) = (l,r)

exists::NTree -> Int -> Bool
exists NilT _ = False
exists (Node i l r) v
    | i == v = True
    | otherwise = exists l v || exists r v

getMax::NTree -> Int
getMax NilT = minBound
getMax (Node i l r) = max i (max (getMax l) (getMax r))

getMin::NTree -> Int
getMin NilT = maxBound
getMin (Node i l r) = min i (min (getMin l) (getMin r))

reflect::NTree -> NTree
reflect NilT = NilT
reflect (Node i l r) = Node i r l

--14.33
data Gtree a = Null | Nod a (Gtree a) (Gtree a)
    deriving (Show, Read)

numLeaves::Gtree a -> Int
numLeaves Null = 0
numLeaves (Nod i l r) = 1 + numLeaves l + numLeaves r

depth::Gtree a-> Int
depth Null = 0
depth (Nod i l r) = max (depth l + 1) (depth r + 1)

sumTree::Num a => Gtree a -> a
sumTree Null = 0
sumTree (Nod i l r) = i + sumTree l + sumTree r

mapTree:: (a->b) -> Gtree a -> Gtree b
mapTree _ Null = Null
mapTree f (Nod i l r) = Nod (f i) (mapTree f l) (mapTree f r)

flatten::Gtree a -> [a]
flatten Null = []
flatten (Nod i l r) = [i] ++ flatten l ++ flatten r