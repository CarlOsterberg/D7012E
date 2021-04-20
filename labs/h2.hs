--get diff for x^2 to work
--ur m

import Data.Char

data EXPR = Const Int
     | Var String
     | Op String EXPR EXPR
     | App String EXPR deriving (Eq, Ord, Show)

parse :: String -> EXPR
parse = fst . buildexpr
  where
    notfirst p (_,[]) = True
    notfirst p (_,x:xs) = not (p x)

    buildnumber :: String -> (EXPR,String)
    buildnumber xs = until (notfirst isDigit) accdigits (Const 0, xs)
      where
        accdigits :: (EXPR,String) -> (EXPR,String)
        accdigits (Const n, y:ys) = (Const(10*n+(ord y - 48)), ys)

    buildvar :: String -> (EXPR,String)
    buildvar xs = until (notfirst isLetter) accletters (Var "", xs)
      where
        accletters :: (EXPR,String) -> (EXPR,String)
        accletters (Var s, y:ys) = (Var (s ++[y]), ys)


    buildexpr :: String -> (EXPR,String)
    buildexpr xs = until (notfirst (\c -> c=='-' || c=='+')) accterms (buildterm xs)
      where
        accterms :: (EXPR,String) -> (EXPR,String)
        accterms (term, y:ys) = (Op [y] term term1, zs)
          where
            (term1,zs) = buildterm ys

    buildterm :: String -> (EXPR,String)
    buildterm xs = until (notfirst (\c -> c=='*' || c=='/')) accfactors (buildfactor xs)
      where
        accfactors :: (EXPR,String) -> (EXPR,String)
        accfactors (fact, y:ys) = (Op [y] fact fact1, zs)
          where
            (fact1,zs) = buildfactor ys

    buildfactor :: String -> (EXPR,String)
    buildfactor [] = error "missing factor"
    buildfactor ('(':xs) =  case buildexpr xs of (e, ')':ws) -> (e, ws); _ -> error "missing factor"
    buildfactor (x:xs)
      | isDigit x = buildnumber (x:xs)
      | isLetter x = case buildvar (x:xs) of
                       (Var s, '(':zs) -> let (e,ws)=buildfactor ('(':zs) in (App s e,ws)
                       p -> p
      | otherwise = error "illegal symbol"

unparse :: EXPR -> String
unparse (Const n) = show n
unparse (Var s) = s
unparse (Op oper e1 e2) = "(" ++ unparse e1 ++ oper ++ unparse e2 ++ ")"
unparse (App func arg) = func ++ "(" ++ unparse arg ++ ")"

eval :: EXPR -> [(String,Float)] -> Float
eval (Const n) _ = fromIntegral n
eval (Var x) env = case lookup x env of Just y -> y ; _ -> error (x ++ " undefined")
eval (Op "+" left right) env = eval left env + eval right env
eval (Op "-" left right) env = eval left env - eval right env
eval (Op "*" left right) env = eval left env * eval right env
eval (Op "/" left right) env = eval left env / eval right env
eval (App "sin" arg) env = sin (eval arg env)
eval (App "cos" arg) env = cos (eval arg env)
eval (App "log" arg) env = log (eval arg env)
eval (App "exp" arg) env = exp (eval arg env)

diff :: EXPR -> EXPR -> EXPR
diff _ (Const _) = Const 0
diff (Var id) (Var id2)
  | id == id2 = Const 1
  | otherwise = Const 0
diff v (Op "+" e1 e2) = Op "+" (diff v e1) (diff v e2)
diff v (Op "-" e1 e2) = Op "-" (diff v e1) (diff v e2)
diff v (Op "*" e1 e2) =
  Op "+" (Op "*" (diff v e1) e2) (Op "*" e1 (diff v e2))
diff v (Op "/" e1 e2) =
  Op "/" (Op "-" (Op "*" (diff v e1) e1) (Op "*" e1 (diff v e2))) (Op "*" e2 e2)
diff v (App "sin" arg) = Op "*" (App "cos" arg) (diff v arg)
diff v (App "cos" arg) = Op "*" (Op "-" (Const 0) (App "sin" arg)) (diff v arg)
diff v (App "log" arg) = Op "*" (Op "/" (Const 1) arg) (diff v arg)
diff v (App "exp" arg) = Op "*" (App "exp" arg) (diff v arg)
diff _ _ = error "can not compute the derivative"

simplify :: EXPR -> EXPR
simplify (Const n) = Const n
simplify (Var id) = Var id
simplify (Op oper left right) =
  let (lefts,rights) = (simplify left, simplify right) in
    case (oper, lefts, rights) of
      ("+",e,Const 0) -> e
      ("+",Const 0,e) -> e
      ("*",e,Const 0) -> Const 0
      ("*",Const 0,e) -> Const 0
      ("*",e,Const 1) -> e
      ("*",Const 1,e) -> e
      ("-",e,Const 0) -> e
      ("/",e,Const 1) -> e
      ("-",le,re)     -> if left==right then Const 0 else Op "-" le re
      (op,le,re)      -> Op op le re
simplify (App id args) =
    let arg = simplify args in
        case (id, arg) of
            ("log", App "exp" e) -> e
            ("exp", App "log" e) -> e
            (ids, e) -> App ids e

mkfun ::(EXPR, EXPR) -> (Float -> Float)
mkfun (exp, arg) x = eval exp [(unparse arg, x)]

--x_(n+1)=x_n−f(x_n)/f′(x_n)
findzero :: String -> String -> Float -> Float
findzero arg f x_n0 =
  if abs(x_n0 - x_n1) <= 0.0001
    then x_n1
  else findzero arg f x_n1
    where x_n1 = x_n0 - eval (Op "/" (parse f) (diff (parse arg) (parse f))) [(arg,x_n0)]

pow4 :: EXPR
pow4 = simplify (diff (Var "x") (parse "x*x*x*x"))

ln :: EXPR
ln = simplify (diff (Var "x") (parse "log(x*x)"))

e :: EXPR
e = simplify (diff (Var "x") (parse "exp(x*x)"))

test1 :: String
test1 = unparse (simplify (diff (Var "x") (parse "exp(sin(2*x))")))

test2 :: Float -> Float
test2 x = mkfun (parse "x*x+2", Var "x") x

test3 :: Float
test3 = findzero "x" "x*x*x+x-1" 1.0

test4 :: Float
test4 = findzero "y" "cos(y)*sin(y)" 2.0