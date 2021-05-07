--Carl Österberg

module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    While Expr.T Statement |
    Read String |
    Begin [Statement] |
    Write Expr.T |
    Skip |
    Repeat Statement Expr.T
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skipStmnt = accept "skip" #- require ";" >-> buildSkip
buildSkip _ = Skip

ifStmnt= accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
buildIf ((e, s), s2) = If e s s2

whileStmnt = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e,s) = While e s

readStmnt = accept "read" -# word #- require ";" >-> buildRead
buildRead = Read

writeStmnt = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite = Write

beginStmnt = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin = Begin

repeatStmnt = accept "repeat" -# parse #- require "until" # Expr.parse #- require ";" >-> buildRepeat
buildRepeat (s, e) = Repeat s e

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ r = r
exec (If cond thenStmts elseStmts: stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (While cond stmnt: stmnts) dict input =
    if (Expr.value cond dict)>0
        then exec ([stmnt] ++ [(While cond stmnt)] ++ stmnts) dict input
        else exec stmnts dict input
exec (Skip :stmnts) dict input = exec stmnts dict input
exec ((Begin t):stmnts) dict input = exec (t++stmnts) dict input
exec (Write e:stmnts) dict input = exec stmnts dict (input ++ [Expr.value e dict])
exec (Assignment var e:stmnts) dict input = exec stmnts (Dictionary.insert (var, Expr.value e dict) dict) input
exec (Read v:stmnts) dict input = exec stmnts (Dictionary.insert (v,(head input)) dict) (tail input)
exec (Repeat s e:stmnts) dict input =
    if (Expr.value e dict)<0
        then exec (s:Repeat s e : stmnts) dict input
        else exec (s:stmnts) dict input

pickOut :: Maybe (Integer) -> String -> Integer
pickOut n s = case n of
        Nothing -> error ("undefined variable " ++ s)
        Just l -> l



instance Parse Statement where
  parse = assignment ! ifStmnt ! skipStmnt ! whileStmnt ! readStmnt ! writeStmnt ! beginStmnt ! repeatStmnt
  toString (Assignment v e) = v ++ " := " ++  toString e ++ ";"
  toString Skip = "Skip;"
  toString (If e s s2) = "if " ++ toString e ++ " then\n" ++ toString s ++ "\nelse\n" ++ toString s2
  toString (While e s) = "while " ++ toString e ++ "\n" ++ toString s
  toString (Begin stmnts) = "begin\n" ++ foldr (++) [] (map (\x ->x ++"\n") (map toString stmnts)) ++ "end"
  toString (Read v) = "read " ++ v ++ ";"
  toString (Write e) = "write " ++ toString e ++ ";"
  toString (Repeat s e) = "repeat\n" ++ (toString s) ++ "\nuntil " ++ toString e ++ ";"
