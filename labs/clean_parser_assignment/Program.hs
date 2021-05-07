--Carl Österberg

module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program ([Statement.T]) deriving Show
instance Parse T where
  parse = parseProgram
  toString (Program stmnts) = foldr (++) [] (map (\x -> x ++"\n") (map toString stmnts))

parseProgram = iter Statement.parse >-> buildProgram
buildProgram = Program

exec (Program s) = Statement.exec s Dictionary.empty