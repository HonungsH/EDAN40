module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Skip |
    Begin [Statement] | 
    While Expr.T Statement |
    Read String |
    Write Expr.T |
    Comment String
        deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

ifStatement = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
buildIf ((e, s1), s2) = If e s1 s2  

skipStatement = accept "skip" # require ";" >-> buildSkip
buildSkip s = Skip

beginStatement = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin = Begin

whileStatement = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e, s) = While e s 

readStatement = accept "read" -# word #- require ";" >-> buildRead
buildRead s = Read s

writeStatement = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite e = Write e 

writeComment = accept "--" -# comment >-> buildComment
buildComment s = Comment s

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
    
exec (Assignment name expression : stmts) dict input = exec stmts (Dictionary.insert (name, Expr.value expression dict) dict) input
exec (Skip : stmts) dict input = exec stmts dict input
exec (Begin list : stmts) dict input = exec (list ++ stmts) dict input
exec (While expression statement : stmts) dict input =
    if Expr.value expression dict > 0   then exec (statement : (While expression statement) : stmts) dict input
                                        else exec stmts dict input
                                        
exec (Read string : stmts) dict (input : rest) = exec stmts (Dictionary.insert (string, input) dict) rest
exec (Write expression : stmts) dict input = (Expr.value expression dict) : (exec stmts dict input)
exec (Comment string : stmts) dict input = exec stmts dict input

instance Parse Statement where
  parse = assignment ! skipStatement ! beginStatement ! ifStatement ! whileStatement ! readStatement ! writeStatement ! writeComment
  toString = error "Statement.toString not implemented"