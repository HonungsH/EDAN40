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

skipStatement = accept "skip" -# require ";" >-> buildSkip
buildSkip s = Skip

beginStatement = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin = Begin

whileStatement = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e, s) = While e s 

readStatement = accept "read" -# word #- require ";" >-> buildRead
buildRead s = Read s

writeStatement = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite e = Write e 

writeComment = accept "--" -# comment #- require "\n" >-> buildComment
buildComment s = Comment s

nbrOfTabs tabs = replicate tabs '\t'

printProgram :: Int -> Statement -> String
printProgram tabs (Assignment s e) = nbrOfTabs tabs ++ s ++ " := " ++ Expr.toString e ++ ";\n"
printProgram tabs (If e s1 s2) = nbrOfTabs tabs ++ "if " ++ Expr.toString e ++ " then\n" ++ printProgram (tabs + 1) s1 ++ nbrOfTabs tabs ++ "else\n" ++ printProgram (tabs + 1) s2
printProgram tabs (Skip) = nbrOfTabs tabs ++ "skip;\n"
printProgram tabs (Begin stlist) = nbrOfTabs tabs ++ "begin\n" ++ concat (map (printProgram (tabs + 1)) stlist) ++ nbrOfTabs tabs ++ "end\n"
printProgram tabs (While e s) = nbrOfTabs tabs ++ "while " ++ Expr.toString e ++ " do\n" ++ printProgram (tabs + 1) s
printProgram tabs (Read s) = nbrOfTabs tabs ++ "read " ++ s ++ ";\n"
printProgram tabs (Write e) = nbrOfTabs tabs ++ "write " ++ Expr.toString e ++ ";\n"
printProgram tabs (Comment s) = nbrOfTabs tabs ++ "--" ++ s ++ "\n" 

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
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
  toString = printProgram 0