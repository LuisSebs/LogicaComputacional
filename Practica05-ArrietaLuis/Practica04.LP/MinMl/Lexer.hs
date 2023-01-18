module MinMl.Lexer where
import Data.Char
import Data.List

type Boolean = Bool

type Var = Char 

type Fun = String

type Digit = Int

data MinMl 
      = MinMlVar Var
      | MinMlFun Fun
      | MinMlBool Boolean
      | MinMlDigit Digit
      | MinMlAdd MinMl MinMl
      | MinMlSub MinMl MinMl
      | MinMlMul MinMl MinMl
      | MinMlEq MinMl MinMl
      | MinMlGt MinMl MinMl
      | MinMlLt MinMl MinMl
      | MinMlGtE MinMl MinMl
      | MinMlLtE MinMl MinMl
      | MinMlAnd MinMl MinMl
      | MinMlOr MinMl MinMl
      | MinMlLet MinMl MinMl MinMl 
      | MinMlIf MinMl MinMl MinMl
      | MinMlLetRec MinMl MinMl 
      | MinMlFunc MinMl MinMl
      deriving(Show,Eq)

data Token 
        = TokenVar Var
        | TokenBool Boolean
        | TokenDigit Digit
        | TokenFun Fun
        | TokenOB
        | TokenCB
        | TokenAdd
        | TokenSub
        | TokenMul
        | TokenEq
        | TokenGt
        | TokenLt 
        | TokenGtE 
        | TokenLtE 
        | TokenAnd
        | TokenOr
        | TokenLet
        | TokenLetRec
        | TokenIf
        | TokenFunc
        deriving (Show,Eq)

lexer :: String -> [Token]
lexer string = auxLexerEq(auxLexer(string))

auxLexer :: String -> [Token]
auxLexer [] = []
auxLexer (c:cs) 
      | isSpace c = auxLexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
auxLexer ('+':cs) = TokenAdd : auxLexer cs
auxLexer ('*':cs) = TokenMul : auxLexer cs
auxLexer ('-':(x:xs)) = if (x == '>')
                        then auxLexer (xs)
                        else TokenSub : auxLexer (x:xs)
auxLexer ('(':cs) = TokenOB : auxLexer cs
auxLexer (')':cs) = TokenCB : auxLexer cs
auxLexer (':':cs) = auxLexer cs
auxLexer ('=':(c:x:xs)) = if (c == '=') 
                            then TokenEq : auxLexer (x:xs) 
                            else if (x == '>')
                                    then auxLexer (xs)
                                    else auxLexer (x:xs)
auxLexer ('>':(x:xs)) = if (x == '=') 
                        then TokenGtE : auxLexer (x:xs) 
                        else TokenGt : auxLexer (x:xs)
auxLexer ('<':(x:xs)) = if (x == '=') 
                        then TokenLtE : auxLexer (x:xs) 
                        else TokenLt : auxLexer (x:xs)

lexNum cs = TokenDigit (read num) : auxLexer rest
            where (num,rest) = span isDigit cs

lexVar cs =
   case span isAlpha cs of
      ("true", rest) -> TokenBool True : auxLexer rest
      ("false", rest) -> TokenBool False : auxLexer rest
      ("and", rest) -> TokenAnd : auxLexer rest
      ("or", rest) -> TokenOr : auxLexer rest
      ("let", rest) -> TokenLet : auxLexer rest
      ("in", rest) -> auxLexer rest
      ("end", rest) -> auxLexer rest
      ("if", rest) -> TokenIf : auxLexer rest
      ("then", rest) -> auxLexer rest
      ("else", rest) -> auxLexer rest
      ("letrec", rest) -> TokenLetRec : auxLexer rest
      ("fun", rest) -> TokenFunc : auxLexer rest
      ([x],rest)   -> TokenVar x : auxLexer rest
      (string, (x:xs)) -> TokenFunc : TokenFun string : auxLexer (xs)                            


auxLexerEq :: [Token] -> [Token]
auxLexerEq [] = []
auxLexerEq (TokenIf:TokenOB:TokenVar var:TokenDigit digit:xs) = TokenIf:TokenOB:TokenVar var:TokenEq:TokenDigit digit: auxLexerEq xs
auxLexerEq (x:xs) = x : auxLexerEq xs