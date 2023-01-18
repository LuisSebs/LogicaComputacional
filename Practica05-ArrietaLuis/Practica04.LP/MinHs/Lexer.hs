module MinHs.Lexer where
--module Lexer where

import Data.Char
import Data.List

type Boolean = Bool

type Var = Char 

type Fun = String

type Digit = Int

data Expr 
      = ExprVar Var
      | ExprFun Fun
      | ExprBool Boolean
      | ExprDigit Digit
      | ExprAdd Expr Expr
      | ExprSub Expr Expr
      | ExprMul Expr Expr
      | ExprEq Expr Expr
      | ExprGt Expr Expr
      | ExprLt Expr Expr
      | ExprGtE Expr Expr
      | ExprLtE Expr Expr
      | ExprAnd Expr Expr
      | ExprOr Expr Expr
      | ExprLet Expr Expr Expr 
      | ExprIf Expr Expr Expr
      | ExprRecFun Expr Expr Expr Expr Expr
      | ExprFunc Expr Expr
      | ExprRecFun2 Expr Expr Expr Expr Expr Expr Expr
      | ExprFunc2 Expr Expr Expr
      | ExprVoid
      | Nat 
      | Bool
      | Arrow Expr Expr
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
        | TokenIf
        | TokenRecFun
        | TokenRecFun2
        | TokenFunc
        | TokenFunc2
        | TokenTypeNat
        | TokenTypeBool
        | TokenTypeArrow
        | TokenVoid
        deriving (Show,Eq)

lexer :: String -> [Token]
lexer string = auxLexerFunc(auxLexerType(auxLexer(string)))

auxLexer :: String -> [Token]
auxLexer [] = []
auxLexer (c:(x:xs)) 
      | isSpace c = if (isSpace x) == True
                        then TokenVoid : auxLexer (xs)
                        else auxLexer (x:xs)

      | isAlpha c = lexVar (c:(x:xs))
      | isDigit c = lexNum (c:(x:xs))
auxLexer ('+':cs) = TokenAdd : auxLexer cs
auxLexer ('*':cs) = TokenMul : auxLexer cs
auxLexer ('-':(x:xs)) = if (x == '>')
                        then TokenTypeArrow : auxLexer (xs)
                        else TokenSub : auxLexer (x:xs)
auxLexer ('(':cs) = TokenOB : auxLexer cs
auxLexer (')':cs) = TokenCB : auxLexer cs
auxLexer (':':cs) = auxLexer cs
auxLexer ('=':(x:xs)) = if (x == '=') 
                        then TokenEq : auxLexer (x:xs) 
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
      ("recfun", rest) -> TokenRecFun : auxLexer rest
      ("Nat", rest) -> TokenTypeNat : auxLexer rest
      ("Bool", rest) -> TokenTypeBool : auxLexer rest
      ([x],rest)   -> TokenVar x : auxLexer rest
      (string,(c:x:xs)) -> if (isSpace c && x == '(') 
                            then TokenFunc : TokenFun string : auxLexer (x:xs) 
                            else TokenFun string : auxLexer (x:xs)


auxLexerType :: [Token] -> [Token]
auxLexerType [] = []
auxLexerType (TokenOB:TokenRecFun:TokenFun fun:TokenOB:TokenTypeNat:TokenTypeArrow:TokenTypeNat:TokenTypeArrow:TokenTypeNat:TokenCB:xs) = auxFunc2Void (TokenOB:TokenRecFun2:TokenFun fun:TokenOB:TokenTypeNat:TokenTypeArrow:TokenOB:TokenTypeNat:TokenTypeArrow:TokenTypeNat:TokenCB:TokenCB:xs)
auxLexerType (x:xs) = (x:xs)

auxLexerFunc :: [Token] -> [Token]
auxLexerFunc lst = if (auxContains (TokenRecFun2) lst == True )
                        then auxFuncToFunc2 lst
                        else lst

auxContains :: Token -> [Token] -> Bool
auxContains _ [] = False
auxContains token (x:xs)
    | token == x = True
    | otherwise = auxContains token xs

auxFuncToFunc2 :: [Token] -> [Token]
auxFuncToFunc2 [] = []
auxFuncToFunc2 (x:xs) = if (x == TokenFunc)
                            then TokenFunc2 : auxFuncToFunc2 (xs)
                            else x : auxFuncToFunc2 (xs)

auxFunc2Void :: [Token] -> [Token]
auxFunc2Void [] = []
auxFunc2Void (x:xs) = if(x == TokenVoid)
                        then TokenVoid : TokenVoid : auxFunc2Void (xs)
                        else x : auxFunc2Void (xs)


