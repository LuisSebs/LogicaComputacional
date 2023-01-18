module MinHs.Type where

import MinHs.Lexer as LexHs
import MinHs.Parser as ParHs
import MinHs.Compiler as CompHs
import MinHs.Interpreter as IntHs

import Data.Bool (Bool)
import Data.List


type Ctx = [(MinHs,ParHs.Type)]


verifyAux :: Expr -> Either Int Bool -> ParHs.Type
verifyAux expr (Left num) = ParHs.Nat
verifyAux expr (Right bool) = ParHs.Boolean


verify :: Expr -> Ctx -> ParHs.Type
verify (ExprRecFun  func typeFunc var expr (ExprVoid)) _ =  exprToType(typeFunc)
verify (ExprRecFun2 func typeFunc var1 var2 expr (ExprVoid) param2) _ = exprToType(typeFunc)
verify expr ctx = verifyAux (expr) (IntHs.eval(CompHs.curry(expr))) 


exprToType :: Expr -> ParHs.Type
exprToType (LexHs.Nat) = ParHs.Nat
exprToType (LexHs.Bool) = ParHs.Boolean
exprToType (LexHs.Arrow type1 type2) = ParHs.Arrow (exprToType(type1)) (exprToType(type2))