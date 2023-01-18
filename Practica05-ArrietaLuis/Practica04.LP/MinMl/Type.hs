module MinMl.Type where

import MinMl.Lexer as LexMl
import MinMl.Parser as ParMl
import MinMl.Interpreter as IntMl

import Data.Bool (Bool)
import Data.List


data Type 
        = Nat
        | Boolean
        | Arrow Type Type
        | TVar Char
        | TVar2 LexMl.MinMl
        | TypeOf LexMl.MinMl
        deriving (Show, Eq)



-- | Sinonimo para una restricción como una dupla de tipos (T1,T2) que representa T1 = T2
type Rest = (Type,Type)

-- | Sinonimo para un unificador representando una serie de substituciones
type Unifier = [Substitution]
type Substitution = (Type, Type)

rest :: MinMl -> [Rest]
rest e@(MinMlVar x) = [(TypeOf e, TVar x)]
rest e@(MinMlDigit _) = [(TypeOf e, Nat)]
rest e@(MinMlBool _) = [(TypeOf e, Boolean)]
rest e@(MinMlAdd x y) = (r1 `union` r2) `union` [(TypeOf x, Nat),(TypeOf y, Nat),(TypeOf e, Nat)]
                        where r1 = rest x; r2 = rest y
rest e@(MinMlSub x y) = (r1 `union` r2) `union` [(TypeOf x, Nat),(TypeOf y, Nat),(TypeOf e, Nat)]
                        where r1 = rest x; r2 = rest y                        
rest e@(MinMlMul x y) = (r1 `union` r2) `union` [(TypeOf x, Nat),(TypeOf y, Nat),(TypeOf e, Nat)]
                        where r1 = rest x; r2 = rest y
rest e@(MinMlEq p q) = (r1 `union` r2) `union` [(TypeOf p, Nat),(TypeOf q, Nat),(TypeOf e, Nat)]
                        where r1 = rest p; r2 = rest q
rest e@(MinMlGt p q) = (r1 `union` r2) `union` [(TypeOf p, Nat),(TypeOf q, Nat),(TypeOf e, Nat)]
                        where r1 = rest p; r2 = rest q
rest e@(MinMlLt p q) = (r1 `union` r2) `union` [(TypeOf p, Nat),(TypeOf q, Nat),(TypeOf e, Nat)]
                        where r1 = rest p; r2 = rest q
rest e@(MinMlIf cond thenIf elseIf) = union rf (r1 `union` r2) `union` [(TypeOf cond, Boolean),(TypeOf thenIf, TypeOf elseIf),(TypeOf e, TypeOf thenIf),(TypeOf e, TypeOf elseIf)]
                        where rf = rest cond; r1 = rest thenIf; r2 = rest elseIf
rest e@(MinMlLet var val expr) = (r1 `union` r2) `union` [(TVar2 var, TypeOf val),(TypeOf e, TypeOf expr)]
                        where r1 = rest val; r2 = rest expr
rest e@(MinMlLetRec func param) = (r1 `union` r2) `union` [(TypeOf func, Arrow (TypeOf param) (TypeOf e))]
                        where r1 = rest func; r2 = rest param



infer :: MinMl -> Type
infer expr@(MinMlLetRec func param) = verifyType (IntMl.recFun (expr)) (IntMl.eval(expr))
infer expr@(MinMlFunc var exprFunc) = verifyExpr (expr)
infer _ = error "Infer error"


verifyType :: MinMl -> Either Int Bool -> Type
verifyType expr (Left num) = Nat
verifyType expr (Right bool) = Boolean

verifyExpr :: MinMl -> Type
verifyExpr (MinMlVar _) = Nat
verifyExpr (MinMlDigit _) = Nat
verifyExpr (MinMlBool _) = Boolean
verifyExpr (MinMlFunc var (MinMlIf (cond) (thenIf) (elseIf))) = Arrow (verifyExpr var) (verifyExpr thenIf)
verifyExpr (MinMlFunc var (expr)) = Arrow (verifyExpr var) (verifyExpr expr)
verifyExpr _ = Nat --Caso "de error"
