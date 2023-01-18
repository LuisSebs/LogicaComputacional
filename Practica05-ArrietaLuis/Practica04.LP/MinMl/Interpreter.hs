module MinMl.Interpreter where

import MinMl.Parser
import MinMl.Lexer

eval :: MinMl -> Either Int Bool
eval (MinMlBool bool) = Right bool
eval (MinMlDigit digit) = Left digit
eval (MinMlAdd x y) = Left (auxOp (MinMlAdd x y))
eval (MinMlSub x y) = Left (auxOp (MinMlSub x y))
eval (MinMlMul x y) = Left (auxOp (MinMlMul x y))
eval (MinMlEq p q) = Right (auxProp (MinMlEq p q))
eval (MinMlGt p q) = Right (auxProp (MinMlGt (MinMlDigit (auxOp p)) (MinMlDigit (auxOp q))))
eval (MinMlLt p q) = Right (auxProp (MinMlLt (MinMlDigit (auxOp p)) (MinMlDigit (auxOp q))))
eval (MinMlGtE p q) = Right (auxProp (MinMlGtE (MinMlDigit (auxOp p)) (MinMlDigit (auxOp q))))
eval (MinMlLtE p q) = Right (auxProp (MinMlLtE (MinMlDigit (auxOp p)) (MinMlDigit (auxOp q))))
eval (MinMlAnd p q) = Right (auxProp (MinMlAnd p q))
eval (MinMlOr p q) = Right (auxProp (MinMlOr p q))
eval (MinMlLet var val expr) = Left (auxLet (letAux(MinMlLet var val expr)))
eval (MinMlIf cond ent sino) = Left (auxIf (MinMlIf cond ent sino))
eval (MinMlLetRec func param) = auxEvalLet (recFun (MinMlLetRec func param))
eval (MinMlFunc func param) = Left (auxFun (MinMlFunc func param))

auxEvalLet :: MinMl -> Either Int Bool
auxEvalLet (MinMlDigit digit) = Left digit
auxEvalLet (MinMlBool bool) = Right bool

minMlToInt :: MinMl -> Int
minMlToInt (MinMlDigit digit) = digit
minMlToInt _ = 0


auxFun :: MinMl -> Int
auxFun (MinMlFunc (param) (expr)) = auxLet(MinMlLet (MinMlVar 'x') (param) (expr))


auxLet :: MinMl -> Int
auxLet (MinMlLet (MinMlVar var) (MinMlDigit val) (MinMlDigit digit)) = digit
auxLet (MinMlLet (MinMlVar var1) (MinMlDigit val) (MinMlVar var2)) = if (var1 == var2) == True
                                                                        then val 
                                                                        else 0
auxLet (MinMlLet (MinMlVar var) (MinMlDigit val) (MinMlAdd x y)) = auxLet (MinMlLet (MinMlVar var) (MinMlDigit val) x) + auxLet (MinMlLet (MinMlVar var) (MinMlDigit val) y)
auxLet (MinMlLet (MinMlVar var) (MinMlDigit val) (MinMlMul x y)) = auxLet (MinMlLet (MinMlVar var) (MinMlDigit val) x) * auxLet (MinMlLet (MinMlVar var) (MinMlDigit val) y)
auxLet (MinMlLet (MinMlVar var) (MinMlDigit val) (MinMlLet (MinMlVar var2) (MinMlDigit val2) eab)) = auxLet (MinMlLet (MinMlVar var) (MinMlDigit val) (MinMlDigit (auxLet(MinMlLet (MinMlVar var2) (MinMlDigit val2) eab))))

auxLet (MinMlLet (MinMlVar var) (MinMlDigit val) (MinMlLet (MinMlVar var2) (MinMlVar varAsign) eab)) = if (varAsign == var) == True 
                                                                                                          then  auxLet (MinMlLet (MinMlVar var) (MinMlDigit val) (MinMlDigit (auxLet(MinMlLet (MinMlVar var2) (MinMlDigit val) eab))))
                                                                                                          else error "La variable que estás asignando no está declarada"

auxLet (MinMlLet (MinMlVar var) (MinMlDigit val) (MinMlLet (MinMlVar var2) (MinMlAdd (MinMlVar varAsign) (x)) eab)) = if (varAsign == var) == True 
                                                                                                                        then auxLet (MinMlLet (MinMlVar var) (MinMlDigit val) ( MinMlDigit (auxLet(MinMlLet (MinMlVar var2) (MinMlAdd (MinMlDigit val) (x)) eab )) ))
                                                                                                                        else error "La variable que estás asignando no está declarada"

auxLet (MinMlLet (MinMlVar var) (MinMlMul (x1) (x2)) op) = auxLet (MinMlLet (MinMlVar var) (MinMlDigit (auxOp (MinMlMul (x1) (x2)))) (op))
auxLet (MinMlLet (MinMlVar var) (MinMlAdd (x1) (x2)) op) = auxLet (MinMlLet (MinMlVar var) (MinMlDigit (auxOp (MinMlAdd (x1) (x2)))) (op))
auxLet (MinMlLet (MinMlVar var) (MinMlLet (MinMlVar var2) asign op2) op) = auxLet (MinMlLet (MinMlVar var) (MinMlDigit (auxLet(MinMlLet (MinMlVar var2) (asign) (op2)))) (op))


auxIf :: MinMl -> Int 
auxIf (MinMlIf (MinMlEq p q) thenIf elseIf) = let tIf = auxOp(thenIf); eIf = auxOp(elseIf) in if (auxProp(MinMlEq p q))
                                                                                                then tIf
                                                                                                else eIf
auxIf (MinMlIf (MinMlGt p q) thenIf elseIf) = let tIf = auxOp(thenIf); eIf = auxOp(elseIf) in if (auxProp(MinMlGt p q))
                                                                                                then tIf
                                                                                                else eIf
auxIf (MinMlIf (MinMlLt p q) thenIf elseIf) = let tIf = auxOp(thenIf); eIf = auxOp(elseIf) in if (auxProp(MinMlLt p q))
                                                                                                then tIf
                                                                                                else eIf
auxIf (MinMlIf (MinMlGtE p q) thenIf elseIf) = let tIf = auxOp(thenIf); eIf = auxOp(elseIf) in if (auxProp(MinMlGtE p q))
                                                                                                then tIf
                                                                                                else eIf
auxIf (MinMlIf (MinMlLtE p q) thenIf elseIf) = let tIf = auxOp(thenIf); eIf = auxOp(elseIf) in if (auxProp(MinMlLtE p q))
                                                                                                then tIf
                                                                                                else eIf

auxOp :: MinMl -> Int 
auxOp (MinMlDigit digit) = digit
auxOp (MinMlAdd x y) = auxOp (x) + auxOp (y)
auxOp (MinMlMul x y) = auxOp (x) * auxOp (y)
auxOp (MinMlSub x y) = auxOp (x) - auxOp (y)
auxOp eab = auxLet eab


auxProp :: MinMl -> Bool
auxProp (MinMlBool bool) = bool
auxProp (MinMlAnd p q) = auxProp (p) && auxProp (q)
auxProp (MinMlOr p q) = auxProp (p) || auxProp (q)
auxProp (MinMlEq (MinMlDigit p1) (MinMlDigit p2)) = p1 == p2
auxProp (MinMlEq p q) = auxProp (p) == auxProp (q)
auxProp (MinMlGt (MinMlDigit p1) (MinMlDigit p2)) = p1 > p2
auxProp (MinMlLt (MinMlDigit p1) (MinMlDigit p2)) = p1 < p2
auxProp (MinMlGtE (MinMlDigit p1) (MinMlDigit p2)) = p1 >= p2
auxProp (MinMlLtE (MinMlDigit p1) (MinMlDigit p2)) = p1 <= p2


recFun :: MinMl -> MinMl 
recFun (MinMlLetRec (MinMlFunc (MinMlFun func) (MinMlFunc (varFun) (expr))) (MinMlFunc (MinMlFun funcParam) (param))) = recFunAux (MinMlLetRec (MinMlFunc (MinMlFun func) (MinMlFunc (varFun) expr)) (MinMlFunc (MinMlFun funcParam) (param))) (letAux (MinMlLet (varFun) (param) (expr)))


recFunAux :: MinMl -> MinMl -> MinMl
recFunAux (MinMlLetRec (MinMlFunc (MinMlFun func) (expr) ) (MinMlFunc (MinMlFun funcParam) (param))) (MinMlDigit digit1)  = MinMlDigit digit1
recFunAux (MinMlLetRec (MinMlFunc (MinMlFun func) (expr) ) (MinMlFunc (MinMlFun funcParam) (param))) (MinMlBool bool)  = MinMlBool bool
recFunAux (MinMlLetRec (MinMlFunc (MinMlFun func) (expr) ) (MinMlFunc (MinMlFun funcParam) (param))) (MinMlFunc (MinMlFun fun1) op1) = if (func == fun1) == True
                                                                                                                                        --then recFun (MinMlRecFun (MinMlFun func) var op (MinMlDigit (auxOp(op1))))
                                                                                                                                        then recFun (MinMlLetRec (MinMlFunc (MinMlFun func) (expr)) (MinMlFunc (MinMlFun func) (MinMlDigit (auxOp(op1)))))
                                                                                                                                        else error "Error de recursion"
recFunAux (MinMlLetRec (MinMlFunc (MinMlFun func) (expr) ) (MinMlFunc (MinMlFun funcParam) (param))) (MinMlIf (cond) ent (MinMlMul (x) (y)))  = if (auxProp(cond) == True)
                                                                                                                                                    then ent
                                                                                                                                                    else MinMlDigit (auxOp (MinMlMul (recFunAux (MinMlLetRec (MinMlFunc (MinMlFun func) (expr) ) (MinMlFunc (MinMlFun funcParam) (param))) (x)) (recFunAux (MinMlLetRec (MinMlFunc (MinMlFun func) (expr) ) (MinMlFunc (MinMlFun funcParam) (param))) (y))))
recFunAux (MinMlLetRec (MinMlFunc (MinMlFun func) (expr) ) (MinMlFunc (MinMlFun funcParam) (param))) (MinMlIf (cond) ent (MinMlAdd (x) (y)))  = if (auxProp(cond) == True)
                                                                                                                                                    then ent
                                                                                                                                                    else MinMlDigit (auxOp (MinMlAdd (recFunAux (MinMlLetRec (MinMlFunc (MinMlFun func) (expr) ) (MinMlFunc (MinMlFun funcParam) (param))) (x)) (recFunAux (MinMlLetRec (MinMlFunc (MinMlFun func) (expr) ) (MinMlFunc (MinMlFun funcParam) (param))) (y))))
recFunAux (MinMlLetRec (MinMlFunc (MinMlFun func) (expr) ) (MinMlFunc (MinMlFun funcParam) (param))) (MinMlIf (cond) ent (MinMlSub (x) (y)))  = if (auxProp(cond) == True)
                                                                                                                                                    then ent
                                                                                                                                                    else MinMlDigit (auxOp (MinMlSub (recFunAux (MinMlLetRec (MinMlFunc (MinMlFun func) (expr) ) (MinMlFunc (MinMlFun funcParam) (param))) (x)) (recFunAux (MinMlLetRec (MinMlFunc (MinMlFun func) (expr) ) (MinMlFunc (MinMlFun funcParam) (param))) (y))))
recFunAux (MinMlLetRec (MinMlFunc (MinMlFun func) (expr) ) (MinMlFunc (MinMlFun funcParam) (param))) (MinMlIf (cond) thenIf elseIf) = if (auxProp(cond) == True)
                                                                                                                                            then thenIf
                                                                                                                                            else elseIf
recFunAux (MinMlLetRec (MinMlFunc (MinMlFun func) (expr) ) (MinMlFunc (MinMlFun funcParam) (param))) (MinMlLet (var) (val) (exprLet)) = error "Sin implementar LET"

recFunAux (MinMlLetRec (MinMlFunc (MinMlFun func) (expr) ) (MinMlFunc (MinMlFun funcParam) (param))) (MinMlMul (x) (y)) = MinMlDigit (auxOp (MinMlMul (recFunAux (MinMlLetRec (MinMlFunc (MinMlFun func) (expr) ) (MinMlFunc (MinMlFun funcParam) (param))) (x)) (recFunAux (MinMlLetRec (MinMlFunc (MinMlFun func) (expr) ) (MinMlFunc (MinMlFun funcParam) (param))) (y))))
recFunAux (MinMlLetRec (MinMlFunc (MinMlFun func) (expr) ) (MinMlFunc (MinMlFun funcParam) (param))) (MinMlAdd (x) (y)) = MinMlDigit (auxOp (MinMlAdd (recFunAux (MinMlLetRec (MinMlFunc (MinMlFun func) (expr) ) (MinMlFunc (MinMlFun funcParam) (param))) (x)) (recFunAux (MinMlLetRec (MinMlFunc (MinMlFun func) (expr) ) (MinMlFunc (MinMlFun funcParam) (param))) (y))))
recFunAux (MinMlLetRec (MinMlFunc (MinMlFun func) (expr) ) (MinMlFunc (MinMlFun funcParam) (param))) (MinMlSub (x) (y)) = MinMlDigit (auxOp (MinMlSub (recFunAux (MinMlLetRec (MinMlFunc (MinMlFun func) (expr) ) (MinMlFunc (MinMlFun funcParam) (param))) (x)) (recFunAux (MinMlLetRec (MinMlFunc (MinMlFun func) (expr) ) (MinMlFunc (MinMlFun funcParam) (param))) (y))))

letAux :: MinMl -> MinMl
letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (MinMlDigit digit)) = MinMlDigit digit
letAux (MinMlLet (MinMlVar var1) (MinMlDigit val) (MinMlVar var2)) = if (var1 == var2) == True
                                                                        then MinMlDigit val 
                                                                        else (MinMlVar var2)
letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (MinMlFunc (MinMlFun fun) (MinMlMul (x) (y)))) = MinMlFunc (MinMlFun fun) (MinMlMul (letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (x))) (letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (y))))                                  
letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (MinMlFunc (MinMlFun fun) (MinMlSub (x) (y)))) = MinMlFunc (MinMlFun fun) (MinMlSub (letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (x))) (letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (y))))
letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (MinMlFunc (MinMlFun fun) (MinMlAdd (x) (y)))) = MinMlFunc (MinMlFun fun) (MinMlAdd (letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (x))) (letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (y))))
letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (MinMlEq x y)) = MinMlEq (letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (x))) (letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (y)))
letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (MinMlGt x y)) = MinMlGt (letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (x))) (letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (y)))
letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (MinMlLt x y)) = MinMlLt (letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (x))) (letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (y)))
letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (MinMlGtE x y)) = MinMlGtE (letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (x))) (letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (y)))
letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (MinMlLtE x y)) = MinMlLtE (letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (x))) (letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (y)))
letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (MinMlIf (cond) (thenIf) (elseIf))) = MinMlIf (letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (cond))) (letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (thenIf))) (letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (elseIf)))
letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (MinMlMul x y)) = MinMlMul (letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (x))) (letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (y)))
letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (MinMlAdd x y)) = MinMlAdd (letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (x))) (letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (y)))
letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (MinMlSub x y)) = MinMlSub (letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (x))) (letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (y)))
letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (MinMlLet (MinMlVar var2) (MinMlDigit val2) (expr))) = letAux (MinMlLet (MinMlVar var) (MinMlDigit val) (letAux (MinMlLet (MinMlVar var2) (MinMlDigit val2) expr) ) )
letAux (MinMlLet _ _ (MinMlBool bool)) = MinMlBool bool
letAux minml = minml
