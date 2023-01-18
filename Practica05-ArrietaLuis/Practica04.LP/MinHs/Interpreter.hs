module MinHs.Interpreter where

import MinHs.Compiler

eval :: MinHs -> Either Int Bool
eval curriedFun = evalAux(letAux(curriedFun))

evalAux :: MinHs -> Either Int Bool
evalAux (MinHsBool bool) = Right bool
evalAux (MinHsDigit digit) = Left digit
evalAux (MinHsAdd x y) = Left (minHsToInt(opMinHs (x)) + minHsToInt(opMinHs (y)))
evalAux (MinHsMul x y) = Left (minHsToInt(opMinHs (x)) * minHsToInt(opMinHs (y)))
evalAux (MinHsSub x y) = Left (minHsToInt(opMinHs (x)) * minHsToInt(opMinHs (y)))
evalAux (MinHsEq p q) = Right (auxProp (MinHsEq p q))
evalAux (MinHsGt p q) = Right (auxProp (MinHsGt (MinHsDigit (auxOp p)) (MinHsDigit (auxOp q))))
evalAux (MinHsLt p q) = Right (auxProp (MinHsLt (MinHsDigit (auxOp p)) (MinHsDigit (auxOp q))))
evalAux (MinHsGtE p q) = Right (auxProp (MinHsGtE (MinHsDigit (auxOp p)) (MinHsDigit (auxOp q))))
evalAux (MinHsLtE p q) = Right (auxProp (MinHsLtE (MinHsDigit (auxOp p)) (MinHsDigit (auxOp q))))
evalAux (MinHsAnd p q) = Right (auxProp (MinHsAnd p q))
evalAux (MinHsOr p q) = Right (auxProp (MinHsOr p q))
evalAux (MinHsLet var val expr) = Left (minHsToInt(auxLet(MinHsLet var val expr)))
evalAux (MinHsIf cond ent sino) = Left (auxIf (MinHsIf cond ent sino))
evalAux (MinHsRecFun  func typeFunc var expr param) = recFunRes (recFun (MinHsRecFun func typeFunc var expr param)) 
evalAux (MinHsRecFun2 func typeFunc var1 var2 expr param1 param2) = Left (minHsToInt (recFun (MinHsRecFun2 func typeFunc var1 var2 expr param1 param2)))

recFunRes :: MinHs -> Either Int Bool
recFunRes (MinHsDigit digit) = Left digit
recFunRes (MinHsBool bool) = Right bool


recFun :: MinHs -> MinHs 
recFun (MinHsRecFun (MinHsFun func) (typeFunc) (MinHsVar var) (MinHsIf cond ent sino) param) = recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) (MinHsVar var) (MinHsIf cond ent sino) param) (letAux (MinHsLet (MinHsVar var) (param) (MinHsIf cond ent sino)))
recFun (MinHsRecFun2 (MinHsFun func) (typeFunc) (MinHsVar var1) (MinHsVar var2) (MinHsIf cond ent sino) (param1) (param2)) = recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) (MinHsVar var1) (MinHsVar var2) (MinHsIf cond ent sino) (param1) (param2)) (letAux (MinHsLet (MinHsVar var1) (param1) (MinHsLet (MinHsVar var2) (param2) (MinHsIf cond ent sino) )))



recFunAux :: MinHs -> MinHs -> MinHs
recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (MinHsDigit digit1)  = MinHsDigit digit1
recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (MinHsFunc2 (MinHsFun fun1) paramN1 paramN2) = if (func == fun1) == True
                                                                                                                                    then recFun (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op (MinHsDigit (auxOp(paramN1))) (MinHsDigit (auxOp(paramN2))))
                                                                                                                                    else error "Error de recursion"
recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (MinHsMul x y) = opMinHs (MinHsMul x y)
recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (MinHsIf (MinHsEq (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsFunc2 (MinHsFun fun1) paramN1 paramN2)) = if (auxProp (MinHsEq (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                                                                        then ent
                                                                                                                                                                        else recFun (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op (MinHsDigit (auxOp(paramN1))) (MinHsDigit (auxOp(paramN2))))
recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (MinHsIf (MinHsEq (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsMul (x) (y))) = if (auxProp (MinHsEq (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                                             then ent
                                                                                                                                             else MinHsDigit (auxOp (MinHsMul (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (x)) (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (y) ) ))
recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (MinHsIf (MinHsEq (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsAdd (x) (y))) = if (auxProp (MinHsEq (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                                             then ent
                                                                                                                                             else MinHsDigit (auxOp (MinHsAdd (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (x)) (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (y) ) ))
recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (MinHsIf (MinHsEq (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsSub (x) (y))) = if (auxProp (MinHsEq (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                                             then ent
                                                                                                                                             else MinHsDigit (auxOp (MinHsSub (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (x)) (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (y) ) ))
recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (MinHsIf (MinHsGt (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsFunc2 (MinHsFun fun1) paramN1 paramN2)) = if (auxProp (MinHsGt (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                                                                        then ent
                                                                                                                                                                        else recFun (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op (MinHsDigit (auxOp(paramN1))) (MinHsDigit (auxOp(paramN2))))
recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (MinHsIf (MinHsGt (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsMul (x) (y))) = if (auxProp (MinHsGt (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                                             then ent
                                                                                                                                             else MinHsDigit (auxOp (MinHsMul (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (x)) (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (y) ) ))
recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (MinHsIf (MinHsGt (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsAdd (x) (y))) = if (auxProp (MinHsGt (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                                             then ent
                                                                                                                                             else MinHsDigit (auxOp (MinHsAdd (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (x)) (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (y) ) ))
recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (MinHsIf (MinHsGt (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsSub (x) (y))) = if (auxProp (MinHsGt (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                                             then ent
                                                                                                                                             else MinHsDigit (auxOp (MinHsSub (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (x)) (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (y) ) ))
recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (MinHsIf (MinHsLt (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsFunc2 (MinHsFun fun1) paramN1 paramN2)) = if (auxProp (MinHsLt (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                                                                        then ent
                                                                                                                                                                        else recFun (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op (MinHsDigit (auxOp(paramN1))) (MinHsDigit (auxOp(paramN2))))
recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (MinHsIf (MinHsLt (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsMul (x) (y))) = if (auxProp (MinHsLt (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                                             then ent
                                                                                                                                             else MinHsDigit (auxOp (MinHsMul (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (x)) (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (y) ) ))
recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (MinHsIf (MinHsLt (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsAdd (x) (y))) = if (auxProp (MinHsLt (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                                            then ent
                                                                                                                                            else MinHsDigit (auxOp (MinHsAdd (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (x)) (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (y) ) ))
recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (MinHsIf (MinHsLt (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsSub (x) (y))) = if (auxProp (MinHsLt (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                                             then ent
                                                                                                                                             else MinHsDigit (auxOp (MinHsSub (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (x)) (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (y) ) ))
recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (MinHsIf (MinHsGtE (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsFunc2 (MinHsFun fun1) paramN1 paramN2)) = if (auxProp (MinHsGtE (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                                                                        then ent
                                                                                                                                                                        else recFun (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op (MinHsDigit (auxOp(paramN1))) (MinHsDigit (auxOp(paramN2))))                                                                                                                                             
recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (MinHsIf (MinHsGtE (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsMul (x) (y))) = if (auxProp (MinHsGtE (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                                             then ent
                                                                                                                                             else MinHsDigit (auxOp (MinHsMul (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (x)) (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (y) ) ))
recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (MinHsIf (MinHsGtE (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsAdd (x) (y))) = if (auxProp (MinHsGtE (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                                             then ent
                                                                                                                                             else MinHsDigit (auxOp (MinHsAdd (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (x)) (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (y) ) ))
recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (MinHsIf (MinHsGtE (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsSub (x) (y))) = if (auxProp (MinHsGtE (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                                             then ent
                                                                                                                                             else MinHsDigit (auxOp (MinHsSub (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (x)) (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (y) ) ))
recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (MinHsIf (MinHsLtE (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsFunc2 (MinHsFun fun1) paramN1 paramN2)) = if (auxProp (MinHsLtE (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                                                                        then ent
                                                                                                                                                                        else recFun (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op (MinHsDigit (auxOp(paramN1))) (MinHsDigit (auxOp(paramN2))))
recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (MinHsIf (MinHsLtE (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsMul (x) (y))) = if (auxProp (MinHsLtE (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                                             then ent
                                                                                                                                             else MinHsDigit (auxOp (MinHsMul (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (x)) (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (y) ) ))
recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (MinHsIf (MinHsLtE (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsAdd (x) (y))) = if (auxProp (MinHsLtE (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                                             then ent
                                                                                                                                             else MinHsDigit (auxOp (MinHsAdd (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (x)) (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (y) ) ))
recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (MinHsIf (MinHsLtE (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsSub (x) (y))) = if (auxProp (MinHsLtE (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                                             then ent
                                                                                                                                             else MinHsDigit (auxOp (MinHsSub (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (x)) (recFunAux (MinHsRecFun2 (MinHsFun func) (typeFunc) var1 var2 op param1 param2) (y) ) ))

recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (MinHsDigit digit1)  = MinHsDigit digit1
recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (MinHsBool bool)  = MinHsBool bool
recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (MinHsFunc (MinHsFun fun1) op1) = if (func == fun1) == True
                                                                                                        then recFun (MinHsRecFun (MinHsFun func) (typeFunc) var op (MinHsDigit (auxOp(op1))))
                                                                                                        else error "Error de recursion"
                                                                                             
recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (MinHsIf (MinHsEq (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsMul (x) (y)))  = if (auxProp (MinHsEq (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                                                  then ent
                                                                                                                                                  else MinHsDigit (auxOp (MinHsMul (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (x))(recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (y))))
recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (MinHsIf (MinHsEq (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsAdd (x) (y)))  = if (auxProp (MinHsEq (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                                                  then ent
                                                                                                                                                  else MinHsDigit (auxOp (MinHsAdd (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (x)) (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (y))))
recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (MinHsIf (MinHsEq (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsSub (x) (y)))  = if (auxProp (MinHsEq (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                                                  then ent
                                                                                                                                                  else MinHsDigit (auxOp (MinHsSub (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (x)) (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (y)))) 
recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (MinHsIf (MinHsEq (MinHsDigit c1) (MinHsDigit c2)) thenIf elseIf) = if (auxProp (MinHsEq (MinHsDigit c1) (MinHsDigit c2)) == True)  
                                                                                                                                        then thenIf
                                                                                                                                        else elseIf
recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (MinHsIf (MinHsGt (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsMul (x) (y)))  = if (auxProp (MinHsGt (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                              then ent
                                                                                                                              else MinHsDigit (auxOp (MinHsMul (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (x)) (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (y))))
recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (MinHsIf (MinHsGt (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsAdd (x) (y)))  = if (auxProp (MinHsGt (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                              then ent
                                                                                                                              else MinHsDigit (auxOp (MinHsAdd (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (x)) (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (y))))
recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (MinHsIf (MinHsGt (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsSub (x) (y)))  = if (auxProp (MinHsGt (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                              then ent
                                                                                                                              else MinHsDigit (auxOp (MinHsSub (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (x)) (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (y))))
recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (MinHsIf (MinHsLt (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsMul (x) (y)))  = if (auxProp (MinHsLt (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                              then ent
                                                                                                                              else MinHsDigit (auxOp (MinHsMul (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (x)) (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (y))))
recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (MinHsIf (MinHsLt (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsAdd (x) (y)))  = if (auxProp (MinHsLt (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                              then ent
                                                                                                                              else MinHsDigit (auxOp (MinHsAdd (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (x)) (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (y))))
recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (MinHsIf (MinHsLt (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsSub (x) (y)))  = if (auxProp (MinHsLt (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                              then ent
                                                                                                                              else MinHsDigit (auxOp (MinHsSub (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (x)) (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (y))))
recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (MinHsIf (MinHsGtE (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsMul (x) (y)))  = if (auxProp (MinHsGtE (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                              then ent
                                                                                                                              else MinHsDigit (auxOp (MinHsMul (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (x)) (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (y))))
recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (MinHsIf (MinHsGtE (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsAdd (x) (y)))  = if (auxProp (MinHsGtE (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                              then ent
                                                                                                                              else MinHsDigit (auxOp (MinHsAdd (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (x)) (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (y))))
recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (MinHsIf (MinHsGtE (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsSub (x) (y)))  = if (auxProp (MinHsGtE (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                              then ent
                                                                                                                              else MinHsDigit (auxOp (MinHsSub (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (x)) (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (y))))
recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (MinHsIf (MinHsLtE (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsMul (x) (y)))  = if (auxProp (MinHsLtE (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                              then ent
                                                                                                                              else MinHsDigit (auxOp (MinHsMul (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (x)) (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (y))))
recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (MinHsIf (MinHsLtE (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsAdd (x) (y)))  = if (auxProp (MinHsLtE (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                              then ent
                                                                                                                              else MinHsDigit (auxOp (MinHsAdd (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (x)) (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (y))))
recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (MinHsIf (MinHsLtE (MinHsDigit c1) (MinHsDigit c2)) ent (MinHsSub (x) (y)))  = if (auxProp (MinHsLtE (MinHsDigit c1) (MinHsDigit c2)) == True)
                                                                                                                              then ent
                                                                                                                              else MinHsDigit (auxOp (MinHsSub (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (x)) (recFunAux (MinHsRecFun (MinHsFun func) (typeFunc) var op param) (y))))  

letAux :: MinHs -> MinHs
letAux (MinHsLet (MinHsVar var) (val) (MinHsDigit digit)) = MinHsDigit digit
letAux (MinHsLet (MinHsVar var1) (val) (MinHsVar var2)) = if (var1 == var2) == True
                                                                    then val 
                                                                    else (MinHsVar var2)
letAux (MinHsLet (MinHsVar var) (val) (MinHsFunc (MinHsFun fun) (MinHsMul (x) (y)))) = MinHsFunc (MinHsFun fun) (MinHsMul (letAux (MinHsLet (MinHsVar var) (val) (x))) (letAux (MinHsLet (MinHsVar var) (val) (y))))                                  
letAux (MinHsLet (MinHsVar var) (val) (MinHsFunc (MinHsFun fun) (MinHsSub (x) (y)))) = MinHsFunc (MinHsFun fun) (MinHsSub (letAux (MinHsLet (MinHsVar var) (val) (x))) (letAux (MinHsLet (MinHsVar var) (val) (y))))
letAux (MinHsLet (MinHsVar var) (val) (MinHsFunc (MinHsFun fun) (MinHsAdd (x) (y)))) = MinHsFunc (MinHsFun fun) (MinHsAdd (letAux (MinHsLet (MinHsVar var) (val) (x))) (letAux (MinHsLet (MinHsVar var) (val) (y))))
letAux (MinHsLet (MinHsVar var) (val) (MinHsEq x y)) = MinHsEq (letAux (MinHsLet (MinHsVar var) (val) (x))) (letAux (MinHsLet (MinHsVar var) (val) (y)))
letAux (MinHsLet (MinHsVar var) (val) (MinHsGt x y)) = MinHsGt (letAux (MinHsLet (MinHsVar var) (val) (x))) (letAux (MinHsLet (MinHsVar var) (val) (y)))
letAux (MinHsLet (MinHsVar var) (val) (MinHsLt x y)) = MinHsLt (letAux (MinHsLet (MinHsVar var) (val) (x))) (letAux (MinHsLet (MinHsVar var) (val) (y)))
letAux (MinHsLet (MinHsVar var) (val) (MinHsGtE x y)) = MinHsGtE (letAux (MinHsLet (MinHsVar var) (val) (x))) (letAux (MinHsLet (MinHsVar var) (val) (y)))
letAux (MinHsLet (MinHsVar var) (val) (MinHsLtE x y)) = MinHsLtE (letAux (MinHsLet (MinHsVar var) (val) (x))) (letAux (MinHsLet (MinHsVar var) (val) (y)))
letAux (MinHsLet (MinHsVar var) (val) (MinHsIf (cond) (thenIf) (elseIf))) = MinHsIf (letAux (MinHsLet (MinHsVar var) (val) (cond))) (letAux (MinHsLet (MinHsVar var) (val) (thenIf))) (letAux (MinHsLet (MinHsVar var) (val) (elseIf)))
letAux (MinHsLet (MinHsVar var) (val) (MinHsMul x y)) = MinHsMul (letAux (MinHsLet (MinHsVar var) (val) (x))) (letAux (MinHsLet (MinHsVar var) (val) (y)))
letAux (MinHsLet (MinHsVar var) (val) (MinHsAdd x y)) = MinHsAdd (letAux (MinHsLet (MinHsVar var) (val) (x))) (letAux (MinHsLet (MinHsVar var) (val) (y)))
letAux (MinHsLet (MinHsVar var) (val) (MinHsSub x y)) = MinHsSub (letAux (MinHsLet (MinHsVar var) (val) (x))) (letAux (MinHsLet (MinHsVar var) (val) (y)))
letAux (MinHsLet (MinHsVar var) (val) (MinHsLet (MinHsVar var2) (val2) (expr))) = letAux (MinHsLet (MinHsVar var) (val) (letAux (MinHsLet (MinHsVar var2) (val2) expr) ) )
letAux (MinHsLet (MinHsVar var) (val) (MinHsLet (MinHsVar var2) (MinHsVar varAsign) (expr))) = if ((varAsign == var) == True)
                                                                                                    then  letAux (MinHsLet (MinHsVar var) (val) (letAux(MinHsLet (MinHsVar var2) (val) (expr))))
                                                                                                    else (MinHsLet (MinHsVar var2) (MinHsVar varAsign) (expr))
letAux (MinHsLet (MinHsVar var) (val) (MinHsFunc2 (MinHsFun fun) (param1) (MinHsMul (x) (y)))) = MinHsFunc2 (MinHsFun fun) (letAux (MinHsLet (MinHsVar var) (val) (param1))) (MinHsMul (letAux (MinHsLet (MinHsVar var) (val) (x))) (letAux (MinHsLet (MinHsVar var) (val) (y))))                                  
letAux (MinHsLet (MinHsVar var) (val) (MinHsFunc2 (MinHsFun fun) (param1) (MinHsSub (x) (y)))) = MinHsFunc2 (MinHsFun fun) (letAux (MinHsLet (MinHsVar var) (val) (param1))) (MinHsSub (letAux (MinHsLet (MinHsVar var) (val) (x))) (letAux (MinHsLet (MinHsVar var) (val) (y))))
letAux (MinHsLet (MinHsVar var) (val) (MinHsFunc2 (MinHsFun fun) (param1) (MinHsAdd (x) (y)))) = MinHsFunc2 (MinHsFun fun) (letAux (MinHsLet (MinHsVar var) (val) (param1))) (MinHsAdd (letAux (MinHsLet (MinHsVar var) (val) (x))) (letAux (MinHsLet (MinHsVar var) (val) (y))))
letAux (MinHsLet (MinHsVar var) (val) (MinHsRecFun (MinHsFun func) (typeFunc) (MinHsVar varRec) expr param)) = (MinHsRecFun (MinHsFun func) (typeFunc) (MinHsVar varRec) expr param)
letAux (MinHsLet (MinHsVar var) (val) (MinHsRecFun2 (MinHsFun func) (typeFunc) (MinHsVar varRec1) (MinHsVar varRec2) (expr) (param1) (param2))) = MinHsRecFun2 (MinHsFun func) (typeFunc) (MinHsVar varRec1) (MinHsVar varRec2) (expr) (param1) (param2)
letAux (MinHsLet (MinHsVar var) (val) (MinHsBool bool)) = (MinHsBool bool)
letAux expr = expr

minHsToInt :: MinHs -> Int
minHsToInt (MinHsDigit digit) = digit
minHsToInt _ = -1

auxLet :: MinHs -> MinHs
auxLet (MinHsLet (MinHsVar var) (MinHsDigit val) (MinHsDigit digit)) = MinHsDigit digit
auxLet (MinHsLet (MinHsVar var1) (MinHsDigit val) (MinHsVar var2)) = if (var1 == var2) == True
                                                        then MinHsDigit val 
                                                        else (MinHsVar var2)
auxLet (MinHsLet (MinHsVar var) (MinHsDigit val) (MinHsAdd x y)) = opMinHs (MinHsAdd (auxLet (MinHsLet (MinHsVar var) (MinHsDigit val) x)) (auxLet (MinHsLet (MinHsVar var) (MinHsDigit val) y)))
auxLet (MinHsLet (MinHsVar var) (MinHsDigit val) (MinHsMul x y)) = opMinHs (MinHsMul (auxLet (MinHsLet (MinHsVar var) (MinHsDigit val) x)) (auxLet (MinHsLet (MinHsVar var) (MinHsDigit val) y)))
auxLet (MinHsLet (MinHsVar var) (MinHsDigit val) (MinHsSub x y)) = opMinHs (MinHsSub (auxLet (MinHsLet (MinHsVar var) (MinHsDigit val) x)) (auxLet (MinHsLet (MinHsVar var) (MinHsDigit val) y)))


auxLet (MinHsLet (MinHsVar var) (MinHsDigit val) (MinHsLet (MinHsVar var2) (MinHsDigit val2) expr)) = auxLet (MinHsLet (MinHsVar var) (MinHsDigit val) (auxLet(MinHsLet (MinHsVar var2) (MinHsDigit val2) expr)))

auxLet (MinHsLet (MinHsVar var) (MinHsDigit val) (MinHsLet (MinHsVar var2) (MinHsVar varAsign) expr)) = if (varAsign == var) == True 
                                                                                                then  auxLet (MinHsLet (MinHsVar var) (MinHsDigit val) (auxLet(MinHsLet (MinHsVar var2) (MinHsDigit val) expr)))
                                                                                                else error "La variable que estás asignando no está declarada"

auxLet (MinHsLet (MinHsVar var) (MinHsDigit val) (MinHsLet (MinHsVar var2) (MinHsAdd (MinHsVar varAsign) (x)) expr)) = if (varAsign == var) == True 
                                                                                                            then auxLet (MinHsLet (MinHsVar var) (MinHsDigit val) (auxLet(MinHsLet (MinHsVar var2) (MinHsAdd (MinHsDigit val) (x)) expr )))
                                                                                                            else error "La variable que estás asignando no está declarada"

auxLet (MinHsLet (MinHsVar var) (MinHsMul (x1) (x2)) op) = auxLet (MinHsLet (MinHsVar var) (opMinHs (MinHsMul (x1) (x2))) (op))
auxLet (MinHsLet (MinHsVar var) (MinHsAdd (x1) (x2)) op) = auxLet (MinHsLet (MinHsVar var) (opMinHs (MinHsAdd (x1) (x2))) (op))
auxLet (MinHsLet (MinHsVar var) (MinHsLet (MinHsVar var2) asign op2) op) = auxLet (MinHsLet (MinHsVar var) (auxLet(MinHsLet (MinHsVar var2) (asign) (op2))) (op))


opMinHs :: MinHs -> MinHs 
opMinHs (MinHsMul (MinHsVar var) m2) = MinHsMul (MinHsVar var) (m2)
opMinHs (MinHsMul m1 (MinHsVar var)) = MinHsMul (MinHsVar var) (m1)
opMinHs expr = MinHsDigit (auxOp expr)

auxOp :: MinHs -> Int
auxOp (MinHsDigit digit) =  digit
auxOp (MinHsAdd s1 s2) = auxOp (s1) + auxOp (s2)
auxOp (MinHsMul m1 m2) = auxOp (m1) * auxOp (m2)
auxOp (MinHsSub r1 r2) = auxOp (r1) - auxOp (r2)
auxOp (MinHsLet var val inlet) = minHsToInt(auxLet (MinHsLet var val inlet))
auxOp _ = error "No hay con que evalAuxuar"

auxProp :: MinHs -> Bool
auxProp (MinHsBool bool) = bool
auxProp (MinHsAnd p1 p2) = auxProp (p1) && auxProp (p2)
auxProp (MinHsOr p1 p2) = auxProp (p1) || auxProp (p2)
auxProp (MinHsEq (MinHsDigit p1) (MinHsDigit p2)) = p1 == p2
auxProp (MinHsGt (MinHsDigit p1) (MinHsDigit p2)) = p1 > p2
auxProp (MinHsLt (MinHsDigit p1) (MinHsDigit p2)) = p1 < p2
auxProp (MinHsGtE (MinHsDigit p1) (MinHsDigit p2)) = p1 >= p2
auxProp (MinHsLtE (MinHsDigit p1) (MinHsDigit p2)) = p1 <= p2


auxIf :: MinHs -> Int 
auxIf (MinHsIf (MinHsEq p q) thenIf elseIf) = let tIf = auxOp(thenIf); eIf = auxOp(elseIf) in if (auxProp(MinHsEq p q))
                                                                                                then tIf
                                                                                                else eIf
auxIf (MinHsIf (MinHsGt p q) thenIf elseIf) = let tIf = auxOp(thenIf); eIf = auxOp(elseIf) in if (auxProp(MinHsGt p q))
                                                                                                then tIf
                                                                                                else eIf
auxIf (MinHsIf (MinHsLt p q) thenIf elseIf) = let tIf = auxOp(thenIf); eIf = auxOp(elseIf) in if (auxProp(MinHsLt p q))
                                                                                                then tIf
                                                                                                else eIf
auxIf (MinHsIf (MinHsGtE p q) thenIf elseIf) = let tIf = auxOp(thenIf); eIf = auxOp(elseIf) in if (auxProp(MinHsGtE p q))
                                                                                                then tIf
                                                                                                else eIf
auxIf (MinHsIf (MinHsLtE p q) thenIf elseIf) = let tIf = auxOp(thenIf); eIf = auxOp(elseIf) in if (auxProp(MinHsLtE p q))
                                                                                                then tIf
                                                                                                else eIf
