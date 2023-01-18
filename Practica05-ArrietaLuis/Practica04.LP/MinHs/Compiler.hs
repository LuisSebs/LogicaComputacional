module MinHs.Compiler where

import MinHs.Parser as ParHs
import MinHs.Lexer as LexHs
data MinHs 
   	  = MinHsVar Var
      | MinHsFun Fun
      | MinHsBool Boolean
      | MinHsDigit Digit
      | MinHsAdd MinHs MinHs
      | MinHsSub MinHs MinHs
      | MinHsMul MinHs MinHs
      | MinHsEq MinHs MinHs
      | MinHsGt MinHs MinHs
      | MinHsLt MinHs MinHs
      | MinHsGtE MinHs MinHs
      | MinHsLtE MinHs MinHs
      | MinHsAnd MinHs MinHs
      | MinHsOr MinHs MinHs
      | MinHsLet MinHs MinHs MinHs 
      | MinHsIf MinHs MinHs MinHs
      | MinHsRecFun MinHs MinHs MinHs MinHs MinHs
      | MinHsFunc MinHs MinHs
      | MinHsRecFun2 MinHs MinHs MinHs MinHs MinHs MinHs MinHs
      | MinHsFunc2 MinHs MinHs MinHs
      | MinHsVoid
      | MinTypeNat 
      | MinTypeBool
      | MinTypeArrow MinHs MinHs
      deriving(Show,Eq) 



curry :: Expr -> MinHs
curry expr = auxCurry expr

auxCurry :: Expr -> MinHs
auxCurry (ExprVar var) = MinHsVar var 
auxCurry (ExprFun fun) = MinHsFun fun
auxCurry (ExprBool boolean) = MinHsBool boolean
auxCurry (ExprDigit digit) = MinHsDigit digit
auxCurry (ExprAdd x y) = MinHsLet (MinHsVar 'x') (exprToMinHs(x)) (MinHsLet (MinHsVar 'y') (exprToMinHs(y)) (MinHsAdd (MinHsVar 'x') (MinHsVar 'y')))
auxCurry (ExprSub x y) = MinHsLet (MinHsVar 'x') (exprToMinHs(x)) (MinHsLet (MinHsVar 'y') (exprToMinHs(y)) (MinHsSub (MinHsVar 'x') (MinHsVar 'y')))
auxCurry (ExprMul x y) = MinHsLet (MinHsVar 'x') (exprToMinHs(x)) (MinHsLet (MinHsVar 'y') (exprToMinHs(y)) (MinHsMul (MinHsVar 'x') (MinHsVar 'y')))
auxCurry (ExprEq p q) = MinHsLet (MinHsVar 'x') (exprToMinHs(p)) (MinHsLet (MinHsVar 'y') (exprToMinHs(q)) (MinHsEq (MinHsVar 'x') (MinHsVar 'y')))
auxCurry (ExprGt p q) = MinHsLet (MinHsVar 'x') (exprToMinHs(p)) (MinHsLet (MinHsVar 'y') (exprToMinHs(q)) (MinHsGt (MinHsVar 'x') (MinHsVar 'y')))
auxCurry (ExprLt p q) = MinHsLet (MinHsVar 'x') (exprToMinHs(p)) (MinHsLet (MinHsVar 'y') (exprToMinHs(q)) (MinHsLt (MinHsVar 'x') (MinHsVar 'y')))
auxCurry (ExprGtE p q) = MinHsLet (MinHsVar 'x') (exprToMinHs(p)) (MinHsLet (MinHsVar 'y') (exprToMinHs(q)) (MinHsGtE (MinHsVar 'x') (MinHsVar 'y')))
auxCurry (ExprLtE p q) = MinHsLet (MinHsVar 'x') (exprToMinHs(p)) (MinHsLet (MinHsVar 'y') (exprToMinHs(q)) (MinHsLtE (MinHsVar 'x') (MinHsVar 'y')))
auxCurry (ExprAnd p q) = MinHsLet (MinHsVar 'x') (exprToMinHs(p)) (MinHsLet (MinHsVar 'y') (exprToMinHs(q)) (MinHsAnd (MinHsVar 'x') (MinHsVar 'y')))
auxCurry (ExprOr p q) = MinHsLet (MinHsVar 'x') (exprToMinHs(p)) (MinHsLet (MinHsVar 'y') (exprToMinHs(q)) (MinHsOr (MinHsVar 'x') (MinHsVar 'y')))
auxCurry (ExprLet var val expr) = MinHsLet (exprToMinHs(var)) (exprToMinHs(val)) (auxCurry expr)
auxCurry (ExprIf cond thenIf elseIf) = MinHsLet (MinHsVar 'x') (MinHsVar 'x') (exprToMinHs(ExprIf cond thenIf elseIf))
auxCurry (ExprRecFun (ExprFun func) (typeExpr) (ExprVar var) (ExprIf cond ent sino) param) = MinHsLet (MinHsVar var) (exprToMinHs(param)) (MinHsRecFun (MinHsFun func) (exprToMinHs(typeExpr)) (MinHsVar var) (exprToMinHs(ExprIf cond ent sino)) (exprToMinHs(param))) 
auxCurry (ExprRecFun2 (ExprFun func) (typeExpr) (ExprVar var1) (ExprVar var2) (ExprIf cond ent sino) param1 param2) = MinHsLet (MinHsVar var1) (exprToMinHs(param1)) (MinHsLet (MinHsVar var2) (exprToMinHs(param2)) (MinHsRecFun2 (MinHsFun func) (exprToMinHs(typeExpr)) (MinHsVar var1) (MinHsVar var2) (exprToMinHs(ExprIf cond ent sino)) (exprToMinHs(param1)) (exprToMinHs(param2)) )) 

exprToMinHs :: Expr -> MinHs
exprToMinHs (ExprVar var) = MinHsVar var
exprToMinHs (ExprFun fun) = MinHsFun fun
exprToMinHs (ExprBool boolean) = MinHsBool boolean
exprToMinHs (ExprDigit digit) = MinHsDigit digit
exprToMinHs (ExprAdd x y) = MinHsAdd (exprToMinHs x) (exprToMinHs y)
exprToMinHs (ExprSub x y) = MinHsSub (exprToMinHs x) (exprToMinHs y)
exprToMinHs (ExprMul x y) = MinHsMul (exprToMinHs x) (exprToMinHs y)
exprToMinHs (ExprEq p q) = MinHsEq (exprToMinHs p) (exprToMinHs q)
exprToMinHs (ExprGt p q) = MinHsGt (exprToMinHs p) (exprToMinHs q)
exprToMinHs (ExprLt p q) = MinHsLt (exprToMinHs p) (exprToMinHs q)
exprToMinHs (ExprGtE p q) = MinHsGtE (exprToMinHs p) (exprToMinHs q)
exprToMinHs (ExprLtE p q) = MinHsLtE (exprToMinHs p) (exprToMinHs q)
exprToMinHs (ExprAnd p q) = MinHsAnd (exprToMinHs p) (exprToMinHs q)
exprToMinHs (ExprOr p q) = MinHsOr (exprToMinHs p) (exprToMinHs q)
exprToMinHs (ExprIf cond thenIf elseIf) = MinHsIf (exprToMinHs(cond)) (exprToMinHs(thenIf)) (exprToMinHs(elseIf))
exprToMinHs (ExprFunc fun expr) = MinHsFunc (exprToMinHs(fun)) (exprToMinHs(expr))
exprToMinHs (ExprFunc2 func var expr) = MinHsFunc2 (exprToMinHs(func)) (exprToMinHs(var)) (exprToMinHs(expr))
exprToMinHs (ExprLet var val expr) = MinHsLet (exprToMinHs(var)) (exprToMinHs(val)) (exprToMinHs(expr))
exprToMinHs (LexHs.Nat) = MinTypeNat
exprToMinHs (LexHs.Bool) = MinTypeBool
exprToMinHs (LexHs.Arrow type1 type2) = (MinTypeArrow (exprToMinHs(type1)) (exprToMinHs(type2)))
