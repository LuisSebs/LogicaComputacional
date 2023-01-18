
module Test where

import Test.QuickCheck

import MinMl.Lexer as LexMl
import MinMl.Parser as ParMl
import MinMl.Interpreter as IntMl
import MinMl.Type as TypeMl

import MinHs.Lexer as LexHs
import MinHs.Parser as ParHs
import MinHs.Compiler as CompHs
import MinHs.Interpreter as IntHs
import MinHs.Type as TypeHs

-- MinML

ml1 = "letrec fact = fun x => if ( x = 0) then 1 else x * ( fact (x -1) ) in ( fact 5 ) end"
ml2 = "letrec fibo = fun x => if ( x <= 1) then x else ( ( fibo (x -1) ) + ( fibo (x -2) ) ) in ( fibo 8 ) end"
ml3 = "letrec dblSumGaus = fun x => ( ( x *( x + 1))) in ( dblSumGaus 100 ) end"
ml4 = "letrec sumToNum = fun x => if ( x = 1) then x else ( x + ( sumToNum (x - 1) )) in ( sumToNum 100 ) end"
ml5 = "letrec lettt = fun x => if ( x < 10 ) then ( let y = x in ( y * 10) end) else ( x + ( lettt ( x - 1))) in ( lettt 12 ) end"
ml6 = "letrec decimales = fun x => if ( x < 10) then true else false in ( decimales 10 ) end"

ml7 = "fun x => if ( x = 0) then 1 else x * ( fact (x -1) )"
ml8 = "fun x => if ( x <= 1) then x else ( ( fibo (x -1) ) + ( fibo (x -2) ) )"
ml9 = "fun x => ( ( x *( x + 1))) "
ml10 = "fun x => if ( x = 1) then x else ( x + ( sumToNum (x - 1) )) "
ml11 = "fun x => if ( x < 10 ) then ( let y = x in ( y * 10) end) else ( x + ( lettt ( x - 1)))"
ml12 = "fun x => if ( x < 10) then true else false "

t1_ml :: Bool
t1_ml = TypeMl.infer (ParMl.parser $ LexMl.lexer ml1) == TypeMl.Nat

t2_ml :: Bool
t2_ml = TypeMl.infer (ParMl.parser $ LexMl.lexer ml2) == TypeMl.Nat

t3_ml :: Bool
t3_ml = TypeMl.infer (ParMl.parser $ LexMl.lexer ml3) == TypeMl.Nat

t4_ml :: Bool
t4_ml = TypeMl.infer (ParMl.parser $ LexMl.lexer ml4) == TypeMl.Nat

t5_ml :: Bool
t5_ml = TypeMl.infer (ParMl.parser $ LexMl.lexer ml5) == TypeMl.Nat

t6_ml :: Bool
t6_ml = TypeMl.infer (ParMl.parser $ LexMl.lexer ml6) == TypeMl.Boolean

t7_ml :: Bool
t7_ml = TypeMl.infer (ParMl.parser $ LexMl.lexer ml7) == TypeMl.Arrow TypeMl.Nat TypeMl.Nat

t8_ml :: Bool
t8_ml = TypeMl.infer (ParMl.parser $ LexMl.lexer ml8) == TypeMl.Arrow TypeMl.Nat TypeMl.Nat

t9_ml :: Bool
t9_ml = TypeMl.infer (ParMl.parser $ LexMl.lexer ml9) == TypeMl.Arrow TypeMl.Nat TypeMl.Nat

t10_ml :: Bool
t10_ml = TypeMl.infer (ParMl.parser $ LexMl.lexer ml10) == TypeMl.Arrow TypeMl.Nat TypeMl.Nat

t11_ml :: Bool
t11_ml = TypeMl.infer (ParMl.parser $ LexMl.lexer ml11) == TypeMl.Arrow TypeMl.Nat TypeMl.Nat

t12_ml :: Bool
t12_ml = TypeMl.infer (ParMl.parser $ LexMl.lexer ml12) == TypeMl.Arrow TypeMl.Nat TypeMl.Boolean

-- MinHS

hs1 = "(recfun fact :: (Nat -> Nat) x = if (x == 0) then 1 else x * (fact (x -1)) 5)"
hs2 = "(recfun factTail :: (Nat -> Nat -> Nat) x t = if (x == 0) then t else (factTail (x -1) (t*x)) 5 1)"
hs3 = "(recfun fibo :: (Nat -> Nat) x = if (x <= 1) then x else ( (fibo (x -1)) + (fibo (x -2)) ) 8)"
hs4 = "(recfun pot :: (Nat -> Nat -> Nat) x p = if (p == 0) then 1 else x * (pot (x) (p-1)) 5 3)"
hs5 = "(recfun letths :: (Nat -> Nat -> Nat) x z = if ( x < 10 ) then x + let y = x in ( y * 10) end else ( (letths (x-1) (z-1)) + let y = z in ( y * 10) end ) 12 3)"
hs6 = "(recfun dec :: (Nat -> Bool) x = if (x == 0) then true else false 5)"

hs7 = "(recfun fact :: (Nat -> Nat) x = if (x == 0) then 1 else x * (fact (x -1))  )" 
hs8 = "(recfun factTail :: (Nat -> Nat -> Nat) x t = if (x == 0) then t else (factTail (x -1) (t*x))  )" 
hs9 = "(recfun fibo :: (Nat -> Nat) x = if (x <= 1) then x else ( (fibo (x -1)) + (fibo (x -2)) )  )"
hs10 = "(recfun pot :: (Nat -> Nat -> Nat) x p = if (p == 0) then 1 else x * (pot (x) (p-1))  )"
hs11 = "(recfun letths :: (Nat -> Nat -> Nat) x z = if ( x < 10 ) then x + let y = x in ( y * 10) end else ( (letths (x-1) (z-1)) + let y = z in ( y * 10) end )  )"
hs12 = "(recfun dec :: (Nat -> Bool) x = if (x == 0) then true else false  )"

t1_hs :: Bool
t1_hs = TypeHs.verify (ParHs.parser $ LexHs.lexer hs1) [] == ParHs.Nat

t2_hs :: Bool
t2_hs = TypeHs.verify (ParHs.parser $ LexHs.lexer hs2) [] == ParHs.Nat

t3_hs :: Bool
t3_hs = TypeHs.verify (ParHs.parser $ LexHs.lexer hs3) [] == ParHs.Nat

t4_hs :: Bool
t4_hs = TypeHs.verify (ParHs.parser $ LexHs.lexer hs4) [] == ParHs.Nat

t5_hs :: Bool
t5_hs = TypeHs.verify (ParHs.parser $ LexHs.lexer hs5) [] == ParHs.Nat

t6_hs :: Bool
t6_hs = TypeHs.verify (ParHs.parser $ LexHs.lexer hs6) [] == ParHs.Boolean

t7_hs :: Bool
t7_hs = TypeHs.verify (ParHs.parser $ LexHs.lexer hs7) [] == ParHs.Arrow ParHs.Nat ParHs.Nat

t8_hs :: Bool
t8_hs = TypeHs.verify (ParHs.parser $ LexHs.lexer hs8) [] == ParHs.Arrow ParHs.Nat (ParHs.Arrow ParHs.Nat ParHs.Nat)

t9_hs :: Bool
t9_hs = TypeHs.verify (ParHs.parser $ LexHs.lexer hs9) [] == ParHs.Arrow ParHs.Nat ParHs.Nat

t10_hs :: Bool
t10_hs = TypeHs.verify (ParHs.parser $ LexHs.lexer hs10) [] == ParHs.Arrow ParHs.Nat (ParHs.Arrow ParHs.Nat ParHs.Nat)

t11_hs :: Bool
t11_hs = TypeHs.verify (ParHs.parser $ LexHs.lexer hs11) [] == ParHs.Arrow ParHs.Nat (ParHs.Arrow ParHs.Nat ParHs.Nat)

t12_hs :: Bool
t12_hs = TypeHs.verify (ParHs.parser $ LexHs.lexer hs12) [] == ParHs.Arrow ParHs.Nat ParHs.Boolean

main = do
    putStrLn "Test MinMl P1"
    quickCheck t1_ml
    quickCheck t2_ml
    quickCheck t3_ml
    quickCheck t4_ml
    quickCheck t5_ml
    quickCheck t6_ml
    putStrLn ""
    putStrLn "Test MinMl P2"
    quickCheck t7_ml
    quickCheck t8_ml
    quickCheck t9_ml
    quickCheck t10_ml
    quickCheck t11_ml
    quickCheck t12_ml
    putStrLn ""
    putStrLn "Test MinHs"
    quickCheck t1_hs
    quickCheck t2_hs
    quickCheck t3_hs
    quickCheck t4_hs
    quickCheck t5_hs
    quickCheck t6_hs
    putStrLn ""
    putStrLn "Test MinHs P2"
    quickCheck t7_hs 
    quickCheck t8_hs
    quickCheck t9_hs
    quickCheck t10_hs
    quickCheck t11_hs
    quickCheck t12_hs 