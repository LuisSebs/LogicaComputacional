module Ejemplos where
import Practica01 -- Importamos Practica01.hs

{-Formulas proposicionales-}
-- Variable proposicional p
p :: Prop
p = VarProp "p"
--Variable proposicional q
q :: Prop
q = VarProp "q"
-- Variable proposicional r
r :: Prop
r = VarProp "r"
-- Variable proposicional s
s :: Prop
s = VarProp "s"
-- Variable proposicional t
t :: Prop
t = VarProp "t"

{-FORMULAS PROPOSICIONALES-}
{-  Formula 1
   (p v ¬ p)-}
f1 :: Prop
f1 = Or p (Neg p)

{-  Formula 2
   (s ʌ ¬t) v ¬r -}
f2 :: Prop
f2 = Or (And s (Neg t)) (Neg r)

{-  Formula 3
   (r v p)<->(q ʌ p) -}
f3 :: Prop
f3 = Syss (Or r p) (And q p)

{-  Formula 4
   (¬(p v q) ʌ r) -> ((s ʌ ¬p) -> (r ʌ q))  -}
f4 :: Prop
f4 = Imp (And (Neg (Or p q)) r) (Imp (And s (Neg p)) (And r q))

{-  Formula 5
    ¬(((p v ¬q) -> (p ʌ r)) <-> (¬(q ʌ t) -> (s v ¬t))) -}
f5 :: Prop
f5 = Neg (Syss (Imp (Or p (Neg q)) (And p r)) (Imp (Neg(And q t)) (Or s (Neg t))))

{- Formula 6-}
f6 :: Prop 
f6 = (Syss (Imp p q) r)

f7 :: Prop 
f7 = (Imp (And (Or (Neg p)(q))(Or (Neg q) r))(Or (Neg p) r))

f8 :: Prop 
f8 = (Or (Syss r q)(Imp (Neg r) q))

f10 :: Prop 
f10 = (Or p (And q r))

f11 :: Prop 
f11 = (Or (And p q) r)

f12 :: Prop 
f12 = (And (Or p q) (Or r s))

f13 :: Prop 
f13 = (Or (And p q) (And r s))

f14 :: Prop 
f14 = (Or (And p (Neg q)) (And q (Neg p)))

f15 :: Prop 
f15 = (Or (And p q) (And q p))

f16 :: Prop 
f16 = (And (Or p q)(Or r s)) 

f17 :: Prop 
f17 = (Or (Or p q)(Or r s))

f18 :: Prop 
f18 = (Or (Neg (Syss p q))(Imp (Neg q) r))

{-Casos basicos que tiene que cumplir la funcion de distribucion-}
c1 :: Prop 
c1 = (Or p q) -- CHECK

c2 :: Prop 
c2 = (Or p (And q r)) -- CHECK

c3 :: Prop 
c3 = (Or p (Or q r)) -- CHECK

c4 :: Prop 
c4 = (Or (And p q ) r)-- CHECK

c5 :: Prop 
c5 = (Or (Or p q) r)-- CHECK

c6 :: Prop 
c6 = (Or (Or p q) (And r s)) -- CHECK

c7 :: Prop 
c7 = (Or (And p q) (Or r s)) -- CHECK

c8 :: Prop 
c8 = (Or (Or p q)(Or r s)) -- CHECK

c9 :: Prop 
c9 = (Or (And p q)(And r s)) -- CHECK

{-Formula proposicional que hicimos en clase con Javi-}
ultima :: Prop 
ultima = (Or (Neg (Syss p q)) (Imp (Neg q) r))

-- Ejemplos para 
f19 :: Prop
f19 = (And p q)

f20 :: Prop 
f20 = (Or p q)

f21 :: Prop 
f21 = (And (Or p q) (Or r s))

f22 :: Prop 
f22 = fnc ultima