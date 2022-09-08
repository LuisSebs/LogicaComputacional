-- Creamos el lenguaje Prop
data Prop = VarProp String
          | T 
          | F 
          | Neg Prop
          | Imp Prop Prop
          | And Prop  Prop
          | Or Prop Prop
          | Syss Prop Prop
 deriving (Eq) -- Se elimina Show para instanciar nuestro propio Show

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

{- PARTE 1  -}
-- Regresa una lista con las sublistas de los elementos de la lista (Conjunto potencia)
subConj :: [Int] -> [[Int]]
subConj [] = [[]]
subConj (x:xs) = aux x (subConj xs) ++ (subConj xs)
 
 -- Funcion axiliar 
aux x [] = []
aux x (y:ys) = (x:y) : aux x ys 

-- Devuelve (sin repeticion) una lista con las variables en una fórmula
vars :: Prop -> [Prop]
vars T = [T]
vars F = [F]
vars (VarProp x) = [VarProp x] 
vars (Neg p) = varsAux(vars p)
vars (Imp p q) = varsAux((vars p) ++ (vars q))
vars (And p q) = varsAux((vars p) ++ (vars q))
vars (Or p q) = varsAux((vars p) ++ (vars q))
vars (Syss p q) = varsAux((vars p) ++ (vars q))

-- Funcion auxiliar que elimina las variables proposicionales repetidas en una lista
varsAux :: (Eq a) => [a] -> [a]
varsAux [] = []
varsAux (x:xs) = x : varsAux (filter (/= x) xs)

-- Definimos el tipo Estados
type Estado = [Prop]

{- PARTE 2  -}
-- Interpreta cierta o falsa una formula con ciertos estados dados
interpretacion :: Prop -> Estado -> Bool
interpretacion T x = True
interpretacion F x = False
interpretacion (VarProp p) x = iAux (VarProp p) x
interpretacion (Neg a) x = if (interpretacion a x) then False else True
interpretacion (And a b) x = if (((interpretacion a x) ==  True) && ((interpretacion b x) == True)) then True else False
interpretacion (Or a b) x =  if (((interpretacion a x) == False) && ((interpretacion b x) == False)) then False else True
interpretacion (Imp a b) x = if (((interpretacion a x) == True) && ((interpretacion b x) == False)) then False else True
interpretacion (Syss a b) x = if ((interpretacion a x) == (interpretacion b x)) then True else False

-- Funcion axiliar que encuentra una variable proposicional en una lista de estados
iAux :: Prop -> Estado -> Bool
iAux (VarProp p) [] = False
iAux (VarProp p) (x:xs) = if (VarProp p) == x
                           then True
                           else iAux (VarProp p) xs

{- La formula1 = f1 es
   (p v ¬ p) = True siempre sin importar los estados [x]
-}

{- La formula2 = f2 es
   (s ʌ ¬t) v ¬r = True con los estados [s] 
   (s ʌ ¬t) v ¬r = False con los estados [r] o [s,r,t]
-}

{- La formula3 = f3 es
   (r v p)<->(q ʌ p) = True con los estados [q] 
   (r v p)<->(q ʌ p) = False con los estados [r,p] 
-}

{- La formula4 = f4 es
   (¬(p v q) ʌ r) -> ((s ʌ ¬p) -> (r ʌ q)) = True con los estados [q,r,s] 
   (¬(p v q) ʌ r) -> ((s ʌ ¬p) -> (r ʌ q)) = False con los estados [r,s] 
-}

-- Funcion auxiliar que regresa el conjunto potencia de una lista de proposiciones (hace lo mismo que subConj)
subConjProp :: [Prop] -> [[Prop]]
subConjProp [] = [[]]
subConjProp (x:xs) = aux x (subConjProp xs) ++ (subConjProp xs)

-- Devuelve una lista con todos los posibles estados de una fórmula
estados :: Prop -> [[Prop]]
estados T = [[]]
estados F = [[]]
estados (VarProp p) =  subConjProp (vars (VarProp p))
estados (Neg p) = subConjProp(vars p)
estados (And p q) = subConjProp(vars(And p q))
estados (Or p q) = subConjProp(vars(Or p q))
estados (Imp p q) = subConjProp(vars(Imp p q))
estados (Syss p q) = subConjProp(vars(Syss p q)) 

{-extra-}
instance Show Prop where
   show (T) = "⊤"++" "
   show (F) = "⊥"++" "
   show (VarProp p) = p
   show (Neg a) = "¬"++show(a)
   show (And a b) = "(" ++ show(a) ++ " ∧ "  ++ show(b) ++ ")"
   show (Or a b) = "(" ++ show(a) ++ " ∨ "  ++ show(b) ++ ")"
   show (Imp a b) = "(" ++ show(a) ++ " -> " ++ show(b) ++ ")"
   show (Syss a b) = "(" ++ show(a) ++ " <-> " ++ show(b) ++ ")"

{- PARTE 3  -}
-- Funcion que elimina equivalencias
eliminaEquiv :: Prop -> Prop
eliminaEquiv T = T 
eliminaEquiv F = F 
eliminaEquiv (VarProp p) = (VarProp p)
eliminaEquiv (Neg a) = (Neg(eliminaEquiv a))
eliminaEquiv (And a b) = (And (eliminaEquiv a) (eliminaEquiv b))
eliminaEquiv (Or a b) = (Or (eliminaEquiv a) (eliminaEquiv b))
eliminaEquiv (Imp a b) = (Imp (eliminaEquiv a) (eliminaEquiv b))
eliminaEquiv (Syss a b) = (And (Imp (eliminaEquiv a) (eliminaEquiv b)) (Imp (eliminaEquiv b) (eliminaEquiv a)))

-- Elimina implicaciones en una formula
eliminaImp :: Prop -> Prop
eliminaImp T = T 
eliminaImp F = F 
eliminaImp (VarProp p) = (VarProp p)
eliminaImp (Neg a) = (Neg (eliminaImp a))
eliminaImp (And a b) = (And (eliminaImp a) (eliminaImp b))
eliminaImp (Or a b) = (Or (eliminaImp a) (eliminaImp b))
eliminaImp (Imp a b) = (Or (Neg (eliminaImp a)) (eliminaImp b))

-- Empuja las negaciones y elimina dobles negaciones
empujaNeg :: Prop -> Prop 
empujaNeg T = T 
empujaNeg F = F
empujaNeg (VarProp p) = (VarProp p)
empujaNeg (And a b) = (And (empujaNeg a)(empujaNeg b))
empujaNeg (Or a b) = (Or (empujaNeg a)(empujaNeg b))
empujaNeg (Neg a) = empujaNegAux(Neg a)
-- Funcion auxiliar que considera el casos espaciales de la funcion empujaNeg
empujaNegAux :: Prop -> Prop 
empujaNegAux (Neg T) = T 
empujaNegAux (Neg F) = F
empujaNegAux (Neg (VarProp p)) = (Neg (VarProp p))
empujaNegAux (Neg (And a b)) =(Or (empujaNeg(Neg a))(empujaNeg(Neg b)))
empujaNegAux (Neg (Or a b)) = (And (empujaNeg(Neg a))(empujaNeg(Neg b)))
empujaNegAux (Neg (Neg a)) = empujaNeg(a)

{-Parte 4-}
{-Funcion que determina si una formula es una literal o no-}
literal :: Prop -> Bool
literal T = True
literal F = True
literal (VarProp p) = True
literal (Neg T) = True 
literal (Neg F) = True
literal (Neg (VarProp p)) = True
literal (Neg a) = False -- Suponiendo que las negaciones solo figuran frente a atomos
literal (And a b) = False
literal (Or a b) = False
literal (Imp a b) = False
literal (Syss a b) = False

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
f7 = (Neg (And p (Or (Neg q) r)))

f8 :: Prop
f8 = (And p (Neg (Or q r)))

f9 :: Prop 
f9 = (Neg (Or p (Neg (And q r))))

f10 :: Prop 
f10 = (Or (Neg p) (Neg (Or (Neg q) r)))

f11 ::  Prop 
f11 = (Or (Neg p) (And (Neg (Neg q)) (Neg r)))

f12 :: Prop 
f12 = (Or (Neg (Or (Neg (Neg p)) q)) (Or (Neg (Neg r)) s))

f13 :: Prop 
f13 = (Or (Neg (Or p q)) (And p r))

f14 :: Prop 
f14 = (Or (Or (Neg p) (Neg q)) (And p r))

f15 :: Prop 
f15 = (Or (Or (Neg p) (Neg (Or q r))) (And p r))
