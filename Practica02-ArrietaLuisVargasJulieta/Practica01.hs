module Practica01 where

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

{-Podemos probar todas las funciones con los ejemeplos 
del archivo Ejemplos.hs-}

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
   show (T) = "T"++" "
   show (F) = "F"++" "
   show (VarProp p) = p
   show (Neg a) = "¬"++show(a)
   show (And a b) = "(" ++ show(a) ++ " ^ "  ++ show(b) ++ ")"
   show (Or a b) = "(" ++ show(a) ++ " v "  ++ show(b) ++ ")"
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

-- Forma normal negativa
fnn :: Prop -> Prop 
fnn a = empujaNeg((eliminaImp(eliminaEquiv(a))))

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

{-Forma normal conjuntiva-}
fnc :: Prop -> Prop
fnc a = fncAux(fnn a)
-- Funcion auxiliar de la fnc
fncAux :: Prop -> Prop
fncAux T = T 
fncAux F = F 
fncAux (VarProp p) = (VarProp p)
fncAux (Neg p) = (Neg p) -- Suponiendo que las negaciones figuran frente a atomos
fncAux (And a b) = (And (fnc(a))(fnc(b)))
fncAux (Or a b) = (distr (fnc a) (fnc b))
-- Funcion auxiliar de la fnc que distribuye una disyuncion
distr :: Prop -> Prop -> Prop 
distr a b = if (literal(a)&&literal(b)) then (Or a b) else distrAux a b
-- Funcion auxiliar de la funcion de distribucion
distrAux :: Prop -> Prop -> Prop
distrAux (VarProp p) (Or (VarProp q) (VarProp s)) = (Or (VarProp p) (Or (VarProp q) (VarProp s)))
distrAux (Or (VarProp q) (VarProp s)) (VarProp p)  = (Or  (Or (VarProp q) (VarProp s)) (VarProp p))
distrAux (Or a1 a2) (Or b1 b2) = (Or (distr a1 a2) (distr b1 b2))
distrAux (And a1 a2) (And b1 b2) = (And (distr a1 (And b1 b2)) (distr a2 (And b1 b2)))
distrAux (And a1 a2) b = (And (distr b a1) (distr b a2))
distrAux a (And b1 b2) = (And (distr a b1) (distr a b2))
distrAux a b = (Or a b) --Ya cubrimos todos los casos por lo tanto este ultimo seria el que es una clausula de puras disyunciones

{-Parte 5-}

{- Implementemos una cláusula con una lista de literales en una fórmula-}
type Literal = Prop
type Clausula = [Literal]

{-Resolucion Binaria-}
res :: Clausula -> Clausula -> Clausula 
res x y = elimD((resAux(elimD(x))(elimD(y)))) 

{-Funcion auxiliar para la resolucionBinaria que procesa las listas sin elementos repetidos -}
resAux :: Clausula -> Clausula -> Clausula
resAux [] [] = []
resAux x [] = x
resAux [] x = x
resAux (x:xs) (y:ys) = if (elem(compLit(x)) (y:ys)) 
                     then filter (not.(==compLit(x))) (y:ys) ++ filter (not.(==x)) (x:xs)
                     else [x] ++ resAux xs (y:ys)

{-Implementar una función que dada una literal (l) nos
regrese su literal contraria (l)^c-}
compLit :: Literal -> Literal
compLit T = F
compLit F = T
compLit (VarProp p) = (Neg (VarProp p))
compLit (Neg p) = p

-- Funcion auxiliar que elimina los elementos duplicados de una lista
elimD :: (Eq a) => [a] -> [a]
elimD [] = []
elimD (x:xs) = x : elimD (filter (/= x) xs)

{-Boletin de ejercicios-}
{-grado: recibe una fórmula lógica y regresa el número de conectivos lógicos que tiene-}
grado ::  Prop -> Int
grado T = 0
grado F = 0
grado (VarProp p) = 0
grado (Neg a) = 1 + grado(a)
grado (And a b) = 1 + grado(a) + grado(b)
grado (Or a b) = 1 + grado(a) + grado(b)
grado (Imp a b) = 1 + grado(a) + grado(b)
grado (Syss a b) = 1 + grado(a) + grado(b)

{-atom: recibe una fórmula y regresa el conjunto de subfórmulas atómicas-}
atom :: Prop -> [Prop]
atom T = [T]
atom F = [F]
atom (VarProp p) = [(VarProp p)]
atom (Neg a) = atom(a)
atom (And a b) = atom(a) ++ atom(b)
atom (Or a b) = atom(a) ++ atom(b)
atom (Imp a b) = atom(a) ++ atom(b)
atom (Syss a b) = atom(a) ++ atom(b)

{-sub: recibe una fórmula y regresa el conjunto de todas las subfórmulas-}
sub :: Prop -> [Prop]
sub T = [T]
sub F = [F]
sub (VarProp p) = [(VarProp p)]
sub (Neg a) = [Neg a] ++ sub(a)
sub (And a b) = [And a b] ++ sub(a) ++ sub(b)
sub (Or a b) = [Or a b] ++ sub(a) ++ sub(b)
sub (Imp a b) = [Imp a b] ++ sub(a) ++ sub(b)
sub (Syss a b) = [Syss a b] ++ sub(a) ++ sub(b)

{-eln: recibe una fórmula phi y regresa la fórmula que resulta de reemplazar phi 
en cada subformula de la forma ¬phi por phi -> F-}
eln :: Prop -> Prop
eln T = T 
eln F = F 
eln (VarProp p) = (VarProp p)
eln (Neg a) = (Imp a F)
eln (And a b) = (And (eln(a)) (eln(b)))
eln (Or a b) = (Or (eln(a)) (eln(b)))
eln (Imp a b) = (Imp (eln(a)) (eln(b)))
eln (Syss a b) = (Syss (eln(a)) (eln(b)))