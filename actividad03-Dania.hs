----- regresa una lista con las sublistas de la entrada---

subconj:: [a] -> [[a]]
subconj [] = [[]]
subconj (x:xs) = auxsubconj x (subconj xs) ++ (subconj xs)

auxsubconj x [] = []
auxsubconj x (y:ys) = (x:y) : auxsubconj x ys


--Prop. Tipo de datos para las proposiciones lógicas

data Prop = VarProp Int
     | T
     | F
     | Neg Prop
     | And Prop Prop
     | Or Prop Prop
     | Imp Prop Prop
     | DouImp Prop Prop 

     deriving(Show, Eq)



--- devueleve una lista con las variables en una fórmula (sin repetición)---

vars :: Prop -> [Prop]
vars T = [T]
vars F = [F]
vars (VarProp x) = [VarProp x]

vars (Neg x) = elimina (vars x)
vars (And x y) = elimina (vars x ++ vars y)
vars (Or x y) = elimina (vars x++vars y)
vars (Imp x y) = elimina (vars x++ vars y)
vars (DouImp x y) = elimina (vars x++ vars y)

--- función auxiliar que elimina elementos repetidos---

elimina :: [Prop] -> [Prop]
elimina [] = []
elimina [x] = [x]
elimina (x:xs) = x : [ y  | y <- elimina(xs), y /=x ]


type Estado = [Prop]


--función de interpretación, interpreta cierta o falsa una formula con ciertos estados dados----

interpretacion :: Prop -> Estado -> Bool
interpretacion T x = True
interpretacion F x = False
interpretacion (VarProp x) y = encontrar y (VarProp x)
interpretacion (Neg x) y = if(interpretacion x y) == True then False else True
interpretacion (And x y) z = if(interpretacion x z) == True && (interpretacion y z) == True then True else False
interpretacion (Or x y) z = if(interpretacion x z) == False && (interpretacion y z) == False then False else True
interpretacion (Imp x y) z = if(interpretacion x z) == True && (interpretacion y z) == False then False else True
interpretacion (DouImp x y) z = if(interpretacion x z) == (interpretacion y z) then True else False


--función auxiliar de interpretación, encuentra la variable proposicional en el estado--

encontrar :: Estado -> Prop -> Bool
encontrar [] (VarProp x) = False 
encontrar (x:xs) (VarProp y) = if (VarProp y)==x then True else encontrar (xs) (VarProp y)

p :: Prop
p = VarProp 1

q :: Prop
q = VarProp 2

r :: Prop
r = VarProp 3

s :: Prop
s = VarProp 4

t :: Prop
t = VarProp 5

for1 :: Prop
for1 = Or p (Neg p)

f2 :: Prop
f2 = Or (And s (Neg t)) (Neg r)


