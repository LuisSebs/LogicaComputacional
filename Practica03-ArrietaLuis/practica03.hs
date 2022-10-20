{-Arrieta Mancera Luis Sebastian 318174116-}

-- ----------------------------------------------------------------------
-- Los identificadores de los términos son alias de cadenas
-- ----------------------------------------------------------------------
type Ident = String

-- ----------------------------------------------------------------------
-- Un término es una variable o un símbolo de función seguido de una
-- lista de términos. No hay constantes explícitas.
-- ----------------------------------------------------------------------
data Termino = V Ident
             | T Ident [Termino]
             deriving Eq

-- ----------------------------------------------------------------------
-- Instancia show para terminos 
-- ----------------------------------------------------------------------
instance Show Termino where
  show (V ident)    = ident
  show (T ident []) = ident
  show (T ident ts) = ident ++ concat [show ts]
  
-- ----------------------------------------------------------------------
-- Una variable será alias de Termino. 
-- ----------------------------------------------------------------------
type Variable = Termino

-- ----------------------------------------------------------------------
-- Una sustitución es una lista de pares formados por una variable y un 
-- término.
-- ----------------------------------------------------------------------
type Sustitucion = [(Variable, Termino)]

-- Símbolos de constante
a = T "a" []
b = T "b" []
c = T "c" []

-- Variables
x = V "x"
y = V "y"
z = V "z"
u = V "u"

-- Símbolos de función
f = T "f" 
g = T "g"
h = T "h"

-- Términos
t1 = f [x,y,x]
t2 = f [y, g [x],x]
t3 = f [g [x], h [x,u]]
t4 = f [z, h [f [y,y],z]]
t5 = h [g [z]]
t6 = h [f [a], g [x]]
t7 = h [z,z]
t8 = h [y,z]
t9 = h [x, g [a]]
t10 = h [g [z],z]

s1 = [(x, a), (z, f [x, y])]
s2 = [(x, z), (y, u)]
s3 = [(z, x), (x, b), (u, c)]
s4 = [(u, f [x]), (y, a)]
s5 = [(x, h [z]), (y, g [b])]
s6 = [(z, g [x])]
s7 = [(x, f [y,y])]
s8 = [(u, g [f [y,y]])]

-- ----------------------------------------------------------------------
-- --Funciones auxiliares sobre términos
-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Ejercicio 1: Define una función que nos indique si un término dado es
--              una variable:
-- ----------------------------------------------------------------------
esVariable :: Termino -> Bool
esVariable (V i) = True
esVariable x = False

-- ----------------------------------------------------------------------
-- Ejercicio 2: Define una función que regrese el conjunto de variables 
--              presentes en un término dado
-- ----------------------------------------------------------------------
variables :: Termino -> [Variable]
variables (V i) = [(V i)]
variables (T i ts) = nub(variablesAux(ts))

{-Funcion auxiliar que regresa la lista de variables encontradas en una lista de terminos-}
variablesAux :: [Termino] -> [Variable]
variablesAux [] = []
variablesAux [x] = if esVariable(x) then [x] else variables x
variablesAux (x:xs) = if esVariable(x) then [x] ++ variablesAux xs else variables x ++ variablesAux xs

{-Funcion que elimina las variables repetidas de una lista de variables-}
nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

-- ----------------------------------------------------------------------
-- Ejercicio 3: Define una función que regrese la lista de variables, 
--              sin duplicados, dada una lista de términos.
-- ----------------------------------------------------------------------
variablesEnLista :: [Termino] -> [Variable]
variablesEnLista [] = []
variablesEnLista [x] = variables x
variablesEnLista (x:xs) = nub(variables x ++ variablesEnLista xs)

-- ----------------------------------------------------------------------
-- --Funciones auxiliares sobre sustituciones
-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Función que representa a la sustitución identidad
-- ----------------------------------------------------------------------
idSust:: Sustitucion
idSust = []

-- ----------------------------------------------------------------------
-- Ejercicio 1: Define una función que dada una sustitución, obtiene su 
--              dominio.
-- ----------------------------------------------------------------------
dominio :: Sustitucion -> [Variable]
dominio [] = [] 
dominio (x:xs) = dominioAux x ++ dominio xs

{-Funcion auxiliar que regresa una lista con la primer entrada de la pareja-}
dominioAux :: (Variable, Termino) -> [Variable]
dominioAux (x,t) = [x]

-- ----------------------------------------------------------------------
-- Ejercicio 2: Define una función que dada una sustitución y una variable,
--              regresa la aplicación de la sustitución a la variable.
-- ----------------------------------------------------------------------
aplicaVar :: Sustitucion -> Variable -> Termino
aplicaVar [] x = x
aplicaVar (s:ls) x = if (aplicaVarAux s x) == x then aplicaVar ls x else aplicaVarAux s x

{-Funcion auxiliar que regresa la segunda entrada de la pareja si la variable
coincide con la primer entrada de la pareja. En caso contrario regresa la variable-}
aplicaVarAux :: (Variable, Termino) -> Variable -> Termino
aplicaVarAux (v,t) x = if v == x then t else x

-- ----------------------------------------------------------------------
-- Ejercicio 3: Define una función que dada una sustitución y un término, 
--              regresa la aplicación de la sustitución al término.
-- ----------------------------------------------------------------------
aplicaT :: Sustitucion -> Termino -> Termino
aplicaT s t | esVariable(t) = aplicaVar s t
aplicaT s (T i ts) = (T i (aplicaTAux s ts))

aplicaTAux :: Sustitucion -> [Termino] -> [Termino]
aplicaTAux s [] = []
aplicaTAux s (t:ts) = [aplicaT s t] ++  (aplicaTAux s ts)

-- ----------------------------------------------------------------------
-- Ejercicio 4: Define una función que elimina los pares cuyos elementos
--              son iguales en una sustitución. 
-- ----------------------------------------------------------------------
reduce :: Sustitucion -> Sustitucion
reduce [] = []
reduce (x:xs) = (reduceAux x) ++ reduce xs

{-Funcion auxiliar que regresa la lista vacia si la primer entrada
de la pareja es igual a la segunda-}
reduceAux :: (Variable, Termino) -> Sustitucion
reduceAux (v,t) = if v == t then [] else [(v,t)]

-- ----------------------------------------------------------------------
-- Ejercicio 5: Define una función que dadas dos sustituciones, regresa 
--              su composición.
-- ----------------------------------------------------------------------
composicion :: Sustitucion -> Sustitucion -> Sustitucion
composicion [] s = s
composicion (s:ls) x = remove(reduce((composicionAux s x) ++ (composicion ls x)))

{-Funcion auxiliar que aplica una sustitucion a la segunda entrada de una pareja -}
composicionAux :: (Variable, Termino) -> Sustitucion -> Sustitucion
composicionAux (v,t) s = [(v,(aplicaT s t))]

{-Funcion auxiliar que remueve de la sustitucion a aquellas parejas
donde la primer entrada ya figura dentro de la primer entrada de otra pareja
encontrada anteriormente.-}
remove :: Sustitucion -> Sustitucion
remove s = removeAux s []

{-Funcion auxiliar que dada una lista de variables, elimina
de la sustitucion a aquellas parejas tales que la primer entrada figure 
dentro de la lista de variables. En caso de que la lista se encuentre
vacia al iniciar la ejecucion, el comportamiento de la funcion sera el mismo
que el de la funcion remove. -}
removeAux :: Sustitucion -> [Variable] -> Sustitucion
removeAux [] l = []
removeAux (x:xs) l = if (notIn (ext x) l)  then [x] ++ (removeAux xs ([(ext x)] ++ l)) else (removeAux xs l)

{-Funcion auxiliar que regresa la variable contenida en la primer entrada-}
ext :: (Variable, Termino) -> Variable
ext (v,t) = v

{-Funcion auxiliar que regresa True en caso de que la variable no
se encuentre dentro de la lista de variables, regresa False en caso
de que si se encuentre.-}
notIn :: Variable -> [Variable] -> Bool
notIn t l = not(elem t l)

-- ----------------------------------------------------------------------
-- Ejercicio 6: Define la función complista que compone todas las
--              sustituciones de una lista. 
-- ----------------------------------------------------------------------
complista :: [Sustitucion] -> Sustitucion
complista [x] = x
complista (x:xs) = complista ([(composicion x (sig xs))] ++ (cola xs))

{-Funcion auxiliar que regresa el siguiente elemento de la lista-}
sig :: [a] -> a
sig (x:xs) = x

{-Funcion auxiliar que regresa el la lista sin la cabeza-}
cola :: [a] -> [a]
cola (x:xs) = xs

-- ----------------------------------------------------------------------
-- --El algoritmo Martellli-Montanari
-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Una ecuacion es un par de terminos. 
-- ----------------------------------------------------------------------
type EqTerm = (Termino,Termino)

-- ----------------------------------------------------------------------
-- Ejercicio 1: Define una función que aplique una sustitución a una 
--              ecuación
-- ----------------------------------------------------------------------
apSustEq :: Sustitucion -> EqTerm -> EqTerm
apSustEq s (t1,t2) = (aplicaT s t1, aplicaT s t2)

-- ----------------------------------------------------------------------
-- Ejercicio 2: Define la función que implementa el algoritmo de 
--              Martelli-Montanari
-- ----------------------------------------------------------------------
unifmm :: [EqTerm] -> [Sustitucion] -> Sustitucion
unifmm = error "falta implementar :3"

-- ----------------------------------------------------------------------
-- Aquí hay una función que unifica una ecuación, úsala sabiamente ;)
-- ----------------------------------------------------------------------
unif :: EqTerm -> Sustitucion
unif eq = unifmm [eq] []

-- ----------------------------------------------------------------------
-- Ejercicio 3: Define una función que regresa el unificador más general
--              para una lista de términos
-- ----------------------------------------------------------------------
unifL :: [Termino] -> Sustitucion
unifL = error "falta implementar :3"








--Ejemplo
ss1 = [(x,(T "h" [y]))]
tt1 = f [h[x],x,g[f[x]],y]
e1 = aplicaT ss1 tt1

--Ejemplo para reduce
rho = [(x,a),(z,f[x,y]),(y,y),(u,u)]

--Ejemplo para composicion
sigma = [(x,a),(z,f[x,y])]
p = [(x,z),(y,u)]

--Ejemplos para remove
p1 = [(x,a),(z,f[z,u]),(x,z),(y,u),(x,f[g[x,y,z]])]
p2 = [(x,a),(z,f[z,u]),(x,z),(y,u),(z,f[g[x,y,z]])]
p3 = [(x,a),(z,f[z,u]),(x,z),(y,u),(z,f[g[x,y,z]]),(z,x),(z,y)]
p4 = [(x,a),(x,z),(z,x),(z,y),(z,f[z,u]),(y,h[f[g[x],y]]),(z,f[g[x,y,z]]),(y,u)]

--Ejemplo para complist
lista = [s2,s6,s7]

--Ejemplo para apSustEq
eq1 = (t1,t3)




