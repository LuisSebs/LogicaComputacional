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

{-Funcion auxiliar que regresa la primera entrada de la pareja-}
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


--Ejemplo
ss1 = [(x,(T "h" [y]))]
tt1 = f [h[x],x,g[f[x]],y]
e1 = aplicaT ss1 tt1