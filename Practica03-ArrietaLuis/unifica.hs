module Unificacion where

import Data.List


-- ----------------------------------------------------------------------
-- Deiniciones preliminares:  
-- ----------------------------------------------------------------------

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

-- ----------------------------------------------------------------------
-- Miscelánea de ejemplos:
-- ----------------------------------------------------------------------

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

-- Sustituciones
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
esVariable = error "falta implementar :3"


-- ----------------------------------------------------------------------
-- Ejercicio 2: Define una función que regrese el conjunto de variables 
--              presentes en un término dado
-- ----------------------------------------------------------------------
variables :: Termino -> [Variable]
variables = error "falta implementar :3"

-- ----------------------------------------------------------------------
-- Ejercicio 3: Define una función que regrese la lista de variables, 
--              sin duplicados, dada una lista de términos.
-- ----------------------------------------------------------------------
variablesEnLista :: [Termino] -> [Variable]
variablesEnLista = error "falta implementar :3"






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
dominio = error "falta implementar :3"


-- ----------------------------------------------------------------------
-- Ejercicio 2: Define una función que dada una sustitución y una variable,
--              regresa la aplicación de la sustitución a la variable.
-- ----------------------------------------------------------------------
aplicaVar :: Sustitucion -> Variable -> Termino
aplicaVar = error "falta implementar :3"


-- ----------------------------------------------------------------------
-- Ejercicio 3: Define una función que dada una sustitución un término, 
--              regresa la aplicación de la sustitución al término.
-- ----------------------------------------------------------------------
aplicaT :: Sustitucion -> Termino -> Termino
aplicaT = error "falta implementar :3"


-- ----------------------------------------------------------------------
-- Ejercicio 4: Define una función que elimina los pares cuyos elementos
--              son iguales en una sustitución. 
-- ----------------------------------------------------------------------
reduce :: Sustitucion -> Sustitucion
reduce = error "falta implementar :3"


-- ----------------------------------------------------------------------
-- Ejercicio 5: Define una función que dadas dos sustituciones, regresa 
--              su composición.
-- ----------------------------------------------------------------------
composicion :: Sustitucion -> Sustitucion -> Sustitucion
composicion = error "falta implementar :3"


-- ----------------------------------------------------------------------
-- Ejercicio 6: Define la función complista que compone todas las
--              sustituciones de una lista. 
-- ----------------------------------------------------------------------
complista :: [Sustitucion] -> Sustitucion
complista = error "falta implementar :3" 





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
apSustEq = error "falta implementar :3"

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



