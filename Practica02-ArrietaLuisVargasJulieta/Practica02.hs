import Practica01
import Ejemplos
import Actividad03

{-Practica02 - DPLL-}

{-Definimos los tipos de datos que usaremos-}
type Formula = [Clausula]
type Modelo = [Literal]
type Configuracion = (Modelo, Formula)

--Aquí una fórmula y una configuración de ejemplo para las pruebas :)
phi = [[(Neg p), r, (Neg t)],[(Neg q),(Neg r)],[p, (Neg s)],[(Neg p),q,(Neg r),(Neg s)]]
conf = ([], phi)

-- ---------------------------------------------------------------------
-- Definir una función que nos permita saber si en una configuración dada
-- podemos utilizar la regla de la cláusula unitaria, es decir, si hay 
-- literales unitarias en la fórmula y su complemento no se encuentra en
-- el modelo. 
-- ---------------------------------------------------------------------
usarUnit :: Configuracion -> Bool
usarUnit (m,f) = usarUnitAuxForFormula m f
usarUnit x = False -- Cualquier otra cosa que no coincida con los patrones anteriores es falso, por ejemplo ([],[])

usarUnitAuxForFormula :: Modelo -> Formula -> Bool
usarUnitAuxForFormula  m [c] = usarUnitAuxForClausula m c
usarUnitAuxForFormula  m (c:cs) = usarUnitAuxForClausula m c || usarUnitAuxForFormula m cs
usarUnitAuxForFormula x z = False -- Cualquier otra cosa que no coincida con los patrones anteriores es falso

usarUnitAuxForClausula :: Modelo -> Clausula -> Bool
usarUnitAuxForClausula m [x] = not (elem (compLit x) m) -- Si no se encuentra la literal complementaria de x
usarUnitAuxForClausula m (x:xs) = False
usarUnitAuxForClausula x z = False -- Cualquier otra cosa que no coincida con los patrones anteriores es falso

-- ---------------------------------------------------------------------
-- Definir una función que nos permita saber si en una configuración dada
-- podemos utilizar la regla de eliminación, es decir, si hay cláusulas 
-- que contengan una literal que ya se encuentra en nuestro modelo.
-- ---------------------------------------------------------------------
usarElim :: Configuracion -> Bool
usarElim (m,f) = usarElimAuxForFormula m f 
usarElim x = False -- Cualquier otra cosa que no coincida con los patrones anteriores es falso, por ejemplo ([],[])

usarElimAuxForFormula :: Modelo -> Formula -> Bool
usarElimAuxForFormula m [c] = usarElimAuxForClausula m c
usarElimAuxForFormula m (c:cs) = usarElimAuxForClausula m c || usarElimAuxForFormula m cs
usarElimAuxForFormula x z = False -- Cualquier otra cosa que no coincida con los patrones anteriores es falso

usarElimAuxForClausula :: Modelo -> Clausula -> Bool
usarElimAuxForClausula m [x] = elem x m
usarElimAuxForClausula m (x:xs) = usarElimAuxForClausula m [x] || usarElimAuxForClausula m xs
usarElimAuxForClausula x z = False -- Cualquier otra cosa que no coincida con los patrones anteriores es falso

-- ---------------------------------------------------------------------
-- Definir una función que nos permita saber si en una configuración dada
-- podemos utilizar la regla de reducción, es decir, si hay cláusulas 
-- que contengan el complemento de alguna literal de nuestro modelo. 
-- ---------------------------------------------------------------------
usarRed :: Configuracion -> Bool
usarRed (m,f) = usarRedAuxForFormula m f 
usarRed x = False -- Cualquier otra cosa que no coincida con los patrones anteriores es falso, por ejemplo ([],[])

usarRedAuxForFormula :: Modelo -> Formula -> Bool
usarRedAuxForFormula m [c] = usarRedAuxForClausula m c
usarRedAuxForFormula m (c:cs) = usarRedAuxForClausula m c || usarRedAuxForFormula m cs
usarRedAuxForFormula x z = False -- Cualquier otra cosa que no coincida con los patrones anteriores es falso

usarRedAuxForClausula :: Modelo -> Clausula -> Bool
usarRedAuxForClausula m [x] = elem (compLit(x)) m
usarRedAuxForClausula m (x:xs) = usarRedAuxForClausula m [x] || usarRedAuxForClausula m xs 
usarRedAuxForClausula x z = False -- Cualquier otra cosa que no coincida con los patrones anteriores es falso

-- ---------------------------------------------------------------------
-- Definir la función éxito, que nos permita saber si en una configuración
-- dada, la lista de cláusulas que representa la fórmula en cuestión es
-- vacía.
-- ---------------------------------------------------------------------
exito :: Configuracion -> Bool
exito (m, []) = True 
exito x = False -- Cualquier otra cosa que no coincida con los patrones anteriores es falso, por ejemplo ([p,r],[[]])

-- ---------------------------------------------------------------------
-- Definir la función conflicto, que nos permita saber si en una configuración
-- dada, dentro de la lista de cláusula que representa la fórmula en cuestión
-- hay una cláusula vacía.
-- ---------------------------------------------------------------------
conflicto :: Configuracion -> Bool
conflicto (m, f) = conflictoAuxForFormula m f
conflicto x = False -- Cualquier otra cosa que no coincida con los patrones anteriores es falso, por ejemplo ([r,q],[])

conflictoAuxForFormula :: Modelo -> Formula -> Bool
conflictoAuxForFormula m [c] = conflictoAuxForClausula m c
conflictoAuxForFormula m (c:cs) = conflictoAuxForClausula m c || conflictoAuxForFormula m cs
conflictoAuxForFormula x z = False

conflictoAuxForClausula :: Modelo -> Clausula -> Bool
conflictoAuxForClausula m [] = True
conflictoAuxForClausula m (x:xs) = False
conflictoAuxForClausula x z = False -- Cualquier otra cosa que no coincida con los patrones anteriores es falso

-- ---------------------------------------------------------------------
-- Definir una función que devuelve la literal contenida en la primer 
-- clásula unitaria que se encuentre en una fórmmula. ASUMIENDO que existe 
-- al menos una cláusula de este tipo.
-- (Es decir casos como unitaria [[r,s,t],[p,q]]) no pueden ocurrir,
-- tiene que haber al menos una clausula unitaria)
-- ---------------------------------------------------------------------
unitaria :: Formula -> Literal
unitaria (c:cs) = if (esClausulaUnitaria c) 
                    then extraeLit c 
                    else unitaria cs 
{-Funcion auxiliar que determina si una clausula es unitaria-}
esClausulaUnitaria :: Clausula -> Bool
esClausulaUnitaria [x] = True
esClausulaUnitaria x = False -- Cualquier otra cosa que no coincida con el patron anterior es falso
{-Extrar la literal contenida en una clausula unitaria-}
extraeLit :: Clausula -> Literal
extraeLit [c] = c 

-- ---------------------------------------------------------------------
-- Definir una función que elimina la primer cláusula unitaria de una 
-- fórmula dada.
-- ---------------------------------------------------------------------
quitaUnitaria :: Formula -> Formula
quitaUnitaria [x] = if esClausulaUnitaria x then [] else [x]
quitaUnitaria (c:cs) = if esClausulaUnitaria c 
                        then filter (/=c) cs
                        else quitaUnitaria cs ++ [c]
quitaUnitaria x = x -- Cualquier otra cosa que no coincida con los patrones anteriores se regresa la formula tal cual

-- ---------------------------------------------------------------------
-- Definir una función que aplica la regla de eliminación. Es decir, elimina 
-- todas las cláusulas que contengan la literal. 
-- ---------------------------------------------------------------------
elim :: Literal -> Formula -> Formula
elim l [] = []
elim l [x] = [ x | not(elimAux l x)]
elim l (c:cs) = [ c | not(elimAux l c)] ++ elim l cs
{-Funcion auxiliar que devuelve True si la literal esta en la clausula,
False en caso contrario-}
elimAux :: Literal -> Clausula -> Bool
elimAux l [] = False
elimAux l (x:xs) = elem l (x:xs)

-- ---------------------------------------------------------------------
-- Definir una función que implemente la regla de reducción. Es decir, 
-- que elimine de todas las cláusulas en una fórmula a todas las literales 
-- que sean complementarias a una literal dada. 
-- ---------------------------------------------------------------------
red :: Literal -> Formula -> Formula
red l [c] = [redAux l c]
red l (c:cs) = [redAux l c] ++ red l cs
red l x = x -- Cualquier otra cosa que no coincida con los patrones anteriores se regresa la formula tal cual
{-Funcion auxiliar de reduccion-}
redAux :: Literal -> Clausula -> Clausula 
redAux l [c] = if (compLit l) == c then [] else [c]
redAux l (c:cs) = filter (/= (compLit l)) (c:cs)
redAux l x = x -- Cualquier otra cosa que no coincida con los patrones anteriores se regresa la formula tal cual

-- ----------------------------------------------------------------------
-- Definir una función que regrese alguna literal contenida en la fórmula
-- dada inicialmente.
-- ----------------------------------------------------------------------
sigLit :: Formula -> Literal
sigLit [] = error "Error: La formula es vacia"
sigLit [[]] =  error "Error:  La clausula es vacia"
sigLit [c] = headClausula c
sigLit (c:cs) = if c==[] then sigLit cs else headClausula c

{-Funcion auxiliar para sigLit que regresa la cabeza de la clausula-}
headClausula :: Clausula -> Literal
headClausula (l:ls) = l

-- ----------------------------------------------------------------------
-- Definir una función que implemente el algoritmo dpll de búsqueda hacia 
-- atrás para decidir la satisfacibilidad de fórmulas proposicionales.
-- Dado un modelo y una fórmula nos devuelve un modelo en caso de la fórmula 
-- haya sido satisfacible. Devuelve una lista vacía en caso contrario. 
-- ----------------------------------------------------------------------
dpll :: Configuracion -> Configuracion
dpll (m, f) 
    | (usarUnit (m,f)) = dpll((m ++ [(unitaria f)]),(quitaUnitaria f))
dpll ((l:ls), f)
    | (usarElim ((l:ls),f)) = dpll(ls ++ [l],(elim l f))
    | (usarRed ((l:ls), f)) = dpll(ls ++ [l],(red l f))
dpll (m, f)
    | (exito (m, f)) = (m, f) {- Parece redundante este caso y el de abajo pero es para que se termine la ejecucion  ya que 
    en cualquier caso se regresaria (m, [])si es exito y (m,[[]]) si es conflicto, osea las funciones estan bien definidas-}
    | (conflicto (m,f)) = (m,f)
    | otherwise = dpll((m ++ [sigLit f]), f)

-- Formula que da conflicto
phiC = [[q,s],[(Neg q),s],[(Neg q),(Neg s)]]
confC = ([r,(Neg p), q],phiC)

--- Formula [[p,q]],[r]
-- Formula [] = el vacio
-- Formula [[]] = clausula vacia[]