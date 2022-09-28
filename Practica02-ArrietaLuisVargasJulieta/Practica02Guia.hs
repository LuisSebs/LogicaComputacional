module Practica2 where
    
    ---import Practica1
    ---import Actividad3
    import Data.List

--------------------------------------------------------------------------------------
----------------------------------Practica 2 - DPLL-----------------------------------
--------------------------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definiendo los tipo de dato que usaremos --
-- ---------------------------------------------------------------------
--    type Literal = Prop
--    type Clausula = [Literal]
    type Formula = [Clausula]
    type Modelo = [Literal]
    type Configuracion = (Modelo, Formula)

-- ----------------------------------------------------------------------
-- Aquí una fórmula y una configuración de ejemplo para las pruebas :)
-- ----------------------------------------------------------------------
p = VarP 1
q = VarP 2
r = VarP 3
s = VarP 4
t = VarP 5

phi = [[(Neg p), r, (Neg t)],[(Neg q),(Neg r)],[p, (Neg s)],[(Neg p),q,(Neg r),(Neg s)]]
conf = ([], phi)

-- ---------------------------------------------------------------------
-- Definir una función que nos permita saber si en una configuración dada
-- podemos utilizar la regla de la cláusula unitaria, es decir, si hay 
-- literales unitarias en la fórmula y su complemento no se encuentra en
-- el modelo. 
-- ---------------------------------------------------------------------
    usarUnit :: Configuracion -> Bool
    usarUnit (m,f) = error "definir :3"




-- ---------------------------------------------------------------------
-- Definir una función que nos permita saber si en una configuración dada
-- podemos utilizar la regla de eliminación, es decir, si hay cláusulas 
-- que contengan una literal que ya se encuentra en nuestro modelo.
-- ---------------------------------------------------------------------
    usarElim :: Configuracion -> Bool
    usarElim (m, f)= error "definir :3"




-- ---------------------------------------------------------------------
-- Definir una función que nos permita saber si en una configuración dada
-- podemos utilizar la regla de reducción, es decir, si hay cláusulas 
-- que contengan el complemento de alguna literal de nuestro modelo. 
-- ---------------------------------------------------------------------
    usarRed :: Configuracion -> Bool
    usarRed (m,f) = error "definir :3"





-- ---------------------------------------------------------------------
-- Definir la función éxito, que nos permita saber si en una configuración
-- dada, la lista de cláusulas que representa la fórmula en cuestión es
-- vacía.
-- ---------------------------------------------------------------------
    exito :: Configuracion -> Bool
    exito (m, f) = error "definir :3"




-- ---------------------------------------------------------------------
-- Definir la función conflicto, que nos permita saber si en una configuración
-- dada, dentro de la lista de cláusula que representa la fórmula en cuestión
-- hay una cláusula vacía.
-- ---------------------------------------------------------------------
    conflicto :: Configuracion -> Bool
    conflicto (m, f) = error "definir :3"




-- ---------------------------------------------------------------------
-- Definir una función que aplica la regla de eliminación. Es decir, elimina 
-- todas las cláusulas unitarias de una formula que contengan una literal dada. 
-- ---------------------------------------------------------------------
    elim :: Literal -> Formula -> Formula
    elim l f = error "definir :3"




 
-- ---------------------------------------------------------------------
-- Definir una función que implemente la regla de reducción. Es decir, 
-- que elimine de todas las cláusulas en una fórmula a todas las literales 
-- que sean complementarias a una literal dada. 
-- ---------------------------------------------------------------------
    red :: Literal -> Formula -> Formula
    red l f = error "definir :3"





-- ---------------------------------------------------------------------
-- Definir una función que devuelve la literal contenida en la primer 
-- clásula unitaria que se encuentre en una fórmmula. Asumiendo que existe 
-- al menos una cláusula de este tipo.
-- ---------------------------------------------------------------------
    unitaria :: Formula -> Literal
    unitaria f = error "definir :3" 




-- ---------------------------------------------------------------------
-- Definir una función que elimina la primer cláusula unitaria de una 
-- fórmula dada.
-- ---------------------------------------------------------------------
    quitaUnitaria :: Formula -> Formula
    quitaUnitaria (c:cs) = error "definir :3"




-- ---------------------------------------------------------------------
-- Definir una función que una dos configuraciones dadas, de modo que si
-- una de ellas contiene un modelo, éste se respete. Si ambas configuraciones
-- tienen modelo, elegimos uno de ellos. En caso de que ninguna lo tenga, 
-- sólo devolvemos la lista vacía. 
-- ---------------------------------------------------------------------
    unirConfig :: Configuracion -> Configuracion -> Configuracion
    unirConfig (m,f) (n,g) = error "definir :3"






-- ----------------------------------------------------------------------
-- Definir una función que regrese alguna literal contenida en la fórmula
-- dada inicialmente.
-- ----------------------------------------------------------------------
    sigLit :: Formula -> Literal
    sigLit (c:cs) = head c

 

-- ----------------------------------------------------------------------
-- Definir una función que implemente el algoritmo dpll de búsqueda hacia 
-- atrás para decidir la satisfacibilidad de fórmulas proposicionales.
-- Dado un modelo y una fórmula nos devuelve un modelo en caso de la fórmula 
-- haya sido satisfacible. Devuelve una lista vacía en caso contrario. 
-- ----------------------------------------------------------------------
    dpll :: Configuracion -> Configuracion
    dpll (m, f) = error "definir :3"






