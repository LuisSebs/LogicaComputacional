module Actividad03 where 
    
    import Practica01
    import Ejemplos

-- Actividad 1 

    {-Esta funcion recibe una formula proposicional en forma normal conjuntiva (fnc)
    y regresa una lista de listas de las variables proposicionales de cada clausula -}
    clausulasFNC :: Prop -> [Clausula]
    clausulasFNC T = [clausulasFNCAux(T)]
    clausulasFNC F = [clausulasFNCAux(F)]
    clausulasFNC (VarProp p) = [clausulasFNCAux(VarProp p)]
    clausulasFNC (Neg p) = [clausulasFNCAux(Neg p)]-- Suponiendo que esta en fnc entonces las negaciones estan frente a variables proposicionales
    clausulasFNC (And p q) = clausulasFNC(p) ++ clausulasFNC(q)
    clausulasFNC (Or p q) = [clausulasFNCAux(Or p q)]

    {-Funcion auxiliar que considera el caso en el que la formula proposicional sea una disyuncion-}
    clausulasFNCAux :: Prop -> Clausula
    clausulasFNCAux T = [T]
    clausulasFNCAux F = [F]
    clausulasFNCAux (Neg p) = [(Neg p)]
    clausulasFNCAux (VarProp p) = [(VarProp p)]
    clausulasFNCAux (Or p q) = clausulasFNCAux(p) ++ clausulasFNCAux(q)

-- Actividad 2
    esModeloClausulas :: Estado -> [Clausula] -> Bool
    esModeloClausulas e [x] = esModeloAux e x
    esModeloClausulas e (x:xs) = (esModeloAux e x) && (esModeloClausulas e xs)

    esModeloAux :: Estado -> Clausula -> Bool 
    esModeloAux e [x] = elem x e
    esModeloAux e (x:xs) = (elem x e) ||  (esModeloAux e xs) || ((elem x (x:xs))&&(elem (compLit(x)) (x:xs))) 
    {- El ultimo || de la funcion esModeloAux es el caso para p v ¬p ya que siempre que encontremos 
    una variable y su complementaria en una clausula entonces la clausula ya es verdadera (True)-}

    {-Código hecho por el ayudante
    esModeloClausulas :: Estado -> [Clausula] -> Bool
    esModeloClausulas e (c:cs) = (esModeloAux e c) && (esModeloClausulas e cs) 

    esModeloAux :: Estado -> Clausula -> Bool    
    esModeloAux e (l:ls) = (elem l e) || (elem (inv l) e) || (esModeloAux e ls)
    esModeloAux e l = (elem l e) || (elem (inv l) e) 

    inv :: Literal -> Literal -- Funcion equivalente a la funcion compLit(l) de la practica01
    inv (VarProp x) = (Neg (VarProp x))
    inv (Neg x) = x
    inv c = (not c)
    -}
    


    
