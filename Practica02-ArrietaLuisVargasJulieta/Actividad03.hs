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
    esModeloAux e [T] = True
    esModeloAux e [F]= False
    esModeloAux e [x] = elem x e
    esModeloAux e (x:xs) = (esModeloAux e [x])|| (esModeloAux e xs) || ((elem x (x:xs))&&(elem (compLit(x)) (x:xs))) 