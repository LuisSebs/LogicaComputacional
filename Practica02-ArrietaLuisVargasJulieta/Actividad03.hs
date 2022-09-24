module Actividad03 where 
    
    import Practica01
    import Ejemplos

-- Actividad 1 
    type Literal = Prop
    type Clausula = [Literal]

    {-Esta funcion recibe una formula proposicional en forma normal conjuntiva (fnc)-}
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
    esModeloClausulas e [[T]] = True
    esModeloClausulas e [[F]] = False
    esModeloClausulas e [[(VarProp p)]] = iAux (VarProp p) e 
    esModeloClausulas e [[(Neg (p))]] = if iAux (p) e then False else True 
    esModeloClausulas e [[p, q]] = iAux (p) e || iAux (q) e
    esModeloClausulas e (c:cs) = (esModeloAux e c) && (esModeloClausulas e cs) 

    esModeloAux :: Estado -> Clausula -> Bool    
    esModeloAux e [l] = (elem l e) || (elem (inv l) e) 
    esModeloAux e (l:ls) = (elem l e) || (elem (inv l) e) || (esModeloAux e ls)

    
    inv :: Literal -> Literal
    inv (VarProp x) = (Neg (VarProp x))
    inv (Neg x) = x
    inv T = T
    inv F = F
    
    -- Ejemplo de la actividad 1 del pdf
    fe1 :: Prop 
    fe1 = (And (Or (Neg p) q) r)
    

    -- Ejemplos extra
    f19 :: Prop
    f19 = (And p q)

    f20 :: Prop 
    f20 = (Or p q)

    f21 :: Prop 
    f21 = (And (Or p q) (Or r s))

    f22 :: Prop 
    f22 = fnc ultima

    
