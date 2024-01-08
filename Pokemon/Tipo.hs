{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
module Tipo where
import PokemonData

{-
acero::Tipo
acero = Nombre "Acero" (aceroAtaque,aceroDefensa)
aceroAtaque :: Tipo
aceroAtaque = Ataques [ Debil ["Acero","Fuego","Agua","Eléctrico"], Fuerte ["Roca","Hielo","Hada"]]
aceroDefensa :: Tipo
aceroDefensa = Defensas [Debil ["Lucha", "Fuego", "Tierra"], Fuerte ["Normal", "Volador", "Roca", "Bicho", "Acero", "Planta", "Psíquico", "Hielo", "Dragón", "Hada"], Inmune ["Veneno"]]
-}

{-
    Dado un Tipo, devuelvel el nombre del contructor: data Tipo = Principal Nombre [Tipo] | 
                                                                        Defensa [Tipo] | 
                                                                        Ataques [Tipo] | 
                                                                        Debil Nombre | 
                                                                        Fuerte Nombre | 
                                                                        Inmune Nombre | 
                                                                        Tipo Nombre |
                                                                        Null

        Ej
            fuego::Tipo
            fuego = Principal "Fuego" [Debil "Agua", Fuerte "Planta"]
            getNombreTipo fuego == "Fuego"
-}
getNombreTipo :: Tipo -> String
getNombreTipo (Nombre x _) = x
getNombreTipo Null = ""
getNombreTipo _ = error "Error al extraer el nombre del tipo!"

{-
    Dado un Ataque, devuelvel el nombre del tipo del ataque con el contructor: Ataque ID Nombre Daño String

        Ej:
            hidropulso :: Ataque
            hidropulso = Ataque 0 "Hidropulso" 70 "Agua"
            getNombreTipo hidropulso == "Agua"
-}

getTipoHabilidad :: Habilidad -> String
getTipoHabilidad (Habilidad _ _ _ x) = x

getAtkDef :: Tipo -> ([Tipo],[Tipo])
getAtkDef (Nombre _ (Ataques atk, Defensas def)) = (atk, def)
getAtkDef _ = error "El Pokemon/Ataque carece de tipo!" 



{-
    Calcula si un tipo es eficaz contra otro Tipo. 
    
            -Si el Tipo 1 es muy eficaz (Fuerte) contra 
            el otro tipo, devolverá un x2

            -Si el Tipo 1 es debil (Debil) contra
            el otro tipo, devolverá un x0.5

            -Si el Tipo 1 no afecta (Inmune) al otro tipo
            devolverá un x0

            Ejemplo:
                Agua -> Fuego -> x2
                Agua -> Planta -> x0.5
                Fantasma -> Normal -> x0

-}

--getEficaciaAtaques :: Tipo -> Tipo -> Double
--getEficaciaAtaques = undefined

getEficaciaAtaques :: Tipo -> Tipo -> Double
getEficaciaAtaques tipo1 tipo2
    | esDebil tipo1 tipo2 = 0.5
    | esFuerte tipo1 tipo2 = 2.0
    | esInmune tipo1 tipo2 = 0.0
    | otherwise = 1.0

-- Aux para comprobar si es débil
esDebil :: Tipo -> Tipo -> Bool
esDebil (Nombre _ (_, Ataques [Debil debilidades,_,_])) (Nombre nom _) = elem nom debilidades
esDebil _ _ = False

-- Aux para comprobar si es fuerte
esFuerte :: Tipo -> Tipo -> Bool
esFuerte (Nombre _ (_, Ataques [_,Fuerte fortalezas,_])) (Nombre nom _) = elem nom fortalezas
esFuerte _ _ = False

-- Aux para comprobar si es inmune
esInmune :: Tipo -> Tipo -> Bool
esInmune (Nombre _ (_, Ataques [_,_,Inmune inmunidades])) (Nombre nom _) = elem nom inmunidades
esInmune _ _ = False




{-
    Calcula si un tipo es defensivo contra otro Tipo. Exactamente
    recíproco a la función anterior. Pero en este caso, si el tipo 2
    es Debil, devolverá x2, y si el segudno tipo es fuerte devolverá
    x0.5, e inmunne se queda igual x0. Esto es porque el Acero, por ejemplo,
    es fuerte defendiendose contra el Normal (x0.5), pero Debil defendiendose
    contra el tipo Lucha (x2) y el veneno no afecta al Acero (x0)

            Ejemplo:
                Acero -> Normal -> x0.5       
                Acero -> Lucha -> x2                    
                Acero -> Veneno -> x0    
-}

getEficaciaDefensas :: Tipo -> Tipo -> Double
getEficaciaDefensas tipo1 tipo2
    | esDebil' tipo1 tipo2 = 2.0
    | esFuerte' tipo1 tipo2 = 0.5
    | esInmune' tipo1 tipo2 = 0.0
    | otherwise = 1.0

-- Aux para comprobar si es débil
esDebil' :: Tipo -> Tipo -> Bool
esDebil' (Nombre _ (Defensas [Debil debilidades,_,_], _)) (Nombre nom _) = elem nom debilidades
esDebil' _ _ = False

-- Aux para comprobar si es fuerte
esFuerte' :: Tipo -> Tipo -> Bool
esFuerte' (Nombre _ (Defensas [_,Fuerte fortalezas,_], _)) (Nombre nom _) = elem nom fortalezas
esFuerte' _ _ = False

-- Aux para comprobar si es inmune
esInmune' :: Tipo -> Tipo -> Bool
esInmune' (Nombre _ (Defensas [_,_,Inmune inmunidades], _)) (Nombre nom _) = elem nom inmunidades
esInmune' _ _ = False


{-
    GetTipoPorNombre obtiene como parámetros una lista de tipos y un String
    y devuelve dicho Tipo que tenga de nombre ese String
-}
getTipoPorNombre :: [Tipo] -> Nombre -> Tipo
getTipoPorNombre [] _ = error "Lista vacía"
getTipoPorNombre ((Nombre n t):tipos) nombre
    | n == nombre = Nombre n t
    | otherwise = getTipoPorNombre tipos nombre