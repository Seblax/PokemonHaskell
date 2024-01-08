{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
module Tipo where
import PokemonData

acero::Tipo
acero = Nombre "Acero" (aceroAtaque,aceroDefensa)
aceroAtaque :: Tipo
aceroAtaque = Ataques [ Debil ["Acero","Fuego","Agua","Eléctrico"], Fuerte ["Roca","Hielo","Hada"]]
aceroDefensa :: Tipo
aceroDefensa = Defensas [Debil ["Lucha", "Fuego", "Tierra"], Fuerte ["Normal", "Volador", "Roca", "Bicho", "Acero", "Planta", "Psíquico", "Hielo", "Dragón", "Hada"], Inmune ["Veneno"]]

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

getEficaciaAtaques :: Tipo -> Tipo -> Double
getEficaciaAtaques tipo1 tipo2
    | esDebil tipo2 = 0.5 -- si es débil : eficacia = 0.5
    | esFuerte tipo2 = 2.0 -- si es fuerte : eficacia = 2.0
    | esInmune tipo2 = 0.0 -- si es inmune : eficacia = 0.0
    | otherwise = 1.0 -- eoc : eficacia = 1.0


{-
    Aux para comprobar que el ataque sea débil: 
-}
esDebil :: Tipo -> Bool
esDebil (Ataques atk) = any (`elem` tipo) (tipoDebiles atk)
    where 
        tipo = [x | Debil xs <- atk, x <- xs]
esDebil _ = False

tipoDebiles :: [Tipo] -> [Nombre]
tipoDebiles tipos = [x | Debil xs <- tipos, x <- xs]


{-
    Aux para comprobar que el ataque sea fuerte: 
-}
esFuerte :: Tipo -> Bool
esFuerte (Ataques atk) = any (`elem` tipo) (tipoFuertes atk)
    where 
        tipo = [x | Fuerte xs <- atk, x <- xs]
esFuerte _ = False

tipoFuertes :: [Tipo] -> [Nombre]
tipoFuertes tipos = [x | Fuerte xs <- tipos, x <- xs]


{-
    Aux para comprobar que el ataque sea inmune: 
-}
esInmune :: Tipo -> Bool
esInmune (Ataques atk) = any (`elem` tipo) (tipoInmunes atk)
    where 
        tipo = [x | Inmune xs <- atk, x <- xs]
esInmune _ = False

tipoInmunes :: [Tipo] -> [Nombre]
tipoInmunes tipos = [x | Inmune xs <- tipos, x <- xs]


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



{-
    GetTipoPorNombre obtiene como parámetros una lista de tipos y un String
    y devuelve dicho Tipo que tenga de nombre ese String
-}
getTipoPorNombre :: [Tipo] -> Nombre -> Tipo
getTipoPorNombre [] _ = error "Lista vacía"
getTipoPorNombre ((Nombre n t):tipos) nombre
    | n == nombre = Nombre n t
    | otherwise = getTipoPorNombre tipos nombre