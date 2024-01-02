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
            getTipo fuego == "Fuego"
-}
getTipo :: Tipo -> String
getTipo (Nombre x _) = x
getTipo Null = ""
getTipo _ = error "El Pokemon/Ataque carece de tipo!"


{-
    Dado un Ataque, devuelvel el nombre del tipo del ataque con el contructor: Ataque ID Nombre Daño String

        Ej:
            hidropulso :: Ataque
            hidropulso = Ataque 0 "Hidropulso" 70 "Agua"
            getTipo hidropulso == "Agua"
-}

getTipoAtaque :: Habilidad -> String
getTipoAtaque (Habilidad _ _ _ x) = x

getAtkDef :: Tipo -> ([Tipo],[Tipo])
getAtkDef (Nombre _ (Ataques atk, Defensas def)) = (atk, def)
getAtkDef _ = error "El Pokemon/Ataque carece de tipo!" 



{-
    Calcula si un tipo es eficaz contra otro Tipo, por ejemplo
    getAtaques fuego "Planta" == 1
-}
getAtaques :: Tipo -> String -> Double
getAtaques (Debil xs) atc 
    | elem atc xs = 0.5
    | otherwise = 1  --Hace la llamada recursiva y suma lo que returne
getAtaques (Fuerte xs) atc 
    | elem atc xs = 2
    | otherwise = 1  --Hace la llamada recursiva y suma lo que returne
getAtaques (Inmune xs) atc 
    | elem atc xs = 0
    | otherwise = 1  --Hace la llamada recursiva y suma lo que returne
getAtaques (Nombre _ (ataque,_)) atc = getAtaques ataque atc 
getAtaques (Ataques ts) atc = minimum [getAtaques t atc | t <-ts]


{-
    Calcula si un tipo es defensivo contra otro Tipo, por ejemplo
    getAtaques fuego "Agua" == 1
-}
getDefensas :: Tipo -> String -> Double
getDefensas (Debil xs) def
    | elem def xs = 2
    | otherwise = 1  --Hace la llamada recursiva y suma lo que returne
getDefensas (Fuerte xs) def 
    | elem def xs = 0.5
    | otherwise = 1  --Hace la llamada recursiva y suma lo que returne
getDefensas (Inmune xs) def
    | elem def xs = 0
    | otherwise = 1  --Hace la llamada recursiva y suma lo que returne
getDefensas (Nombre _ (_,defensas)) def = getDefensas defensas def 
getDefensas (Defensas ts) def = minimum [getDefensas t def | t <-ts]

-- getTipoID :: [(Int, Tipo)] -> Int -> Tipo
-- getTipoID xs id = head [ y | (x,y) <-xs, x==id] 
