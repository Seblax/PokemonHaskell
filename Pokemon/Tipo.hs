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
getEficaciaAtaques (Nombre _ (Ataques atk1, _)) (Nombre _ (Ataques atk2, _)) = calculaEficacia atk1 atk2
getEficaciaAtaques _ _ = error "No se puede calcular la eficacia!"

--Aux para calcular eficacia
calculaEficacia :: [Tipo] -> [Tipo] -> Double
calculaEficacia [] _ = 1.0  
calculaEficacia _ [] = 1.0
calculaEficacia (a:as) t2
    | esDebil a t2 = 0.5 -- Si es débil : eficacia = 0.5
    | esFuerte a t2 = 2.0 -- Si es fuerte : eficacia = 2.0
    | esInmune a t2 = 0.0 -- Si es inmune : eficacia = 0.0
    | otherwise = 1.0


--Aux para saber si es débil
esDebil :: Tipo -> [Tipo] -> Bool
esDebil _ [] = False
esDebil (Debil ds) (Nombre n _ : resto)
    | elem n ds = True
    | otherwise = esDebil (Debil ds) resto

--Aux para saber si es fuerte
esFuerte :: Tipo -> [Tipo] -> Bool
esFuerte _ [] = False
esFuerte (Fuerte fs) (Nombre n _ : resto)
    | elem n fs = True
    | otherwise = esFuerte (Fuerte fs) resto

--Aux para saber si es inmune
esInmune :: Tipo -> [Tipo] -> Bool
esInmune _ [] = False
esInmune (Inmune is) (Nombre n _ : resto)
    | elem n is = True
    | otherwise = esInmune (Inmune is) resto

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

getEficaciasDefensas :: Tipo -> Tipo -> Double
getEficaciasDefensas = undefined

{-
    GetTipoPorNombre obtiene como parámetros una lista de tipos y un String
    y devuelve dicho Tipo que tenga de nombre ese String
-}
getTipoPorNombre :: [Tipo] -> Nombre -> Tipo
getTipoPorNombre = undefined