module Data.Tipo where
import Data.PokemonData

{-
    Dado un Tipo, devuelvel el nombre del tipo
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
    Dado una Habilidad, devuelv el nombre del tipo de la Habilidad 
        Ej:
            hidropulso :: Ataque
            hidropulso = Ataque 0 "Hidropulso" 70 "Agua"
            getNombreTipo hidropulso == "Agua"
-}

getTipoHabilidad :: Habilidad -> String
getTipoHabilidad (Habilidad _ _ _ x) = x

esNull :: Tipo -> Bool
esNull Null = True
esNull _ = False

{-
    GetTipoPorNombre obtiene como parámetros una lista de tipos y un String
    y devuelve dicho Tipo que tenga de nombre ese String
-}
getTipoPorNombre :: [Tipo] -> Nombre -> Tipo
getTipoPorNombre [] n = error $ "No se ha encontrado el Tipo: " ++ n
getTipoPorNombre ((Nombre n t):tipos) nombre
    | nombre == "null" = Null
    | n == nombre = Nombre n t
    | otherwise = getTipoPorNombre tipos nombre