module Data.Tipo where
import Data.PokemonData

-----------------------------------------------------------------------------------
--  .----------------.  .----------------.  .----------------.  .----------------. 
-- | .--------------. || .--------------. || .--------------. || .--------------. |
-- | |  _________   | || |     _____    | || |   ______     | || |     ____     | |
-- | | |  _   _  |  | || |    |_   _|   | || |  |_   __ \   | || |   .'    `.   | |
-- | | |_/ | | \_|  | || |      | |     | || |    | |__) |  | || |  /  .--.  \  | |
-- | |     | |      | || |      | |     | || |    |  ___/   | || |  | |    | |  | |
-- | |    _| |_     | || |     _| |_    | || |   _| |_      | || |  \  `--'  /  | |
-- | |   |_____|    | || |    |_____|   | || |  |_____|     | || |   `.____.'   | |
-- | |              | || |              | || |              | || |              | |
-- | '--------------' || '--------------' || '--------------' || '--------------' |
--  '----------------'  '----------------'  '----------------'  '----------------' 
--
--Este módulo está centrado en el tratamiento del tipo de dato abstracto Tipo
--con funciones que tratan el tipo de dato ya sea para obtener su nombre, obtener
--el tipo de un pokemon o habilidad, etc.
-----------------------------------------------------------------------------------

--Es Null--------------------------------------------------------------------------
-- Comprueba si el Tipo que recive como entrada es de tipo Null (un contructor del 
-- dato Tipo).
-----------------------------------------------------------------------------------
esNull :: Tipo -> Bool
esNull Null = True
esNull _ = False

--Get Nombre Tipo------------------------------------------------------------------
-- Dado un Tipo, devuelvel el nombre del tipo
--     Ej
--         fuego::Tipo
--         fuego = Principal "Fuego" (Aatque [Debil "Agua", Fuerte "Planta"])
--         getNombreTipo fuego == "Fuego"
-----------------------------------------------------------------------------------
getNombreTipo :: Tipo -> String
getNombreTipo (Nombre x _) = x
getNombreTipo Null = ""
getNombreTipo _ = error "Error al extraer el nombre del tipo!"

--Get Tipo Habilidad---------------------------------------------------------------
--     Dado una Habilidad, devuelve el nombre del tipo de la Habilidad 
--         Ej:
--             hidropulso :: Ataque
--             hidropulso = Ataque 0 "Hidropulso" 70 "Agua"
--             getNombreTipo hidropulso == "Agua"
-----------------------------------------------------------------------------------
getTipoHabilidad :: Habilidad -> String
getTipoHabilidad (Habilidad _ _ _ x) = x

--Get Tipo Por Nombre---------------------------------------------------------------
-- Get Tipo Por Nombre, recive como parámetro de entrada una lista de Tipos y un String,
-- devuelve el primer tipo de la lista que tenga como nombre el parámetro de entrada Nombre
-----------------------------------------------------------------------------------
getTipoPorNombre :: [Tipo] -> Nombre -> Tipo
getTipoPorNombre [] n = error $ "No se ha encontrado el Tipo: " ++ n
getTipoPorNombre ((Nombre n t):tipos) nombre
    | nombre == "null" = Null
    | n == nombre = Nombre n t
    | otherwise = getTipoPorNombre tipos nombre