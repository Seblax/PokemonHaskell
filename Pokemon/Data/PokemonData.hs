module Data.PokemonData where


-------------------------------------------------------------------------------
--   _____      _                                _____        _        
--  |  __ \    | |                              |  __ \      | |       
--  | |__) |__ | | _____ _ __ ___   ___  _ __   | |  | | __ _| |_ __ _ 
--  |  ___/ _ \| |/ / _ \ '_ ` _ \ / _ \| '_ \  | |  | |/ _` | __/ _` |
--  | |  | (_) |   <  __/ | | | | | (_) | | | | | |__| | (_| | || (_| |
--  |_|   \___/|_|\_\___|_| |_| |_|\___/|_| |_| |_____/ \__,_|\__\__,_|
--
-- En este Módulo se definen todos los tipos Abstractos que se usarán
-- a lo largo de todo el proyecto
-------------------------------------------------------------------------------

----Sinonimos--------------------------
-- Definidos para tener más limpiza visual
---------------------------------------
type Hp = Int 
type ID = Int
type Daño = Int
type Nombre = String
type NombreTipo = String

--Habilidades-----------------------------------------------
-- Son las habilidades que pueden aprender los Pokemons
--      Habilidad = Habilidad 0 "Energi Bola" 70 "Eléctrico"
--
data Habilidad = Habilidad ID Nombre Daño NombreTipo
    deriving (Show, Eq)

--Tipos-----------------------------------------------------
-- Estructura de los tipos:
--     Tipo = Nombre "Agua" (Ataque [Debil [], Fuerte [], Inmune []])
--     Tipo = Null (Existe porque hay pokemons que solo tienen un Tipo)
------------------------------------------------------------
data Tipo = Nombre NombreTipo Tipo | 
    Ataques [Tipo] | 
    Debil [NombreTipo] | Fuerte [NombreTipo] | Inmune [NombreTipo] | Tipo NombreTipo |
    Null
    deriving (Show, Eq)

-- Pokemons con varios tipos, amazing -------------------------------------------
-- data Pokemon =  Pokemon Nombre (Tipo 1, Tipo 2) Hp [Habilidades]
--     deriving Show
----------------------------------------------------------------------------------

data Pokemon =  Pokemon Nombre (Tipo, Tipo) Hp [Habilidad]
    deriving Show