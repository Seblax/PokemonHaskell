module Data.PokemonData where

----Sinonimos--------------------------
-- Definidos para tener más limpiza visual
--
type Hp = Int 
type ID = Int
type Daño = Int
type Nombre = String

--Habilidades-----------------------------------------------
-- Son las habilidades que pueden aprender los Pokemons
--      Habilidad = Habilidad 0 "Energi Bola" 70 "Eléctrico"
--Maximo tamaño de nombre 12
data Habilidad = Habilidad ID Nombre Daño Nombre
    deriving (Show, Eq)

--Tipos-----------------------------------------------------
-- Estructura de los tipos:
--     Tipo = Nombre "Agua" [Ataque [Debil [], Fuerte [], Inmune []], Defensas [Debil [], Fuerte [], Inmune []]]
------------------------------------------------------------
data Tipo = Nombre Nombre Tipo | 
    Ataques [Tipo] | 
    Debil [Nombre] | Fuerte [Nombre] | Inmune [Nombre] | Tipo Nombre |
    Null
    deriving (Show, Eq)

-- Pokemons con varios tipos, amazing -------------------------------------------

-- data Pokemon =  Pokemon Nombre (Tipo 1, Tipo 2) Hp [ID]
--     deriving Show

----------------------------------------------------------------------------------

data Pokemon =  Pokemon Nombre (Tipo, Tipo) Hp [Habilidad]
    deriving Show