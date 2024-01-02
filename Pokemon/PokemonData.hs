module PokemonData where

----Sinonimos--------------------------
-- Definidos para tener más limpiza visual
--
type Hp = Int 
type ID = Int
type Daño = Int
type Nombre = String

--Maximo tamaño de nombre 12
data Habilidad = Habilidad ID Nombre Daño Nombre
    deriving Show

------------------------------------------------------------
ataque1 :: Habilidad
ataque1 = Habilidad 0 "Acero" 70 "Acero"
ataque2 :: Habilidad
ataque2 = Habilidad 0 "Bicho" 70 "Bicho"
ataque3 :: Habilidad
ataque3 = Habilidad 0 "Dragon" 70 "Dragon"
ataque4 :: Habilidad
ataque4 = Habilidad 0 "Electrico" 70 "Electrico"
ataque5 :: Habilidad
ataque5 = Habilidad 0 "Fantasma" 70 "Fantasma"
ataque6 :: Habilidad
ataque6 = Habilidad 0 "Lucha" 70 "Lucha"
ataque7 :: Habilidad
ataque7 = Habilidad 0 "Hielo" 70 "Hielo"
ataque8 :: Habilidad
ataque8 = Habilidad 0 "Normal" 70 "Normal"
ataque9 :: Habilidad
ataque9 = Habilidad 0 "Planta" 70 "Planta"
ataque10 :: Habilidad
ataque10 = Habilidad 0 "Psiquico" 70 "Psiquico"
ataque11 :: Habilidad
ataque11 = Habilidad 0 "Roca :D" 70 "Piedra"
ataque12 :: Habilidad
ataque12 = Habilidad 0 "Siniestro" 70 "Siniestro"
ataque13 :: Habilidad
ataque13 = Habilidad 0 "Tierra" 70 "Tierra"
ataque14 :: Habilidad
ataque14 = Habilidad 0 "Fuego" 70 "Fuego"
ataque15 :: Habilidad
ataque15 = Habilidad 0 "Veneno" 70 "Veneno"
ataque16 :: Habilidad
ataque16 = Habilidad 0 "Agua" 70 "Agua"
ataque17 :: Habilidad
ataque17 = Habilidad 0 "Volador" 70 "Volador"
ataque18 :: Habilidad
ataque18 = Habilidad 0 "Hada" 70 "Hada"

--Tipos-----------------------------------------------------
-- Estructura de los tipos:
--     Tipo = Nombre "Agua" [Ataque [Debil [], Fuerte [], Inmune []], Defensas [Debil [], Fuerte [], Inmune []]]
------------------------------------------------------------
data Tipo = Nombre Nombre (Tipo,Tipo) | 
    Defensas [Tipo] | 
    Ataques [Tipo] | 
    Debil [Nombre] | Fuerte [Nombre] | Inmune [Nombre] | Tipo Nombre |
    Null
    deriving Show

-- veneno :: Tipo
-- veneno = Nombre "Veneno" (
--     Defensas [
--         Debil["Tierra", "Psíquico"],
--         Fuerte["Lucha","Veneno","Bicho","Planta","Hada"], 
--         Inmune []
--         ], 
--     Ataque [
--         Debil ["Tierra", "Veneno", "Fantasma", "Roca"],
--         Fuerte ["Planta", "Hada"],
--         Inmune ["Acero"]
--         ])


-- Pokemons con varios tipos, amazing -------------------------------------------

-- data Pokemon =  Pokemon Nombre Hp [ID] [Pokemon] | TipoPok Tipo
--     deriving Show

-- squirtle = Pokemon "Squirtle" 150 [0..12] [TipoPok tipo1, TipoPok tipo2]

-- data Pokemon =  Pokemon Nombre Tipo Hp [ID]
--     deriving Show
----------------------------------------------------------------------------------

data Pokemon =  Pokemon Nombre (Tipo, Tipo) Hp [ID]
    deriving Show

squirtle :: Pokemon
squirtle = Pokemon "Squirtle" (tipo, Null) 150 [0..12]
    where 
        tipo :: Tipo
        tipo = Null

charizard :: Pokemon
charizard = Pokemon "Charizard" (tipo, Null) 750 [0..12]
    where 
        tipo :: Tipo
        tipo = Null