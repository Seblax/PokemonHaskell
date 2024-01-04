import PokemonData
{-
data Pokemon =  Pokemon Nombre (Tipo, Tipo) Hp [ID]
    deriving Show
-}

--GetPokemonTipo------------------------------------------
-- Dado un pokemon, que devuelva una tupla con el nombre de sus dos Tipos:
--      - Por ejepmlo:
--               Charizard: Volador Fuego -> (Volador, Fuego)
getPokemonTipo :: Pokemon -> (String,String)
getPokemonTipo = Silvia

--GetPokemonVida------------------------------------------
-- Dado un pokemon, que devuelva el valor de su vida (Int)
--      - Por ejemplo:
--              Empoleon: 150 -> 150
-- (Esta es trivial vamos)
getPokemonVida :: Pokemon -> Hp
getPokemonVida = Silvia