import PokemonData
import Tipo(getNombreTipo)
{-
data Pokemon =  Pokemon Nombre (Tipo, Tipo) Hp [ID]
    deriving Show
-}

--GetPokemonTipo------------------------------------------
-- Dado un pokemon, que devuelva una tupla con el nombre de sus dos Tipos:
--      - Por ejepmlo:
--               Charizard: Volador Fuego -> (Volador, Fuego)

getPokemonTipo :: Pokemon -> (String,String)
getPokemonTipo (Pokemon _ (tipo1, tipo2) _ _) = (getNombreTipo tipo1, getNombreTipo tipo2)

--GetPokemonVida------------------------------------------
-- Dado un pokemon, que devuelva el valor de su vida (Int)
--      - Por ejemplo:
--              Empoleon: 150 -> 150

getPokemonVida :: Pokemon -> Hp
getPokemonVida (Pokemon _ _ vida _)= vida