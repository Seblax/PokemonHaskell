module Pokemon where

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

getPokemonHabilidades :: Pokemon -> [Habilidad]
getPokemonHabilidades (Pokemon _ _ _ x) = x

getPokemonNombreHabilidades :: Pokemon -> [String]
getPokemonNombreHabilidades (Pokemon _ _ _ xs) = [ getNombreHabilidades x | x <- xs]

getNombreHabilidades:: Habilidad -> String
getNombreHabilidades (Habilidad _ n _ _ ) = n 

getPokemonNombre :: Pokemon -> String
getPokemonNombre (Pokemon n _ _ _) = n