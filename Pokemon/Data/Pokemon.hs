module Data.Pokemon where

import Data.PokemonData
import Data.Tipo
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

setPokemonVida :: Pokemon -> Int -> Pokemon
setPokemonVida (Pokemon n t hp h) daño = Pokemon n t (hp-daño) h


getPokemonHabilidades :: Pokemon -> [Habilidad]
getPokemonHabilidades (Pokemon _ _ _ xs) = xs

getNombreHabilidades:: Habilidad -> String
getNombreHabilidades (Habilidad _ n _ _ ) = n

{-

getPokemonNombreHabilidades :: Pokemon -> [String]
getPokemonNombreHabilidades (Pokemon _ _ _ xs) = [ getNombreHabilidades x | x <- xs]

-}

getPokemonNombreHabilidades :: Pokemon -> [String]
getPokemonNombreHabilidades (Pokemon _ _ _ xs) = foldr (\x ac -> getNombreHabilidades x : ac) [] xs

getPokemonHabilidadPorNombre :: String -> [Habilidad] -> Habilidad
getPokemonHabilidadPorNombre n hs = head [ h | h@(Habilidad _ nom _ _) <- hs, n == nom]

getPokemonNombre :: Pokemon -> String
getPokemonNombre (Pokemon n _ _ _) = n