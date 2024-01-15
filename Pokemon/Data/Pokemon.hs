module Data.Pokemon where

import Data.PokemonData
import Data.Tipo
-- -----------------------------------------------------------------------------------
--                             .-'''-.                                                      .-'''-.                
--                            '   _    \                                                   '   _    \              
-- _________   _...._       /   /` '.   \     .           __.....__      __  __   ___    /   /` '.   \    _..._    
-- \        |.'      '-.   .   |     \  '   .'|       .-''         '.   |  |/  `.'   `. .   |     \  '  .'     '.  
--  \        .'```'.    '. |   '      |  '.'  |      /     .-''"'-.  `. |   .-.  .-.   '|   '      |  '.   .-.   . 
--   \      |       \     \\    \     / /<    |     /     /________\   \|  |  |  |  |  |\    \     / / |  '   '  | 
--    |     |        |    | `.   ` ..' /  |   | ____|                  ||  |  |  |  |  | `.   ` ..' /  |  |   |  | 
--    |      \      /    .     '-...-'`   |   | \ .'\    .-------------'|  |  |  |  |  |    '-...-'`   |  |   |  | 
--    |     |\`'-.-'   .'                 |   |/  .  \    '-.____...---.|  |  |  |  |  |               |  |   |  | 
--    |     | '-....-'`                   |    /\  \  `.             .' |__|  |__|  |__|               |  |   |  | 
--   .'     '.                            |   |  \  \   `''-...... -'                                  |  |   |  | 
-- '-----------'                          '    \  \  \                                                 |  |   |  | 
--                                       '------'  '---'                                               '--'   '--' 
-- Este módulo se centra en el tratamiento de los tipos de datos Pokemon y Habilidad,
-- con funciones que obtienen la vida del pokemon, modifican la vida, obtienen pokemon por nombre
-- obtener las habilidades de los pokemons, etc.
-----------------------------------------------------------------------------------

--Get Pokemon Tipo-----------------------------------------------------------------
-- Dado un pokemon, que devuelva una tupla con el nombre de sus dos Tipos:
--      - Por ejepmlo:
--               Charizard: Volador Fuego -> ("Volador", "Fuego")
-----------------------------------------------------------------------------------
getPokemonTipo :: Pokemon -> (String,String)
getPokemonTipo (Pokemon _ (tipo1, tipo2) _ _) = (getNombreTipo tipo1, getNombreTipo tipo2)

--Get Pokemon Vida-----------------------------------------------------------------
-- Dado un pokemon, que devuelva el valor de su vida (Int)
--      - Por ejemplo:
--              Empoleon: 150 -> 150
-----------------------------------------------------------------------------------
getPokemonVida :: Pokemon -> Hp
getPokemonVida (Pokemon _ _ vida _)= vida

--Set Pokemon Vida-----------------------------------------------------------------
-- Dado un pokemon y un Int que representa el daño que se le realiza al pokemon.
-- Devuelve el mismo pokemon, pero restandole a la vida el daño recivido como entrada
--      - Por ejemplo:
--              setPokemonVida (Empoleon 150) 28 -> Empoleon 122
-----------------------------------------------------------------------------------
setPokemonVida :: Pokemon -> Int -> Pokemon
setPokemonVida (Pokemon n t hp h) daño = Pokemon n t (hp-daño) h

--Get Pokemon Habilidades----------------------------------------------------------
-- Obtiene la lista de Habilidades de un pokemon dado
-----------------------------------------------------------------------------------
getPokemonHabilidades :: Pokemon -> [Habilidad]
getPokemonHabilidades (Pokemon _ _ _ xs) = xs

--Get Nombre Habilidades-----------------------------------------------------------
-- Obtiene el nombre de una Habilidad dada
-----------------------------------------------------------------------------------
getNombreHabilidades:: Habilidad -> String
getNombreHabilidades (Habilidad _ n _ _ ) = n

--Get Pokemon Nombre Habilidades---------------------------------------------------
-- Obtiene una lista con los nombres de la lista de Habilidades de un Pokemon
-----------------------------------------------------------------------------------
getPokemonNombreHabilidades :: Pokemon -> [String]
getPokemonNombreHabilidades (Pokemon _ _ _ xs) = foldr (\x ac -> getNombreHabilidades x : ac) [] xs

--Get Pokemon Habilidad Por Nombre-------------------------------------------------
-- Dado un Nombre y una lista de habilidades, devuelve la primera habilidad que
-- tenga por nombre el nombre dado como parámetro
-----------------------------------------------------------------------------------
getPokemonHabilidadPorNombre :: Nombre -> [Habilidad] -> Habilidad
getPokemonHabilidadPorNombre n hs = head [ h | h@(Habilidad _ nom _ _) <- hs, n == nom]

--Get Pokemon Nombre---------------------------------------------------------------
-- Obtiene el nombre de un Pokemon
-----------------------------------------------------------------------------------
getPokemonNombre :: Pokemon -> Nombre
getPokemonNombre (Pokemon n _ _ _) = n

--Get Pokemon Habilidades----------------------------------------------------------
-- Obtiene la lista de Habilidades de un pokemon dado
-----------------------------------------------------------------------------------
getHabilidadPorID :: [Habilidad] -> Int -> Habilidad
getHabilidadPorID habilidades idPok = head [h | h@(Habilidad id _ _ _) <- habilidades, id == idPok]