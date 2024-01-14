module Enemigo where

import System.IO
import System.Directory

import Data.Tipo
import Data.PokemonData
import Data.Pokemon

import Parser

import Daño

import UI.GameUI
import UI.UIColors

generateEnemyAttack :: (Pokemon, Pokemon) -> [Tipo] -> IO(String,Pokemon)
generateEnemyAttack pokemons@(pokemonEnemig,pokemonAliado) tipos = 
    do           
        rand <- getNumRandomInterval 0 3

        let habilidad = getPokemonHabilidades pokemonEnemig !! rand 
        let tipoHabilidad = getTipoPorNombre tipos (getTipoHabilidad habilidad)
                        
        critico <- esCritico

        let (c, p) = hacerElDaño (pokemonAliado, pokemonEnemig ) (habilidad, tipoHabilidad) critico False
        return (c,p)

-- generateEnemyAttack :: (Pokemon, Pokemon) -> [Tipo] -> Pociones -> IO(String,(Pokemon,Pokemon))
-- generateEnemyAttack pokemons@(pokemonE,pokemonA) tipos pocionesEnemig = 
--     do           
--         rand <- getNumRandomInterval 0 3


--         if getPokemonVida pokemonE <= 30 then 
--             do
--                 let (cura, nuevasPociones) = usarPocion pocionesEnemig
--                 let pe = setPokemonVida pokemonE (-cura)

--                 let c = comentarioCuraEnemigo cura

--                 return (c,(pe,pokemonA))
--         else 
--             do
--                 let habilidad = getPokemonHabilidades pokemonE !! rand 
--                 let tipoHabilidad = getTipoPorNombre tipos (getTipoHabilidad habilidad)
                                
--                 critico <- esCritico
--                 let (c, p) = hacerElDaño (pokemonA, pokemonE ) (habilidad, tipoHabilidad) critico False
--                 return (c,(pokemonE,p))