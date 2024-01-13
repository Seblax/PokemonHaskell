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