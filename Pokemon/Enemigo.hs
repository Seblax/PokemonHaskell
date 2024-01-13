module Enemigo where

import System.IO
import System.Directory

import Tipo
import PokemonData
import UIColors

import Parser
import GameUI

import System.Random

import Pokemon

import Daño

generateEnemyAttack :: (Pokemon, Pokemon) -> [Tipo] -> IO(String,Pokemon)
generateEnemyAttack pokemons@(pokemonEnemig,pokemonAliado) tipos = 
    do           
        seed <- tiempo
        let generator = mkStdGen seed
        let (rand, _) = randomR (0::Int,3::Int) generator

        let habilidad = getPokemonHabilidades pokemonEnemig !! rand 
        let tipoHabilidad = getTipoPorNombre tipos (getTipoHabilidad habilidad)
                        
        critico <- esCritico

        let (c, p) = hacerElDaño (pokemonAliado, pokemonEnemig ) (habilidad, tipoHabilidad) critico False
        return (c,p)