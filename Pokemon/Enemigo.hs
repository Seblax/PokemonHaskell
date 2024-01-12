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

generateEnemyAttack :: (Pokemon, Pokemon) -> IO(String,Pokemon)
generateEnemyAttack pokemons@(pokemonEnemig,pokemonAliado) = 
    do           
        seed <- tiempo
        let generator = mkStdGen seed
        let (rand, _) = randomR (0::Int,3::Int) generator

        let ataque = getPokemonHabilidades pokemonEnemig !! rand 
                        
        critico <- esCritico

        let (c, p) = hacerElDaño (pokemonAliado, pokemonEnemig ) ataque critico False
        return (c,p)