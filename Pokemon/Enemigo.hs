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
import Data.Potion
--------------------------------------------------------------------------------
-- ▓█████  ███▄    █ ▓█████  ███▄ ▄███▓ ██▓  ▄████  ▒█████  
-- ▓█   ▀  ██ ▀█   █ ▓█   ▀ ▓██▒▀█▀ ██▒▓██▒ ██▒ ▀█▒▒██▒  ██▒
-- ▒███   ▓██  ▀█ ██▒▒███   ▓██    ▓██░▒██▒▒██░▄▄▄░▒██░  ██▒
-- ▒▓█  ▄ ▓██▒  ▐▌██▒▒▓█  ▄ ▒██    ▒██ ░██░░▓█  ██▓▒██   ██░
-- ░▒████▒▒██░   ▓██░░▒████▒▒██▒   ░██▒░██░░▒▓███▀▒░ ████▓▒░
-- ░░ ▒░ ░░ ▒░   ▒ ▒ ░░ ▒░ ░░ ▒░   ░  ░░▓   ░▒   ▒ ░ ▒░▒░▒░ 
--  ░ ░  ░░ ░░   ░ ▒░ ░ ░  ░░  ░      ░ ▒ ░  ░   ░   ░ ▒ ▒░ 
--    ░      ░   ░ ░    ░   ░      ░    ▒ ░░ ░   ░ ░ ░ ░ ▒  
--    ░  ░         ░    ░  ░       ░    ░        ░     ░ ░  
-- Módulo que contiene la "IA" del enemigo

--Generate Enemy Attack---------------------------------------------------------
-- Para entender esta función lo mejor es usar una máquina de estados sencilla que se encuentra
-- en el Readme. Pero en resumen, si el pokemon enemigo está por debajo de 10 hp siempre usará
-- pociones de vida, sino, eligirá ,entre los ataques del pokemon, uno al azar y lo usará
--------------------------------------------------------------------------------
generateEnemyAttack :: (Pokemon, Pokemon) -> [Tipo] -> Pociones -> IO(String,(Pokemon,Pokemon),Pociones)
generateEnemyAttack pokemons@(pokemonE,pokemonA) tipos pocionesE = 
    do           
        rand <- getNumRandomInterval 0 3


        if getPokemonVida pokemonE <= 10 && not (pocionesVacias pocionesE) then 
            do
                let (cura, nuevasPociones) = usarPocion pocionesE
                let pe = setPokemonVida pokemonE (-cura)

                let c = comentarioCuraEnemigo cura

                return (c,(pe,pokemonA),nuevasPociones)
        else 
            do
                let habilidad = getPokemonHabilidades pokemonE !! rand 
                let tipoHabilidad = getTipoPorNombre tipos (getTipoHabilidad habilidad)
                                
                critico <- esCritico
                let (c, p) = hacerElDaño (pokemonA, pokemonE ) (habilidad, tipoHabilidad) critico False
                return (c,(pokemonE,p),pocionesE)