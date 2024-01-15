module Enemigo (generateEnemyAttack) where

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
        --Si el enemigo está por debajo de 10 de vida y tiene pociones se cura
        if getPokemonVida pokemonE <= 10 && not (pocionesVacias pocionesE) then
            do
                curar pokemons pocionesE
        -- Si no ataca
        else
            do
                atacar pokemons tipos pocionesE


curar :: (Pokemon, Pokemon) -> Pociones ->  IO(String,(Pokemon,Pokemon),Pociones)
curar pokemons@(pokemonE,pokemonA) pocionesE  = do
    --gasta la poción
    let (cura, nuevasPociones) = usarPocion pocionesE
    --se cura
    let pe = setPokemonVida pokemonE (-cura)

    -- genera comentario de cura
    let c = comentarioCuraEnemigo cura
    return (c,(pe,pokemonA),nuevasPociones)

atacar :: (Pokemon, Pokemon) -> [Tipo] -> Pociones -> IO(String,(Pokemon,Pokemon),Pociones)
atacar pokemons@(pokemonE,pokemonA) tipos pocionesE = do
    --escoge una habilidad aleatoria
    rand <- getNumRandomInterval 0 3
    let habilidad = getPokemonHabilidades pokemonE !! rand
    let tipoHabilidad = getTipoPorNombre tipos (getTipoHabilidad habilidad)

    -- Comprobamos si es crítico
    critico <- esCritico

    --Hacemos el ataque
    let (c, p) = hacerElDaño (pokemonA, pokemonE ) (habilidad, tipoHabilidad) critico False
    
    return (c,(pokemonE,p),pocionesE)