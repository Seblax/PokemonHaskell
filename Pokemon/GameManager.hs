import System.IO
import System.Directory

import Tipo
import PokemonData
import UIColors

import Parser
import GameUI

main :: IO()
main = do
    tipos <- loadTipos      --Todos los tipos 
    menuScreen

--Carga los tipos de los pokemons
loadTipos :: IO [Tipo]
loadTipos = do
    ficheroATK <- readFile "Data/AtkTypes.txt"
    ficheroDEF <- readFile "Data/DefTypes.txt"
    let ataques = drop 1 (lines ficheroATK)
    let defensas = drop 1 (lines ficheroDEF)
    let res = parsearTipos ataques defensas
    return res
    
loadAttaks :: IO()
loadAttaks = undefined

loadPokemons :: IO()
loadPokemons = undefined