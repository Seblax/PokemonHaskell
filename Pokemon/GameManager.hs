import System.IO
import System.Directory

import Tipo
import PokemonData
import UIColors

import Parser
import GameUI

main :: IO()
main = do
    tipos <- loadTipos              --Todos los tipos 
    habilidades <- loadHabilities   --Todos los Ataques
    
    --Carga el Menu
    menuScreen
    --Input del jugador
    n <- instruccionColor "Elige un botón del menú:" yellow
    menuBehavior n habilidades


menuBehavior :: String -> [Habilidad] -> IO ()
menuBehavior s h
  | s == "Start" = do wantToContinue h
  | s == "Load" = do putStr $ green ++ "Has seleccionado LOAD!" ++ none
  | s == "Exit" = do putStr $ red ++ "¡Hasta otra entrenador!" ++ none
  | otherwise = main

wantToContinue :: [Habilidad] -> IO()
wantToContinue h = do
    clearScreen
    putStrLn "##########################################################################"
    putStrLn $ "Se va a generar un equipo pokemon aleatoriamente, si deseas volver atrás \n simplemente escriba " ++ red ++  "'Atrás' "++ none  ++ ", si deseas continuar escriba" ++  green ++" 'Ok'" ++ none
    putStrLn "##########################################################################"
    respuesta <- instruccionColor "¿Desea generar tus nuevos Pokemons?" green
    respuestaSelecionada respuesta
    where
        respuestaSelecionada :: String -> IO()
        respuestaSelecionada r 
            | r == "Ok" = batalla h
            | r == "Atrás" = putStr "Has dicho Atrás"
            | otherwise = wantToContinue h 
    


batalla :: [Habilidad] -> IO ()
batalla h = do
  clearScreen
  pokemonBattleUI [squirtle, charizard]
  putStrLn $ habilidadesUI h

--Carga los tipos de los pokemons
loadTipos :: IO [Tipo]
loadTipos = do
    ficheroATK <- readFile "Data/AtkTypes.txt"
    ficheroDEF <- readFile "Data/DefTypes.txt"
    let ataques = drop 1 (lines ficheroATK)
    let defensas = drop 1 (lines ficheroDEF)
    let res = parsearTipos ataques defensas
    return res
    
loadHabilities :: IO [Habilidad]
loadHabilities =  do 
    fichero <- readFile "Data/Habilidades.txt"
    let lineas = (lines fichero)
    let res = parsearHabilidades lineas
    return res

loadPokemons :: IO()
loadPokemons = undefined