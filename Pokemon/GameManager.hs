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
    menuBehavior n habilidades tipos


menuBehavior :: String -> [Habilidad] -> [Tipo] -> IO ()
menuBehavior s h t
  | s == "Start" = do wantToContinue h t
  | s == "Load" = do putStr $ green ++ "Has seleccionado LOAD!" ++ none
  | s == "Exit" = do putStr $ red ++ "¡Hasta otra entrenador!" ++ none
  | otherwise = main

wantToContinue :: [Habilidad] -> [Tipo] -> IO()
wantToContinue h t = do
    clearScreen
    putStrLn "##########################################################################"
    putStrLn $ "Se va a generar un equipo pokemon aleatoriamente, si deseas volver atrás \n simplemente escriba " ++ red ++  "'Atrás' "++ none  ++ ", si deseas continuar escriba" ++  green ++" 'Ok'" ++ none
    putStrLn "##########################################################################"
    respuesta <- instruccionColor "¿Desea generar tus nuevos Pokemons?" green
    respuestaSelecionada respuesta
    where
        respuestaSelecionada :: String -> IO()
        respuestaSelecionada r 
            | r == "Ok" = batalla h t
            | r == "Atrás" = putStr "Has dicho Atrás"
            | otherwise = wantToContinue h t
    


batalla :: [Habilidad] -> [Tipo] -> IO ()
batalla h t = do
  clearScreen
  pokemons <- loadPokemons t h
  --Coger ahora 4 pokemons Randoms
  pokemonBattleUI [head pokemons, last pokemons]
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

loadPokemons :: [Tipo] -> [Habilidad] ->IO [Pokemon]
loadPokemons tipos habilidades =  do 
    fichero <- readFile "Data/Pokemons.txt"
    let lineas = (lines fichero)
    seed <- (instruccionColor "Añadir Seed de la partida (formato INT):" green)
    let res = parsearPokemons lineas tipos habilidades (read seed)
    return res