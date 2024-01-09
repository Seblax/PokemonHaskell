import System.IO
import System.Directory

import Tipo
import PokemonData
import UIColors

import Parser
import GameUI

import Pokemon

main :: IO()
main = do
    menuScreen
    --Input del jugador
    n <- instruccionColor "Elige un botón del menú:" yellow
    menuBehavior n


menuBehavior :: String -> IO ()
menuBehavior s
  | s == "Start" = do wantToContinue
  | s == "Load" = do putStr $ green ++ "Has seleccionado LOAD!" ++ none
  | s == "Exit" = do putStr $ red ++ "¡Hasta otra entrenador!" ++ none
  | otherwise = main

wantToContinue :: IO()
wantToContinue = do
    clearScreen
    textBox $ "Se va a generar un equipo pokemon aleatoriamente, si deseas volver atrás   simplemente escriba " ++ red ++  "Back "++ none  ++ ", si deseas continuar escriba" ++  green ++" Ok" ++ none
    respuesta <- instruccionColor "¿Desea crear una nueva partida?" green
    respuestaSelecionada respuesta
    where
        respuestaSelecionada :: String -> IO()
        respuestaSelecionada r 
            | r == "Ok" = generaBattle
            | r == "Back" = main
            | otherwise = wantToContinue
    


generaBattle :: IO ()
generaBattle = do
  clearScreen
  t <- loadTipos
  h <- loadHabilities

  textBox "La Seed sirve para generar una partida aleatoria con Pokemons y sus respec-tivas Habilidades randomizadas, para poder disfrutar de una partida única  cada vez que se añada una seed distinta. El formato de la Seed debe de ser el sifuiente xxxxxxxx-xxxxxxxx conformada por números enteros. \nPor ejemplo: 12345678-12345678"
  seed <- (instruccionColor "Añadir Seed de la partida 'XXXXXXXX-XXXXXXXX':" green)
  let (semillaPokemon, semillaHabilidades) = parseoSemilla seed

  pokemons <- loadPokemons t h semillaHabilidades
  
  pokemonBattleUI (get2RandomsPokemons pokemons semillaPokemon)
  putStrLn $ habilidadesUI (getPokemonHabilidades (head pokemons))

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

loadPokemons :: [Tipo] -> [Habilidad] -> Int ->IO [Pokemon]
loadPokemons tipos habilidades seed =  do 
    fichero <- readFile "Data/Pokemons.txt"
    let lineas = (lines fichero)
    let res = parsearPokemons lineas tipos habilidades seed
    return res