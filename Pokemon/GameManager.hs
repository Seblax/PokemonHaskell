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


{-
    Esta función sirve para preguntarle al jugador si de verdad desesa continuar y crear una nueva partida
-}
wantToContinue :: IO()
wantToContinue = do
    clearScreen
    textBox $ "Se va a generar un equipo pokemon aleatoriamente, si deseas volver atrás   simplemente escriba " ++ setColor red "Back "  ++ ", si deseas continuar escriba" ++ setColor green " Ok"
    respuesta <- instruccionColor "¿Desea crear una nueva partida?" green
    respuestaSelecionada respuesta
    where
        respuestaSelecionada :: String -> IO()
        respuestaSelecionada r 
            | r == "Ok" = generaBattle
            | r == "Back" = main
            | otherwise = wantToContinue
    

{-
    Generta los pokemons y un aseed que define que pokemon y que ataques tendrá este
-}
generaBattle :: IO ()
generaBattle = do
  clearScreen
  t <- loadTipos
  h <- loadHabilities

  textBox "La Seed sirve para generar una partida aleatoria con Pokemons y sus respec-tivas Habilidades randomizadas, para poder disfrutar de una partida única  cada vez que se añada una seed distinta. El formato de la Seed debe de ser el sifuiente xxxxxxxx-xxxxxxxx conformada por números enteros. \nPor ejemplo: 12345678-12345678"
  seed <- (instruccionColor "Añadir Seed de la partida 'XXXXXXXX-XXXXXXXX':" green)

  --Obtengo la Semilla y la parseo
  let (semillaPokemon, semillaHabilidades) = parseoSemilla seed

  pokemons <- loadPokemons t h semillaHabilidades

  let team@(enemigo,_) = get2RandomsPokemons pokemons semillaPokemon
  setBattle team $ "El enemigo ha sacado a " ++ setColor red (getPokemonNombre enemigo) ++ ". ¿Qué hará nuestro entrenador?"

{-
    Muestra la batalla por pantalla
-}
setBattle :: (Pokemon, Pokemon) -> String -> IO()
setBattle pokemons@(p1,p2) comentario= do
    clearScreen
    putStrLn ""
    pokemonBattleUI pokemons
    textBox comentario  
    eleccion <- instruccionColor yellow $ "Elige una acción: " ++ setColor red "[Atacar] " ++ setColor blue "[Cambiar]"
    if eleccion == "Atacar" then 
        do
            setAttack pokemons ""
          
    else 
        do 
            setBattle pokemons $ "No puedes cambiar ahora," ++ setColor red "no tienes más Pokemons" ++ ". Vaya torpe, ni contar sus pokemons sabe."


setAttack :: (Pokemon, Pokemon) -> String -> IO()
setAttack pokemons@(p1,p2) s = do   
    clearScreen
    pokemonBattleUI pokemons
    putStrLn $ habilidadesUI (getPokemonHabilidades p1)
    ataque <- instruccionColor "\n¿Qué ataque quieres realizar? [Back] " yellow
    input ataque
    where
        input ataque 
            | elem ataque (getPokemonNombreHabilidades p2) =
                do
                    putStrLn $ "Has elegido el ataque: " ++ ataque
            | ataque == "Back" = 
                do
                    setBattle pokemons "Vaya, parece que nuestro entrenador se ha arrepentido de atacar ¿De qué     otra acción se arrepentirá?"
            | otherwise =
                do 
                    putStrLn "Ese ataque no sirve mamawebo"
                    setAttack pokemons "¿Qué Ataque eligirá nuestro Entrenador?"

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