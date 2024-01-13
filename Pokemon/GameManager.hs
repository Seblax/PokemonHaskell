import System.IO
import System.Directory

import Data.Tipo
import Data.PokemonData
import Data.Pokemon

import UI.UIColors
import UI.GameUI

import Parser
import Daño
import Enemigo

main :: IO()
main = do
    menuScreen
    n <- instruccionColor "Elige un botón del menú:" yellow
    menuBehavior n


menuBehavior :: String -> IO ()
menuBehavior s
  | s == "Start" = do wantToContinue
  | s == "Load" = do putStr $ green ++ "Has seleccionado LOAD!" ++ none
  | s == "Exit" = 
    do 
    putStrLn $ setColor red "¡Hasta otra Entrenador del Ciberespacio!"
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
  seed <- instruccionColor "Añadir Seed de la partida 'XXXXXXXX-XXXXXXXX':" green

  --Obtengo la Semilla y la parseo
  let (semillaPokemon, semillaHabilidades) = parseoSemilla seed

  pokemonAliado <- loadPokemons t h semillaHabilidades
  pokemonEnemigo <- loadPokemons t h (semillaHabilidades*79)

  let team@(enemigo,_) = (getPokemonRandom pokemonEnemigo (semillaPokemon*79) ,getPokemonRandom pokemonAliado semillaPokemon)
  setBattle team ("El enemigo ha sacado a " ++ setColor red (getPokemonNombre enemigo) ++ ". ¿Qué hará nuestro entrenador?") True

{-
    Muestra la batalla por pantalla
-}
setBattle :: (Pokemon, Pokemon) -> String -> Bool -> IO()
setBattle pokemons@(p1,_) comentario turno = do
    clearScreen
    pokemonBattleUI pokemons

    endGame pokemons comentario
    
    textBox comentario

    if turno then 
        do
            eleccion <- instruccionColor yellow $ "Elige una acción: " ++ setColor red "[Atacar] " ++ setColor blue "[Cambiar]"
            if eleccion == "Atacar" then 
                do
                    setAttack pokemons "\n¿Qué ataque quieres realizar? [Back] "
                
            else 
                do 
                    setBattle pokemons ("No puedes cambiar ahora," ++ setColor red "no tienes más Pokemons" ++ ". Vaya torpe, ni contar sus pokemons sabe.") True
    else
        do
            getLine
            tipos <- loadTipos
            (comentario, newPokemons) <- generateEnemyAttack pokemons tipos
            setBattle (p1,newPokemons) comentario True


setAttack :: (Pokemon, Pokemon) -> String -> IO()
setAttack pokemons@(p1,p2) s = do   
    clearScreen
    pokemonBattleUI pokemons
    putStrLn $ habilidadesUI (getPokemonHabilidades p2)
    ataque <- instruccionColor s yellow
    input ataque
    where
        input ataque 
            | elem ataque (getPokemonNombreHabilidades p2) =
                do
                    let habilidadSelec = getPokemonHabilidadPorNombre ataque (getPokemonHabilidades p2)

                    critico <- esCritico

                    tipos <- loadTipos
                    let tipoHabilidad = getTipoPorNombre tipos (getTipoHabilidad habilidadSelec)
                    
                    let (comentario,p1) = hacerElDaño pokemons (habilidadSelec,tipoHabilidad)  critico True
                    setBattle (p1,p2) comentario False
            | ataque == "Back" = 
                do
                    setBattle pokemons "Vaya, parece que nuestro entrenador se ha arrepentido de atacar ¿De qué     otra acción se arrepentirá?" True
            | otherwise =
                do 
                    putStr red
                    readFileSprites "Ficheros/Sprites/HabilidadMalEscrita.txt"
                    putStr none
                    putStrLn "Pulsa 'Enter' para continuar"
                    getLine
                    setAttack pokemons $ "Ese ataque no sirve mamawebo " ++ "¿Qué Ataque eligirá nuestro (bobo) Entrenador?"


--End Game
endGame :: (Pokemon, Pokemon) -> String -> IO()
endGame (p1,p2) comentario
    | getPokemonVida p1 <= 0 = ganar comentario
    | getPokemonVida p2 <= 0 = perder comentario
    | otherwise = putStr "" 

ganar :: String -> IO()
ganar s = do 
    textBox $ s ++ setColor colorPlanta "\n\n¡ENHORABUENA HAS GANADO A UN BOT QUE ELIGE ATAQUES ALEATORIOS CON UNA POSIBILIDAD DEL 25% CADA ATAQUE! QUÉ MÁQUINA, TA TO ESHO UN COSINITA"
    getLine
    clearScreen
    putStr colorLucha
    readFileSprites "Ficheros/Sprites/Victoria.txt"
    putStr yellow
    readFileSprites "Ficheros/Sprites/GameOver.txt"
    putStr none
    getLine
    main

perder :: String -> IO()
perder s = do 
    textBox $ s ++ setColor colorLucha "\n\nNuestro Entrenador ha quedado totalmente fuera de combate ¡Valiente despojo Humano!"
    getLine
    clearScreen
    putStr green
    readFileSprites "Ficheros/Sprites/Perder.txt"
    putStr blue
    readFileSprites "Ficheros/Sprites/GameOver.txt"
    putStr none
    getLine
    main

--Carga los tipos de los pokemons
loadTipos :: IO [Tipo]
loadTipos = do
    tablaDeTipos <- readFile "Ficheros/TablaDeTipos.txt"
    let tipos = drop 1 (lines tablaDeTipos)
    let res = parsearTipos tipos
    return res
    
loadHabilities :: IO [Habilidad]
loadHabilities =  do 
    habilidadesSinParsear <- readFile "Ficheros/Habilidades.txt"
    let habilidades = lines habilidadesSinParsear
    let res = parsearHabilidades habilidades
    return res

loadPokemons :: [Tipo] -> [Habilidad] -> Int ->IO [Pokemon]
loadPokemons tipos habilidades seed =  do 
    pokemonsSinParsear <- readFile "Ficheros/Pokemons.txt"
    let pokemons = lines pokemonsSinParsear
    let res = parsearPokemons pokemons tipos habilidades seed
    return res