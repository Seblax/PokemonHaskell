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
import Data.Potion
import Control.DeepSeq (NFData(rnf))

--    ___                                    __  __                            __ _                  
--   / __|   __ _    _ __     ___      o O O|  \/  |  __ _    _ _     __ _    / _` |   ___      _ _  
--  | (_ |  / _` |  | '  \   / -_)    o     | |\/| | / _` |  | ' \   / _` |   \__, |  / -_)    | '_| 
--   \___|  \__,_|  |_|_|_|  \___|   TS__[O]|_|__|_| \__,_|  |_||_|  \__,_|   |___/   \___|   _|_|_  
-- _|"""""|_|"""""|_|"""""|_|"""""| {======|_|"""""|_|"""""|_|"""""|_|"""""|_|"""""|_|"""""|_|"""""| 
-- "`-0-0-'"`-0-0-'"`-0-0-'"`-0-0-'./o--000'"`-0-0-'"`-0-0-'"`-0-0-'"`-0-0-'"`-0-0-'"`-0-0-'"`-0-0-' 
--
--  Game Manager es el pegamento que lo une todo. 
-- Recomiendo ver este módulo siguiendo la máquina de estados que está en el Readme, para que
-- la comprensión y la lectura sea más amena:
--
main :: IO()
main = do
    menuScreen
    n <- instruccionColor yellow "Elige un botón del menú:"
    menuBehavior n


menuBehavior :: String -> IO ()
menuBehavior s
  | s == "Start" = do wantToContinue
  | s == "Load" =
    do
        clearScreen
        textBox "Si deseas cargar la partida solo debes escribir el nombre de la partida guardada."
        path <- instruccionColor magenta "¿Qué partida quieres cargar? "
        loadGame path
  | s == "Exit" =
    do
    putStrLn $ setColor red "¡Hasta otra Entrenador del Ciberespacio!"
    error ""
  | otherwise = main


{-
    Esta función sirve para preguntarle al jugador si de verdad desesa continuar y crear una nueva partida
-}
wantToContinue :: IO()
wantToContinue = do
    clearScreen
    textBox $ "Se va a generar un equipo pokemon aleatoriamente, si deseas volver atrás   simplemente escriba " ++ setColor red "Back "  ++ ", si deseas continuar escriba" ++ setColor green " Ok"
    respuesta <- instruccionColor green "¿Desea crear una nueva partida?"
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

  savePotions (potionSet, potionSet)

  textBox "La Seed sirve para generar una partida aleatoria con Pokemons y sus respec-tivas Habilidades randomizadas, para poder disfrutar de una partida única  cada vez que se añada una seed distinta. El formato de la Seed debe de ser el sifuiente xxxxxxxx-xxxxxxxx conformada por números enteros. \nPor ejemplo: 12345678-12345678"
  seed <- instruccionColor green "Añadir Seed de la partida 'XXXXXXXX-XXXXXXXX':"

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
setBattle pokemons@(p1,p2) comentario turno = do
    clearScreen
    pokemonBattleUI pokemons

    endGame pokemons comentario

    textBox comentario

    if turno then
        do
            eleccion <- instruccionColor yellow $ "Elige una acción: " ++ setColor red "[Attack] " ++ setColor green "[Potions] " ++ setColor colorFantasma "[Save]"
            if eleccion == "Attack" then
                do
                    setAttack pokemons $ "\n¿Qué ataque quieres realizar?" ++ setColor red " [Back]"
            else if eleccion == "Potions" then
                do
                    (pe,pa) <- loadPotions
                    pokemonBattleUI pokemons
                    textBox "Parece que neustro entrenador quiere usar una poción"
                    tomar <- instruccionColor yellow $ "¿Quieres usar una poción? " ++ setColor green "[Ok]" ++ setColor red " [No]"

                    if tomar == "Ok" then do
                        if not (pocionesVacias pa) then do
                            (pokemonCurado, comentario, pocionesA) <- potionBehavior p2 pa 

                            savePotions (pe,pocionesA)

                            setBattle (p1,pokemonCurado) comentario False
                        else 
                            do
                                setBattle pokemons ("Ooooooooooh que pena... Parece que a nuestro Entrenador " ++ setColor red "ya no le quedan más Pociones. " ++ "pobre de el, nah estaba de coña.") False
                    else 
                        do
                            setBattle pokemons ("Este entrenador me tiene echa la picha un lío " ++ setColor red "!Quieres elegir una acción ya de una vez¡") True
            else if eleccion == "Save" then
                do
                    clearScreen
                    pokemonBattleUI pokemons
                    textBox $ "Vaya vaya vaya, parece que nesutro jugador quiere tomarse un descanso y guardar lapartida, eso, o está haciendo trampillas para que no le maten. AAAY que te pillao tramposillo." ++ setColor colorSiniestro " Payaso, que eres un Payaso. "
                    path <- instruccionColor magenta "¿Cómo se va a llamar el archivo de guardado?"
                    saveGame pokemons path

            else
                do
                    setBattle pokemons "Y bueno, aquí seguimos esperando a que nuestro Entrnador eliga una acción para realizar..." True
    else
        do
            instruccionColor yellow "Pulsa Enter para continuar."

            tipos <- loadTipos
            (pe,pa) <- loadPotions

            (comentario, newPokemons,pe) <- generateEnemyAttack pokemons tipos pe

            savePotions (pe,pa)

            setBattle newPokemons comentario True

potionBehavior :: Pokemon -> Pociones -> IO(Pokemon, String, Pociones)
potionBehavior pa potions = 
    do
        let (cura, nuevasPociones) = usarPocion potions
        let nuevoPokemon = setPokemonVida pa (-cura)

        let c = comentarioCuraAliado cura

        return (nuevoPokemon, c, nuevasPociones)

setAttack :: (Pokemon, Pokemon) -> String -> IO()
setAttack pokemons@(p1,p2) s = do
    clearScreen
    pokemonBattleUI pokemons
    putStrLn $ habilidadesUI (getPokemonHabilidades p2)
    ataque <- instruccionColor yellow s
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
                    readFileSprites "Ficheros/Sprites/HabilidadMalEscrita.sprite"
                    putStr none

                    instruccionColor yellow "Pulsa Enter para continuar."

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
    readFileSprites "Ficheros/Sprites/Victoria.sprite"

    putStr yellow
    readFileSprites "Ficheros/Sprites/GameOver.sprite"
    putStr none

    instruccionColor yellow "Pulsa Enter para continuar."

    main

perder :: String -> IO()
perder s = do

    textBox $ s ++ setColor colorLucha "\n\nNuestro Entrenador ha quedado totalmente fuera de combate ¡Valiente despojo Humano!"
    getLine

    clearScreen
    putStr green
    readFileSprites "Ficheros/Sprites/Perder.sprite"

    putStr blue
    readFileSprites "Ficheros/Sprites/GameOver.sprite"
    putStr none

    instruccionColor yellow "Pulsa Enter para continuar."

    main

--Carga los tipos de los pokemons
loadTipos :: IO [Tipo]
loadTipos = do
    tablaDeTipos <- readFile "Ficheros/TablaDeTipos.pokemon"
    let tipos = drop 1 (lines tablaDeTipos)
    let res = parsearTipos tipos
    return res

loadHabilities :: IO [Habilidad]
loadHabilities =  do
    habilidadesSinParsear <- readFile "Ficheros/Habilidades.pokemon"
    let habilidades = lines habilidadesSinParsear
    let res = parsearHabilidades habilidades
    return res

loadPokemons :: [Tipo] -> [Habilidad] -> Int ->IO [Pokemon]
loadPokemons tipos habilidades seed =  do
    pokemonsSinParsear <- readFile "Ficheros/Pokemons.pokemon"
    let pokemons = lines pokemonsSinParsear
    let res = parsearPokemons pokemons tipos habilidades seed
    return res

-------------------------------------------------------------------------
saveGame :: (Pokemon,Pokemon) -> String -> IO()
saveGame pokemons@(penemigo,paliado) nombre =
    do
        let path = "Save/" ++ nombre ++ ".pokemon"
        (pocionesE,pocionesA) <- loadPotions

        existe <- doesFileExist path
        if existe then 
            do
                pokemonBattleUI pokemons
                textBox $ "Oh no, parece que el archivo de nombre " ++ setColor yellow nombre ++ " ya existe. ¿Querrá nuestro Entrenador sobrescribirlo?"
                sobresciribir <- instruccionColor red2 $ "¿Desea sobrescribir la partida Guardada?" ++ setColor green " [Ok] " ++ setColor blue "[No]" 
                if sobresciribir == "Ok" then do
                    writeFile path (savePokemon paliado ++ "\n" ++ savePokemon penemigo ++ "\n" ++ show pocionesE ++ "\n" ++ show pocionesA)
                    main
                else do
                    setBattle pokemons "Ni guardar la partida sabes, que penoso. Espero que luchar con los Pokemons se te de algo mejor" True
        else 
            do
                writeFile path (savePokemon paliado ++ "\n" ++ savePokemon penemigo ++ "\n" ++ show pocionesE ++ "\n" ++ show pocionesA)
                main
loadGame :: String -> IO()
loadGame nombre =
    do
        let path = "Save/" ++ nombre ++ ".pokemon"

        existe <- doesFileExist path
        if existe then
            do
                partida <- readFile path
                tipos <- loadTipos
                habilidades <- loadHabilities

                let pokemons = lines partida
                let pokemonAliado = loadPokemonSave (head pokemons) habilidades tipos
                let pokemonEnemigo = loadPokemonSave (pokemons!!1) habilidades tipos

                writeFile "Ficheros/Pociones.pot" $ pokemons!!2 ++ "\n" ++ pokemons!!3

                setBattle (pokemonEnemigo,pokemonAliado) "Vaya, parece que acabamos de ser una partida cargada, me pregunto por qué neustro entrenador dejó la partida a medias" True
        else
            do
                clearScreen
                textBox $ "No existe la partida gaurdada con nombre: '" ++ setColor red nombre ++ "'."
                instruccionColor yellow "Pulsa Enter para continuar."
                main

-------------------------------------------------------

savePotions :: (Pociones,Pociones) -> IO()
savePotions (pe,pa) = do
    writeFile "Ficheros/Pociones.pot" $ show pe ++ "\n" ++ show pa

loadPotions :: IO (Pociones,Pociones)
loadPotions = do
    fichero <- openFile "Ficheros/Pociones.pot" ReadMode
    pocionesStr <- hGetContents fichero

    -- putStrLn pocionesStr
    -- getLine

    let lineas = lines pocionesStr
    let listaPociones = parsearPocion lineas

    rnf pocionesStr `seq` hClose fichero

    return (head listaPociones, last listaPociones)

    where
        parsearPocion xs = [parsePotion x | x <-xs]