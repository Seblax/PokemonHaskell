{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module GameUI (menuScreen) where

import Data.Char
import PokemonData
import System.Directory
import System.IO
import UIColors
import Tipo

type Screen = String

replace :: (Eq a) => a -> a -> [a] -> [a]
replace a b = map $ \c -> if c == a then b else c

justifyLeft, justifyRight :: Int -> a -> [a] -> [a]
justifyLeft n c s = s ++ replicate (n - length s) c
justifyRight n c s = replicate (n - length s) c ++ s

menuScreen :: IO ()
menuScreen = do
  putStr blue
  readFileSpritesLn "Data/Menu.txt"
  putStr none

  putStrLn (justifyRight 52 ' ' (button "Start" yellow))
  putStrLn (justifyRight 55 ' ' (button "Load" green))
  putStrLn (justifyRight 58 ' ' (button "Exit" red))
  n <- instruccion "Elige un botón del menú:"
  menuBehavior n

readFileSpritesLn :: String -> IO ()
readFileSpritesLn path = do
  xs <- fmap lines $ readFile path
  mapM_ putStrLn xs

readFileSprites :: String -> IO ()
readFileSprites path = do
  xs <- fmap lines $ readFile path
  mapM_ putStr xs

menuBehavior :: String -> IO ()
menuBehavior s
  | s == "Start" = do battle
  | s == "Load" = do putStr $ green ++ "Has seleccionado LOAD!" ++ none
  | s == "Exit" = do putStr $ red ++ "¡Hasta otra entrenador!" ++ none
  | otherwise = menuScreen

battle :: IO ()
battle = do
  pokemonShow squirtle 100
  pokemonShow charizard 0
  putStr (ataques [ataque1,ataque2,ataque3,ataque4])

instruccion :: String -> IO String
instruccion s = do
  putStrLn (yellow ++ s ++ none)
  getLine

button :: String -> Color -> String
button s c = blue ++ "-=" ++ c ++ s ++ blue ++ "=-" ++ none

-- loadScreen :: IO()
-- loadScreen = do
--     let ataqueList = setColorHabilidades ataques tipoAgua ++ "\n" ++ setColorHabilidades ataques tipoPlanta ++ "\n" ++ setColorHabilidades ataques tipoFuego ++ "\n" ++ setColorHabilidades ataques tipoFantasma
--     putStrLn ataqueList

ataques :: [Habilidad] -> String
ataques [] = ""
ataques (a1 : a2 : xs) = boxes ++ boxesAtaques ++ boxesTipo ++ boxes ++ ataques xs
  where
    boxes = colorAtaque a1 ++ "\n\t########################\t" ++ none ++ colorAtaque a2 ++ "########################\n" ++ none
    boxesAtaques = colorAtaque a1 ++ getAtaque a1 ++ colorAtaque a2 ++ getAtaque a2
    boxesTipo = colorAtaque a1 ++ "\n\t#  " ++ getTipoAtaque a1 ++ "\t\t" ++ colorAtaque a2 ++  "\t#  " ++ getTipoAtaque a2 ++ "\t\t"
    getAtaque (Habilidad i n _ _) = "\t#  " ++ n ++ "\t\t"
    colorAtaque (Habilidad _ _ _ tipo) = setColorHabilidades tipo

setColorHabilidades :: String -> String
setColorHabilidades s
  | s == "Acero" = colorAcero
  | s == "Agua" = colorAgua
  | s == "Bicho" = colorBicho
  | s == "Dragon" = colorDragón
  | s == "Electrico" =colorEléctrico
  | s == "Fantasma" = colorFantasma
  | s == "Fuego" = colorFuego
  | s == "Hada" = colorHada
  | s == "Hielo" = colorHielo
  | s == "Lucha" = colorLucha
  | s == "Normal" = colorNormal
  | s == "Planta" = colorPlanta
  | s == "Psiquico" = colorPsíquico
  | s == "Piedra" = colorRoca
  | s == "Siniestro" =colorSiniestro
  | s == "Tierra" = colorTierra
  | s == "Veneno" = colorVeneno
  | s == "Volador" = colorVolador
  | otherwise = green

pokemonShow :: Pokemon -> Int -> IO ()
pokemonShow (Pokemon n (t1, t2) hp _) i = do
  putStrLn (justifyRight i ' ' "###################")
  putStrLn (justifyRight i ' ' n)
  putStrLn (justifyRight i ' ' ("HP: " ++ show hp ++ " [" ++ getTipo t1 ++ getTipo t2 ++ "]"))
  putStrLn (justifyRight i ' ' "###################")
