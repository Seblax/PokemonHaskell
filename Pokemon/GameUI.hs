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

justifyRight :: Int -> a -> [a] -> [a]
justifyRight n c s = replicate (n - length s) c ++ s

menuScreen :: IO ()
menuScreen = do
  putStr blue
  readFileSprites "Data/Menu.txt"
  putStr none

  putStrLn (justifyRight 52 ' ' (button "Start" yellow))
  putStrLn (justifyRight 55 ' ' (button "Load" green))
  putStrLn (justifyRight 58 ' ' (button "Exit" red))
  n <- instruccion "Elige un botón del menú:"
  menuBehavior n

readFileSprites :: String -> IO ()
readFileSprites path = do
  xs <- fmap lines $ readFile path
  mapM_ putStrLn xs

menuBehavior :: String -> IO ()
menuBehavior s
  | s == "Start" = do batalla
  | s == "Load" = do putStr $ green ++ "Has seleccionado LOAD!" ++ none
  | s == "Exit" = do putStr $ red ++ "¡Hasta otra entrenador!" ++ none
  | otherwise = menuScreen

batalla :: IO ()
batalla = do
  clearScreen
  pokemonBattleShow [squirtle, charizard]
  pokemonBattleShow [charizard, squirtle]
  putStr (ataques [ataque1,ataque2,ataque3,ataque4])

instruccion :: String -> IO String
instruccion s = do
  putStrLn (yellow ++ s ++ none)
  getLine

button :: String -> Color -> String
button s c = blue ++ "-=" ++ c ++ s ++ blue ++ "=-" ++ none

ataques :: [Habilidad] -> String
ataques [] = ""
ataques (a1 : a2 : xs) = boxes ++ boxesAtaques ++ boxesTipo ++ boxes ++ ataques xs
  where
    boxes = colorAtaque a1 ++ "\n\t########################\t" ++ none ++ colorAtaque a2 ++ "########################\n" ++ none
    boxesAtaques = colorAtaque a1 ++ getAtaque a1 ++ colorAtaque a2 ++ getAtaque a2
    boxesTipo = colorAtaque a1 ++ "\n\t#  " ++ getTipoHabilidad a1 ++ "\t\t" ++ colorAtaque a2 ++  "\t#  " ++ getTipoHabilidad a2 ++ "\t\t"
    getAtaque (Habilidad i n _ _) = "\t#  " ++ n ++ "\t\t"
    colorAtaque (Habilidad _ _ _ tipo) = setColorTipo tipo


-------------------------------------------
-------------------------------------------
clearScreen :: IO()
clearScreen = putStr clear

-------------------------------------------
-------------------------------------------
pokemonBattleShow :: [Pokemon] -> IO()
pokemonBattleShow ps = do
  pokemonShow (last ps) 100
  pokemonShow (head ps) 0

pokemonShow :: Pokemon -> Int -> IO ()
pokemonShow (Pokemon n (t1, t2) hp _) i = do
  putStrLn (justifyRight i ' ' "###################")
  putStrLn (justifyRight i ' ' n)
  putStrLn (justifyRight (i-8) ' ' ("HP: " ++ show hp) ++ tiposDelPokemon)
  putStrLn (justifyRight i ' ' "###################")
  where
    tiposDelPokemon :: String
    tiposDelPokemon = " [" ++ setTipoColorPokemonBatalla t1 ++ "-" ++ setTipoColorPokemonBatalla t2 ++ "]"