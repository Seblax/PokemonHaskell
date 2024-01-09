module GameUI where

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
  clearScreen
  putStr blue
  readFileSprites "Data/Menu.txt"
  putStr none

  putStrLn (justifyRight 52 ' ' (buttonUI "Start" yellow))
  putStrLn (justifyRight 55 ' ' (buttonUI "Load" green))
  putStrLn (justifyRight 58 ' ' (buttonUI "Exit" red))

readFileSprites :: String -> IO ()
readFileSprites path = do
  xs <- fmap lines $ readFile path
  mapM_ putStrLn xs

-------------------------------------------
-------------------------------------------

instruccionColor :: String -> Color -> IO String
instruccionColor s c = do
  putStrLn (c ++ s ++ none)
  getLine

-------------------------------------------
-------------------------------------------

buttonUI :: String -> Color -> String
buttonUI s c = blue ++ "-=" ++ c ++ s ++ blue ++ "=-" ++ none

-------------------------------------------
-------------------------------------------

habilidadesUI :: [Habilidad] -> String
habilidadesUI [] = ""
habilidadesUI (a1 : a2 : xs) = boxes ++ boxeshabilidadesUI ++ boxesTipo ++ boxes ++ habilidadesUI xs
  where
    boxes = colorAtaque a1 ++ "\n\t########################\t" ++ none ++ colorAtaque a2 ++ "########################\n" ++ none
    boxeshabilidadesUI = colorAtaque a1 ++ getAtaque a1 ++ colorAtaque a2 ++ getAtaque a2
    boxesTipo = colorAtaque a1 ++ "\n\t#  " ++ getTipoHabilidad a1 ++ "\t\t" ++ colorAtaque a2 ++  "\t#  " ++ getTipoHabilidad a2 ++ "\t\t"
    getAtaque (Habilidad i n _ _) = "\t#  " ++ n ++ "\t\t"
    colorAtaque (Habilidad _ _ _ tipo) = setColorTipo tipo


-------------------------------------------
-------------------------------------------
clearScreen :: IO()
clearScreen = putStr clear

-------------------------------------------
-------------------------------------------
pokemonBattleUI :: (Pokemon,Pokemon) -> IO()
pokemonBattleUI (p1,p2) = do
  clearScreen
  pokemonShow p1 100
  pokemonShow p2 0

pokemonShow :: Pokemon -> Int -> IO ()
pokemonShow (Pokemon n (t1, t2) hp _) i = do
  putStrLn (justifyRight i ' ' "###################")
  putStrLn (justifyRight i ' ' n)
  putStrLn (justifyRight (i-8) ' ' ("HP: " ++ show hp) ++ tiposDelPokemon)
  putStrLn (justifyRight i ' ' "###################")
  where
    tiposDelPokemon :: String
    tiposDelPokemon = " [" ++ setTipoColorPokemonBatalla t1 ++ "-" ++ setTipoColorPokemonBatalla t2 ++ "]"

-------------------------------------------
-------------------------------------------
textBox :: String -> IO()
textBox s = do
  putStrLn boxes
  textoSplit s
    where 
      boxes = "###########################################################################"
      textoSplit :: String -> IO()
      textoSplit s 
        | length s > 75 = do
          putStrLn (take 75 s)
          textoSplit (drop 75 s)
        | otherwise = do 
          putStrLn s 
          putStrLn boxes
