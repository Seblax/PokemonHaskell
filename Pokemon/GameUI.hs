module GameUI where

import Data.Char
import PokemonData
import System.Directory
import System.IO
import UIColors
import Tipo
import Pokemon

type Screen = String

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

--Para mostrar la imagen del principio
readFileSprites :: String -> IO ()
readFileSprites path = do
  xs <- fmap lines $ readFile path
  mapM_ putStrLn xs

-------------------------------------------
-- Obtiene le input del jugador, además permite modificar el color del texto
-------------------------------------------

instruccionColor :: String -> Color -> IO String
instruccionColor s c = do
  putStrLn (c ++ s ++ none)
  getLine

-------------------------------------------
-- Botones del menú
-------------------------------------------

buttonUI :: String -> Color -> String
buttonUI s c = blue ++ "-=" ++ c ++ s ++ blue ++ "=-" ++ none

-------------------------------------------
-- Muestra las habilidades de los pokemons por pantalla
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
-- Limpia la Pantalla por completo
-------------------------------------------
clearScreen :: IO()
clearScreen = putStr clear

-------------------------------------------
--Muestra los Pokemons en pantalla
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
    tiposDelPokemon 
      | esNull t2 = " [" ++ setTipoColorPokemonBatalla t1 ++ "]"
      | otherwise = " [" ++ setTipoColorPokemonBatalla t1 ++ "-" ++ setTipoColorPokemonBatalla t2 ++ "]"

-------------------------------------------
--Sirve para poner las cajas de texto de dialogos
--"###########################################################################"
-- Esto es un Ejemplo de diálogo
--"###########################################################################"
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


-------------------------------------------
-- generarComentario pa h (e,d,c)
generarComentario :: Pokemon -> Habilidad -> (Double, Double) -> String
generarComentario p h (e,c) = "Nuestro entrenador ha usado " ++  setColorHabilidad h ++ " contra " ++ getPokemonNombre p ++ ". " ++ eficaz ++ critico
  where
    eficaz 
      | e == 4 = "Madre mía, el ataque ha dejado temblando al oponente, ha sido un x4... ¡ES SUPER EFICAZ! "
      | e == 2 = "Que buen ataque, como sabe nuestro entrenador las debilidades de su oponente, ha hecho un ataque muy eficaz. "
      | e == 0.5 = "¡Vaya, no sé que le  habrá dado a nuestro entrenador usando ese ataque poco eficaz contra su oponente! "
      | e == 0.25 = "¡El oponente no ha sentido ni cosquillas con ese ataque tan débil! "
      | e == 0 = "Enserio... Le has hecho un ataque al cual tu oponente es inmune... ¡ESPABILA! "
      |otherwise = "Un ataque bastante normal, ha optado por lo seguro nuestro Entrenador. "
    critico 
      | c == 2 && e /= 0 = setColor red "¡Ha sido un golpe crítico! "
      | otherwise = ""


generarComentarioEnemigo :: Pokemon -> Habilidad -> (Double, Double) -> String
generarComentarioEnemigo p h (e,c) = "El pokemon enemigo ha usado " ++  setColorHabilidad h ++ " sobre tu " ++ getPokemonNombre p ++ ". " ++ eficaz ++ critico
  where
    eficaz 
      | e == 4 = "¡DIOS! un x4 a tu pokemon, suerte si ha sobrevivido a eso... "
      | e == 2 = "¡Qué buen ataque! Es un ataque muy eficaz contra tu pokemon. "
      | e == 0.5 = "El Pokemon de nuestro entrenador ha aguantado el ataque sin mucho risego. "
      | e == 0.25 = "¡NI SE HA INMUTADO! Cómo aguanta este pokemon. "
      | e == 0 = "¿Inmunne? Oh venga ya, prestad más atención a los tipos, señores. "
      |otherwise = "Ha sido un ataque neutral para tu pokemon. "
    critico 
      | c == 2 && e /= 0 = setColor red " Vaya hostión Willy, le ha dao un crítico a tu pokemon. "
      | otherwise = ""