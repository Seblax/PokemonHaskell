module UI.GameUI where

import Data.Char
import Data.PokemonData
import System.Directory
import System.IO
import UI.UIColors
import Data.Tipo
import Data.Pokemon

type Screen = String

justifyRight :: Int -> a -> [a] -> [a]
justifyRight n c s = replicate (n - length s) c ++ s

menuScreen :: IO ()
menuScreen = do
  clearScreen
  putStr blue
  readFileSprites "Ficheros/Sprites/Menu.sprite"
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
      boxes = setColor blue "###########################################################################"
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
generarComentario :: Pokemon -> Habilidad -> (Double, Double) -> Int -> Bool -> String
generarComentario p h d daño turno = "Nuestro entrenador ha usado " ++  setColorHabilidad h ++ " contra " ++ getPokemonNombre p ++ " y ha hecho " ++ setColor red (show daño ++ " de daño. ") ++ comentario
  where
    comentario 
      | turno = generarComentarioAliado d
      | otherwise = generarComentarioEnemigo d

generarComentarioAliado :: (Double, Double) -> String
generarComentarioAliado (e,c)  = eficaz ++ critico
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


generarComentarioEnemigo :: (Double, Double) -> String
generarComentarioEnemigo (e,c) = eficaz ++ critico
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

comentarioCuraEnemigo :: Int -> String 
comentarioCuraEnemigo cura 
  | cura >= 50 = "¡BIEN! Ejem, digo, OH NO, el enemigo ha usado una poción de " ++ setColor green (show cura ++ " de hp") ++ " y a restaurado toda su vida practicamente de un golpe. Que mal... (Espero que tenga otra pociónmás)"
  | cura <= 50 && cura >= 35 ="AY AY AYYYY QUE PENA QUE EL ENEMIGO TENÍA OTRA POCIÓN. AY AY AYYY, se ha curado cerca de " ++ setColor green (show cura ++ " de hp") 
  | otherwise ="¡EL ENEMIGO A USADO OTRA POCIÓN MENOS MAL QUE ESA NO ERA LA ÚLTIMA! \n" ++  setColor colorHielo "Jimmy: " ++ "De hecho Comentalista, esa era su ultima poción de " ++ setColor green (show cura ++ " de hp") ++ " \n" ++ setColor red "Comentalista: " ++ "Pero... Os dije que le dieráis más pociones, máaaas pociones, he apostado la letra del coche a que ganaba el otro."

comentarioCuraAliado :: Int -> String 
comentarioCuraAliado cura 
  | cura >= 50 ="El combate está siendo tan duro y difícil que nuestro Entrenador ha tenido que usar una cura. Ha gastado la poción de " ++ setColor green (show cura ++ " de hp") ++ " ya tan solo le quedan un par de recursos más."
  | cura <= 50 && cura >= 35 ="Madre mía, ya es la segunda poción que usa, no escatimas en gastos el menda este, pero vamos, no debería costarle tanto una batalla tan amateur. Ha gastado su pocion de " ++ setColor green (show cura ++ " de hp") ++ " ya tan solo le queda una :O"
  | otherwise ="¡OH NO! Nuestro Entrenador ha gastado la última cura " ++ setColor green (show cura ++ " de hp") ++ " que le quedaba, verémos cual será el desenlace de esta trágica historia."
