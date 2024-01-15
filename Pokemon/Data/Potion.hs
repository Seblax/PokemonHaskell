module Data.Potion (
    Pociones, 
    potionSet, 
    usarPocion, 
    pocionesVacias, 
    parsePotion) where

import Data.Pila.PilaTA
import Parser

--  _______  _______  _______ _________ _______  _        _______  _______ 
-- (  ____ )(  ___  )(  ____ \\__   __/(  ___  )( (    /|(  ____ \(  ____ \
-- | (    )|| (   ) || (    \/   ) (   | (   ) ||  \  ( || (    \/| (    \/
-- | (____)|| |   | || |         | |   | |   | ||   \ | || (__    | (_____ 
-- |  _____)| |   | || |         | |   | |   | || (\ \) ||  __)   (_____  )
-- | (      | |   | || |         | |   | |   | || | \   || (            ) |
-- | )      | (___) || (____/\___) (___| (___) || )  \  || (____/\/\____) |
-- |/       (_______)(_______/\_______/(_______)|/    )_)(_______/\_______)
--
-- Un módulo centrado en el tratamiento y gestión del tipo de datos Pocion (Pila)
-----------------------------------------------------------------------------------

--Pociones-------------------------------------------------------------------------
-- Definición del sinónimo Pociones con una Pila Int
-----------------------------------------------------------------------------------
type Pociones = Pila Int

--Potion Set-----------------------------------------------------------------------
-- Variable que contiene el set de pociones con la que todo entrenador empieza:
--      potionSet = [50|35|20|-]
-----------------------------------------------------------------------------------
potionSet :: Pociones
potionSet = foldr apila vacia [50,35,20]

--Usar Pocion-----------------------------------------------------------------------
-- Usar pocion recive como parámetro de entrada Pociones y devuelve una tupla (Int, Pociones)
-- donde el Int que devuelve es la cantidad de vida que cura la poción, y las Pociones
-- que devuelve son las Pociones que reciven como parámetro de entrada pero restandole
-- la poción usada (cima)
-----------------------------------------------------------------------------------
usarPocion :: Pociones -> (Int, Pociones)
usarPocion p
    | esVacia p = (0, vacia)
    | otherwise = (cima p, desapila p)

--Pociones Vacias------------------------------------------------------------------
-- Comprueba si Pociones está vacía o no 
-----------------------------------------------------------------------------------
pocionesVacias :: Pociones -> Bool
pocionesVacias = esVacia


--------------------------------------------------------------------------------------------------------------------------------------------------------
--Parse Potions--------------------------------------------------------------------
-- Reciviendo un String lo parsea y devuelve la respectivas Pociones 
-----------------------------------------------------------------------------------
parsePotion :: String -> Pociones
parsePotion s = parseo lista
    where
        lista = splitText (=='|') s
        parseo (x:xs) 
            | x == "-" = vacia
            | otherwise = apila (read x) (parseo xs) 
        parseo [] = vacia