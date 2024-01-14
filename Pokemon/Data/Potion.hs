module Data.Potion (
    Pociones, 
    potionSet, 
    usarPocion, 
    pocionesVacias, 
    parsePotion) where

import Data.Pila.PilaTA
import Parser

type Pociones = Pila Int

potionSet :: Pociones
potionSet = foldr apila vacia [50,35,20]

usarPocion :: Pociones -> (Int, Pociones)
usarPocion p
    | esVacia p = (0, vacia)
    | otherwise = (cima p, desapila p)

pocionesVacias :: Pociones -> Bool
pocionesVacias = esVacia


-----------------------------------------------
parsePotion :: String -> Pociones
parsePotion s = parseo lista
    where
        lista = splitText (=='|') s
        parseo (x:xs) 
            | x == "-" = vacia
            | otherwise = apila (read x) (parseo xs) 
        parseo [] = vacia