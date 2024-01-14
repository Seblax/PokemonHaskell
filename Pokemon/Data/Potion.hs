module Data.Potion (Pociones, potionSet, usarPocion, pocionesVacias) where

import Data.Pila.PilaTA

type Pociones = Pila Int

potionSet :: Pociones
potionSet = foldr apila vacia [50,35,20]

usarPocion :: Pociones -> (Int, Pociones)
usarPocion p
    | esVacia p = (0, vacia)
    | otherwise = (cima p, desapila p)

pocionesVacias :: Pociones -> Bool
pocionesVacias = esVacia