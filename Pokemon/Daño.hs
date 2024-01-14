module Daño (random, 
    esCritico, 
    esSTAB, 
    esEficaz,
    obtenerPotenciaHabilidad, 
    hacerElDaño, 
    getNumRandomInterval) where

import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random
import System.IO

import Data.PokemonData
import Data.Tipo
import Data.Pokemon

import UI.UIColors
import UI.GameUI

{-
Antes que nada, la fórmula de daño de Pokemon está guardada en un archivo .png llamada FormulaDeDaño, pObjetivoro en nuestro caso
ya que estamos haciendo un pokemon mucho más simplificado, nuestra formula de daño quedaría de la siguiente forma:

        #################################################

                                1.2 * P
            0.01 * B * E * ( ______________ + 2 ) * C
                                 25

        #################################################

    Donde:
        B.- Bonificación de daño si el pokemon que lo lanza es del mismo tipo que el ataque (x1.5 o x1)

        E.- Efectividad del ataque, si el ataque es bueno contra el tipo del pokemon Rival

        P.- Poder del ataque, que sería la potencia que tiene la habilidad

        C.- Crítico o no, con una posibilidad del 10%, multiplicando la potencia del ataque x2
    
    Por lo tanto, vamos a realizar una función auxiliar por cada uno de los atributos variables
    como lo son B (STAB), E (Defensas del enemigo), P (Poder del ataque) y C (es Crítico)
-}



-----------------------------------------------------------------------------------------
--      Cálculo del Crítico     --
-----------------------------------------------------------------------------------------

{-
rand:
    rand coge el rand actual en milisegundos, lo pAtacanterseo (x1000) pAtacantera que no sea float y sea Int pAtacantera poder usarlo como semilla
    Enlace donde he cogido referencias: 
        https://stackoverflow.com/questions/42843882/how-do-you-get-a-millisecond-precision-unix-timestamp-in-haskell
-}
numRandom :: IO Int
numRandom = round . (* 1000) <$> getPOSIXTime

getNumRandomInterval :: Int -> Int -> IO Int
getNumRandomInterval i1 i2 =
    do
    seed <- numRandom
    let generator = mkStdGen seed
    let (rand, _) = randomR (i1,i2) generator
    return rand

{-
    Una vez obtenido el random en milisegundos, simplemente genero un numero aleatorio dodne la semilla sea el random en milisegundo
    y compruebo, con el numeor generado entre 0 y 100, si este es menor que 10. Si lo es, entonces ha sido un ataque Crítico
-}

esCritico :: IO Bool
esCritico = do
    rand <- getNumRandomInterval 0 100
    return (rand < 10)

-- E (Defensas del enemigo), P (Poder del ataque) y C (es Crítico)

--esSTAB-----------------------------------------------------------
-- Debe de volver un Double [x1, x1.5], si alguno de los dos Tipos del pokemon es el mismo que el Tipo de la Habilidad
-------------------------------------------------------------------
esSTAB :: Pokemon -> Habilidad -> Double
esSTAB (Pokemon _ (tipo1, tipo2) _ _) habilidad
    | tipoHabilidad `elem` [getNombreTipo tipo1, getNombreTipo tipo2] = 1.5
    | otherwise = 1.0
    where tipoHabilidad :: String
          tipoHabilidad = getTipoHabilidad habilidad


--esEficaz-------------------------------------------------------------
-- Devolverá un Double [x0,x0.5,x1,x2] si el ataque es inmmune, debil, neutro o fuerte contra el pokemon que se 
-- está utilizando dicho ataque
-- 
-- pAtacanterámetros de entrada:
--      -. Pokemon enemigo (2 tipos o ninguno)
--      -. Habilidad
-----------------------------------------------------------------------

esEficaz :: Pokemon -> Tipo -> Double
esEficaz (Pokemon _ (tipo1, tipo2) _ _) tipoH
    | esNull tipo1 && esNull tipo2 = error $ setColor red "El pokemon carece de tipo"
    | esNull tipo2 = getEficaciaAtaque tipo1 tipoH
    | esNull tipo1 = getEficaciaAtaque tipo2 tipoH
    | otherwise = getEficaciaAtaque tipo1 tipoH * getEficaciaAtaque tipo2 tipoH

{-
    Calcula si un tipo es eficaz contra otro Tipo. 
    
            -Si el Tipo 1 es muy eficaz (Fuerte) contra 
            el otro tipo, devolverá un x2

            -Si el Tipo 1 es debil (Debil) contra
            el otro tipo, devolverá un x0.5

            -Si el Tipo 1 no afecta (Inmune) al otro tipo
            devolverá un x0

            Ejemplo:
                Agua -> Fuego -> x2
                Agua -> Planta -> x0.5
                Fantasma -> Normal -> x0
-}

getEficaciaAtaque :: Tipo -> Tipo -> Double
getEficaciaAtaque (Nombre n _) (Debil xs)
    | n `elem` xs = 0.5
    | otherwise = 1
getEficaciaAtaque (Nombre n _) (Fuerte xs)
    | n `elem` xs = 2
    | otherwise = 1
getEficaciaAtaque (Nombre n _) (Inmune xs)
    | n `elem` xs = 0
    | otherwise = 1
getEficaciaAtaque tPokemon (Nombre _ ataque) = getEficaciaAtaque tPokemon ataque
getEficaciaAtaque tPokemon (Ataques ts) = product [getEficaciaAtaque tPokemon t | t <- ts]






--obtenerPotenciaHabilidad-------------------------------------------------------------
-- Devolverá un Double que será la potencia de la Habilidad
-- 
-- Estrucutra de habilidad:
--      data Habilidad = Habilidad ID Nombre Daño Nombre
-----------------------------------------------------------------------

obtenerPotenciaHabilidad :: Habilidad -> Double
obtenerPotenciaHabilidad (Habilidad _ _ x _) = fromIntegral x


------------------------------------------------------------------------------------------

hacerElDaño :: (Pokemon,Pokemon) -> (Habilidad,Tipo) -> Bool -> Bool -> (String,Pokemon)
hacerElDaño (pObjetivo,pAtacante) (h,th) crit turno = (comentario,nuevoPokemonObjetivo)
        where
            comentario = generarComentario pObjetivo h (e,c) daño turno
            b = esSTAB pAtacante h
            e = esEficaz pObjetivo th
            p = obtenerPotenciaHabilidad h
            c | crit = 2
              | otherwise = 1
            daño = round (b * e * (((1.2*p)/25) + 2) * c * 2) :: Int
            nuevoPokemonObjetivo = setPokemonVida pObjetivo daño ::Pokemon