module Daño where

import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random
import System.IO

import PokemonData
import Tipo
import Pokemon
import UIColors

{-
Antes que nada, la fórmula de daño de Pokemon está guardada en un archivo .png llamada FormulaDeDaño, pero en nuestro caso
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
tiempo:
    Tiempo coge el tiempo actual en milisegundos, lo parseo (x1000) para que no sea float y sea Int para poder usarlo como semilla
    Enlace donde he cogido referencias: 
        https://stackoverflow.com/questions/42843882/how-do-you-get-a-millisecond-precision-unix-timestamp-in-haskell
-}
tiempo :: IO Int
tiempo = round . (* 1000) <$> getPOSIXTime

{-
    Una vez obtenido el tiempo en milisegundos, simplemente genero un numero aleatorio dodne la semilla sea el tiempo en milisegundo
    y compruebo, con el numeor generado entre 0 y 100, si este es menor que 10. Si lo es, entonces ha sido un ataque Crítico
-}

esCritico :: IO Bool
esCritico = do
    seed <- tiempo
    let generator = mkStdGen seed
    let (rand, _) = randomR (0::Int,100::Int) generator
    return (rand < 10)

-- E (Defensas del enemigo), P (Poder del ataque) y C (es Crítico)

--esSTAB-----------------------------------------------------------
-- Debe de volver un Double [x1, x1.5], si alguno de los dos Tipos del pokemon es el mismo que el Tipo de la Habilidad
-------------------------------------------------------------------
esSTAB :: Pokemon -> Habilidad -> Double
esSTAB (Pokemon _ (tipo1, tipo2) _ _) habilidad
    | elem tipoHabilidad [getNombreTipo tipo1, getNombreTipo tipo2] = 1.5
    | otherwise = 1.0
    where tipoHabilidad :: String
          tipoHabilidad = getTipoHabilidad habilidad


--esEficaz-------------------------------------------------------------
-- Devolverá un Double [x0,x0.5,x1,x2] si el ataque es inmmune, debil, neutro o fuerte contra el pokemon que se 
-- está utilizando dicho ataque
-- 
-- Parámetros de entrada:
--      -. Pokemon enemigo (2 tipos o ninguno)
--      -. Habilidad
-----------------------------------------------------------------------

esEficaz :: Pokemon -> Habilidad -> Double
esEficaz (Pokemon _ (tipo1, tipo2) _ _) (Habilidad _ _ _ tipoH)
    | esNull tipo1 && esNull tipo2 = error $ setColor red "El pokemon carece de tipo"
    | esNull tipo2 = getEficaciaAtaque tipoH tipo1
    | esNull tipo1 = getEficaciaAtaque tipoH tipo2
    | otherwise = (getEficaciaAtaque tipoH tipo1) * (getEficaciaAtaque tipoH tipo2)
   
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

getEficaciaAtaque :: String -> Tipo -> Double
getEficaciaAtaque tipoHabilidad tipoPokemon
    | esDebil tipoHabilidad tipoPokemon = 2.0
    | esFuerte tipoHabilidad tipoPokemon = 0.5
    | esInmune tipoHabilidad tipoPokemon = 0.0
    | otherwise = 1.0

-- Aux para comprobar si es débil
esDebil :: String -> Tipo -> Bool
esDebil h (Nombre _ (_,Defensas [Debil debilidades,_,_])) = elem h debilidades

-- Aux para comprobar si es fuerte
esFuerte :: String -> Tipo -> Bool
esFuerte h (Nombre _ (_,Defensas [_,Fuerte fortalezas,_])) = elem h fortalezas

-- Aux para comprobar si es inmune
esInmune :: String -> Tipo -> Bool
esInmune h (Nombre _ (_,Defensas [_,_,Inmune inmunidades])) = elem h inmunidades


--obtenerPotenciaHabilidad-------------------------------------------------------------
-- Devolverá un Double que será la potencia de la Habilidad
-- 
-- Estrucutra de habilidad:
--      data Habilidad = Habilidad ID Nombre Daño Nombre
-----------------------------------------------------------------------

obtenerPotenciaHabilidad :: Habilidad -> Double
obtenerPotenciaHabilidad (Habilidad _ _ x _) = fromIntegral x


------------------------------------------------------------------------------------------


esDefensivo :: Pokemon -> Habilidad -> Double
esDefensivo (Pokemon _ (tipo1, tipo2) _ _) (Habilidad _ _ _ tipoH)
    | esNull tipo1 && esNull tipo2 = error $ setColor red "El pokemon carece de tipo"
    | esNull tipo2 = getEficaciaDefensas tipoH tipo1
    | esNull tipo1 = getEficaciaDefensas tipoH tipo2
    | otherwise = (getEficaciaDefensas tipoH tipo1) * (getEficaciaDefensas tipoH tipo2)

{-
    Calcula si un tipo es defensivo contra otro Tipo. Exactamente
    recíproco a la función anterior. Pero en este caso, si el tipo 2
    es Debil, devolverá x2, y si el segudno tipo es fuerte devolverá
    x0.5, e inmunne se queda igual x0. Esto es porque el Acero, por ejemplo,
    es fuerte defendiendose contra el Normal (x0.5), pero Debil defendiendose
    contra el tipo Lucha (x2) y el veneno no afecta al Acero (x0)

            Ejemplo:
                Acero -> Normal -> x0.5       
                Acero -> Lucha -> x2                    
                Acero -> Veneno -> x0    
-}

getEficaciaDefensas :: String -> Tipo -> Double
getEficaciaDefensas h tipo2
    | esDebil' h tipo2 = 2.0
    | esFuerte' h tipo2 = 0.5
    | esInmune' h tipo2 = 0.0
    | otherwise = 1.0

-- Aux para comprobar si es débil
esDebil' :: String -> Tipo -> Bool
esDebil' h (Nombre _ (_, Ataques [Debil debilidades,_,_])) = elem h debilidades
esDebil' _ _ = False

-- Aux para comprobar si es fuerte
esFuerte' :: String -> Tipo -> Bool
esFuerte' h (Nombre _ (_, Ataques  [_,Fuerte fortalezas,_])) = elem h fortalezas
esFuerte' _ _ = False

-- Aux para comprobar si es inmune
esInmune' :: String -> Tipo -> Bool
esInmune' h (Nombre _ (_, Ataques  [_,_,Inmune inmunidades])) = elem h inmunidades
esInmune' _ _ = False