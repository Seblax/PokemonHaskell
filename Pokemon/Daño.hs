import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random
import System.IO

import PokemonData
import Tipo
import Pokemon

{-
Antes que nada, la fórmula de daño de Pokemon está guardada en un archivo .png llamada FormulaDeDaño, pero en nuestro caso
ya que estamos haciendo un pokemon mucho más simplificado, nuestra formula de daño quedaría de la siguiente forma:

        #################################################

                                1.2 * P
            0.01 * B * E * ( ______________ + 2 ) * C
                                25 * D

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
tiempo =  (round . (* 1000)) <$> getPOSIXTime

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
esEficaz (Pokemon _ (tipo1, tipo2) _ _) habilidad =
    (getEficaciaHabilidad tipo1 habilidad) * (getEficaciaHabilidad tipo2 habilidad)


--Aux para calcular la eficacia de una habilidad respecto al tipo de ataque del pokemon enemigo
getEficaciaHabilidad :: Tipo -> Habilidad -> Double
getEficaciaHabilidad tipo habilidad
    | habDebil tipo habilidad = 0.5
    | habFuerte tipo habilidad = 2.0
    | habInmune tipo habilidad = 0.0
    | otherwise = 1.0

--Aux para comprobar que la habilidad sea débil al pokemon enemigo
habDebil :: Tipo -> Habilidad -> Bool
habDebil (Debil debilidades) (Habilidad _ _ _ h) = elem h debilidades
habDebil _ _ = False
    

--Aux para comprobar que la habilidad sea fuerte al pokemon enemigo
habFuerte :: Tipo -> Habilidad -> Bool
habFuerte (Fuerte fortalezas) (Habilidad _ _ _ h) = elem h fortalezas
habFuerte _ _ = False
    

--Aux para comprobar que la habilidad sea fuerte al pokemon enemigo
habInmune :: Tipo -> Habilidad -> Bool
habInmune (Inmune inmunidades) (Habilidad _ _ _ h) = elem h inmunidades
habInmune _ _ = False
   
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

getEficaciaAtaques :: Tipo -> Tipo -> Double
getEficaciaAtaques tipo1 tipo2
    | esDebil tipo1 tipo2 = 0.5
    | esFuerte tipo1 tipo2 = 2.0
    | esInmune tipo1 tipo2 = 0.0
    | otherwise = 1.0

-- Aux para comprobar si es débil
esDebil :: Tipo -> Tipo -> Bool
esDebil (Nombre _ (_, Ataques [Debil debilidades,_,_])) (Nombre nom _) = elem nom debilidades
esDebil _ _ = False

-- Aux para comprobar si es fuerte
esFuerte :: Tipo -> Tipo -> Bool
esFuerte (Nombre _ (_, Ataques [_,Fuerte fortalezas,_])) (Nombre nom _) = elem nom fortalezas
esFuerte _ _ = False

-- Aux para comprobar si es inmune
esInmune :: Tipo -> Tipo -> Bool
esInmune (Nombre _ (_, Ataques [_,_,Inmune inmunidades])) (Nombre nom _) = elem nom inmunidades
esInmune _ _ = False


--obtenerPotenciaHabilidad-------------------------------------------------------------
-- Devolverá un Double que será la potencia de la Habilidad
-- 
-- Estrucutra de habilidad:
--      data Habilidad = Habilidad ID Nombre Daño Nombre
-----------------------------------------------------------------------

obtenerPotenciaHabilidad :: Habilidad -> Double
obtenerPotenciaHabilidad (Habilidad _ _ x _) = fromIntegral x