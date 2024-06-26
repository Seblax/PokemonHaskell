module Parser (
    parsearHabilidades, 
    parsearTipos, 
    parsearPokemons, 
    parseoSemilla, 
    getPokemonRandom,
    savePokemon,
    loadPokemonSave,
    splitText) where

import Data.PokemonData
import Data.Tipo
import Data.List

import UI.UIColors
import Data.Pokemon

import System.Random
-----------------------------------------------------------------------------
-- ██████╗  █████╗ ██████╗ ███████╗███████╗██████╗ 
-- ██╔══██╗██╔══██╗██╔══██╗██╔════╝██╔════╝██╔══██╗
-- ██████╔╝███████║██████╔╝███████╗█████╗  ██████╔╝
-- ██╔═══╝ ██╔══██║██╔══██╗╚════██║██╔══╝  ██╔══██╗
-- ██║     ██║  ██║██║  ██║███████║███████╗██║  ██║
-- ╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝
-- Módulo centrado en parsear los tipos de archivos .pokemons a datos hábiles
------------------------------------------------------------------------------

{-
    Función que separa un String en función de un Predicado dado.
    Enlaces de donde he sacado las fuentes para entender el código:
        - https://stackoverflow.com/questions/4978578/how-to-splitText-a-string-in-haskell
        - http://zvon.org/other/haskell/Outputprelude/break_f.html
        - http://zvon.org/other/haskell/Outputsyntax/caseQexpressions_reference.html
-}
splitText :: (Char -> Bool) -> String -> [String]   --Devuelve una lista de Strings
splitText p s =  case dropWhile p s of              --Actua como un Switch              
                      "" -> []                      --Si estamos en caso base añadimos una Lista vacía
                      s' -> w : splitText p xs      --Si no, concatenamos w por la izquierda y realizamos llamada recursiva
                            where (w, xs) = break p s' 
                                                    -- Break crea una tupla de 2 listas separadas por una condicion
                                                    -- w lista resultante, minetras que xs es el resto




{-
Para parsear los datos vamos a usar la siguiente estrategia, tenemos el ataque de 
cada tipo en el fichero:
        - TablaDeTipos.pokemon

El documento está ectructurado exactamente de la siguiente forma, cada línea de texto
representa un tipo Pokémon, estas a su vez están divididas en tabulaciones para diferenciar
entre el nombre, debilidades, fortalezas e inmunidades. Otra cosa a tener en cuenta
es que los tipos están ordenados, la línea 1 por ejemplo
es la que pertenece al tipo Acero. Quedando la estructura de la siguiente manera:

Ataques:
    Nombre | Debil x(1/2)               | Fuerte x(2)     | Inmune x(0)
    Acero  | Acero Fuego Agua Eléctrico | Roca Hielo Hada | 

Una vez entendido este tipo de organización, podemos salatar al parseo de tipos.
-}

--Parseo de Tipos-------------------------------------------------------------
-- Dado una lista de Strings (Ataques), devolver una lista de tipos
------------------------------------------------------------------------------
parsearTipos :: [String]  -> [Tipo]
parsearTipos = foldr(\x ac -> parseoUnSoloTipo x : ac) []

parseoUnSoloTipo :: String -> Tipo
parseoUnSoloTipo atk = Nombre nombre attak
    where
        --Parsea el String atk a una tupla de tres valores (Debilidades, Fortalezas, Inmunidades)
        (atk_d, atk_f, atk_i) = parseoDebilFuerteInmune atk

        --Crea el Tipo Ataques [ Debilidades [], Fuerte [], Inmune []]
        attak = Ataques [Debil atk_d, Fuerte atk_f, Inmune atk_i]

        --Obtiene el nombre del Tipo
        nombre = head (splitText (=='\t') atk)


--Parsea un String a una tupla de lista de Strings divididos de la siguiente forma:
--      
--      - (Debilidades, Fortalezas, Inmunidades)

parseoDebilFuerteInmune :: String -> ([String],[String],[String])
parseoDebilFuerteInmune s = (debilidades, fortalezas, inmunidades)
    where
        --No nos interesa el nombre por lo tanto lo dropeamos
        lista = drop 1 (splitText (=='\t') s) 

        -- de la lista de strings cogemos los respectivos valores
        debilidades = (words.head) lista
        fortalezas = words (lista!!1)
        inmunidades = words (lista!!2)


--Parseo de Habilidades----------------------------------------------------------
-- Dado una lista de Strings (Habilidades ), devolver una lista de Habilidades
--      Habilidad = Habilidad 0 "Energi Bola" 70 "Eléctrico"
------------------------------------------------------------------------------
parsearHabilidades :: [String] -> [Habilidad]
parsearHabilidades habilidades = map parseoUnaSolaHabilidad habilidades


parseoUnaSolaHabilidad :: String -> Habilidad 
parseoUnaSolaHabilidad h = (Habilidad id nombreHabilidad poder tipo)
    where
        lista = splitText (=='\t') h
        id = (read.head) lista
        nombreHabilidad = lista!!1
        poder = read (lista!!3)
        tipo = lista!!2


--Parseo de Pokemons----------------------------------------------------------
-- Dado una lista de Strings (Pokemons), devolver una lista de Pokemons
------------------------------------------------------------------------------
parsearPokemons :: [String] -> [Tipo] -> [Habilidad] -> Int -> [Pokemon]
parsearPokemons pokemons t h seed = [parsearUnSoloPokemon pokemon t h seed | pokemon <- pokemons] 

parsearUnSoloPokemon :: String -> [Tipo] -> [Habilidad] -> Int -> Pokemon
parsearUnSoloPokemon p t h seed = Pokemon nombre (t1, t2) vida habilidades
    where
        lista = splitText (=='\t') p
        nombre = head lista
        vida = read (lista!!1)
        t1 = getTipoPorNombre t (lista!!2)
        t2 | lista!!3 == "null" = Null
           | otherwise = getTipoPorNombre t (lista!!3)  
        habilidades = pokemonSetHabilidades h [] (seed)

--Otorga a cada pokemon 4 habilidades aleatorias dado una semilla aleatoria
pokemonSetHabilidades :: [Habilidad] -> [Habilidad] -> Int -> [Habilidad]
pokemonSetHabilidades t ac seed
    | length ac < 4 = pokemonSetHabilidades t (nuevoAtaque ++ ac) (seed + 1)
    | otherwise = ac
    where
        generator = mkStdGen seed
        n = length t
        (rand, _) = randomR (0,n-1) generator
        nuevoAtaque 
            | elem (t!!rand) ac = []
            | otherwise = [t!!rand]

--Devolver un pokemon random dado una semilla
getPokemonRandom :: [Pokemon] -> Int -> Pokemon
getPokemonRandom p seed = p!!rand
    where
        generator = mkStdGen seed
        n = length p
        (rand, _) = randomR (0,n) generator

--Parsea semilla
parseoSemilla :: String -> (Int, Int)
parseoSemilla s 
    | length s2 <= 8 && length s1 <=8 = (read s1, read s2)
    | otherwise = error $ "El formato de la " ++ red ++ "semilla" ++ none ++ " no es correcto"
    where 
        semillas = splitText (=='-') s
        (s1, s2) = (head semillas, last semillas) 


--------------------------------------------------------------------------
--      GUARDAR Y CARGAR DATOS
--------------------------------------------------------------------------

{-
    Lo único que se almacenará son los pokemons y las pociones, el formato de guardado
    será siempre el mismo. Un fichero compuesto por 4 líneas, donde las Impares (suponiendo
     que comienzan en la línea 1) son del pokemon Aliado y las pares del pokemon Enemigo. 
     Un ejemplo de partida guardad sería la siguiente:

        Partida de referencia (Save/Prueba.pokemon):
            1-  Mismagius	60	Fantasma	null	242	260	279	349
            2-  Sinistea	40	Fantasma	null	149	210	158	315
            3-  50|35|20|-
            4-  20|-
    
    En este caso el pokemon aliado sería Mismagius y sus pociones serían las correspondientes
    a la línea 3
    y el restante sería lo correspondiente al equipo enemigo

    Ahora bien, el formato del pokemone s lo siquiente:
        
        Nombre   | HP |  Tipo 1  | Tipo 2 | IDs de las Habilidades
        Mismagius  60	Fantasma	null	242	260	279	349
    
    Solo guardamos las IDs de las Habilidades y el nombre de los tipos, por lo que es necesario
    pasar como parámetro de entrada a la función load todos los Tipos existentes y todas las
    Habilidades y parsear el pokemon en funcion al nombre de los tipos y las IDs de sus Habilidades.
-}

savePokemon :: Pokemon -> String
savePokemon (Pokemon n (t1,t2) hp h) = n ++ "\t" ++ show hp ++ saveTipo t1 ++ saveTipo t2 ++ saveHabilidades h
    where 
        saveTipo (Nombre n a) = "\t" ++ n
        saveTipo Null = "\t" ++ "null"
        saveHabilidades ((Habilidad id _ _ _):hs) = "\t" ++ show id ++ saveHabilidades hs
        saveHabilidades [] = ""

loadPokemonSave :: String -> [Habilidad] -> [Tipo] -> Pokemon
loadPokemonSave line habilidades tipos = Pokemon nombre (tipo1, tipo2) hp h
    where
        lista = splitText (=='\t') line
        nombre = head lista
        hp = read (lista!!1)
        tipo1 = getTipoPorNombre tipos (lista!!2)
        tipo2 = getTipoPorNombre tipos (lista!!3)
        h = [habilidad1,habilidad2,habilidad3,habilidad4]
        habilidad1 = getHabilidadPorID habilidades (read (lista!!4))
        habilidad2 = getHabilidadPorID habilidades (read (lista!!5))
        habilidad3 = getHabilidadPorID habilidades (read (lista!!6))
        habilidad4 = getHabilidadPorID habilidades (read (lista!!7))