module Parser where
import PokemonData
{-
    Función que separa un String en función de un Predicado dado.
    Enlaces de donde he sacado las fuentes para entender el código:
        - https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
        - http://zvon.org/other/haskell/Outputprelude/break_f.html
        - http://zvon.org/other/haskell/Outputsyntax/caseQexpressions_reference.html
-}
split :: (Char -> Bool) -> String -> [String]   --Devuelve una lista de Strings
split p s =  case dropWhile p s of          --Actua como un Switch              
                      "" -> []              --Si estamos en caso base añadimos una Lista vacía
                      s' -> w : split p xs --Si no, concatenamos w por la izquierda y realizamos llamada recursiva
                            where (w, xs) = break p s' 
                                            -- Break crea una tupla de 2 listas separadas por una condicion
                                            -- w lista resultante, minetras que xs es el resto




{-
Para parsear los datos vamos a usar la siguiente estrategia, tenemos las defensas
y el ataque de cada tipo en dos ficheros distintos:
        - AtkTypes.txt
        - DefTypes.txt

Los dos documentos están ectructurados exactamente de la misma forma, cada línea de texto
representa un tipo Pokémon, estas a su vez están divididas en guardas para diferenciar
entre el nombre, debilidades, fortalezas e inmunidades. Otra cosa a tener en cuenta
es que los tipos están ordenados exactamente igual en ambos ficheros, la línea 1 por ejemplo
es la que pertenece alelemento Acero. Quedando la estructura de la siguiente manera:

Defensas: 
    Nombre | Debil x(2)         | Fuerte x(1/2)                                                     | Inmune x(0)
    Acero  | Lucha Fuego Tierra | Normal Volador Roca Bicho Acero Planta Psíquico Hielo Dragón Hada | Veneno

Ataques:
    Nombre | Debil x(1/2)               | Fuerte x(2)     | Inmune x(0)
    Acero  | Acero Fuego Agua Eléctrico | Roca Hielo Hada | 

Una vez entendido este tipo de organización, podemos salatar al parseo de tipos.
-}



--Parseo de Tipos-------------------------------------------------------------
-- Dado dos lista de Strings (Ataques y Defensas), devolver una lista de tipos
------------------------------------------------------------------------------

parsearTipos :: [String] -> [String] -> [Tipo]
parsearTipos ataques defensas = [parseoUnSoloTipo s | s <- zip ataques defensas] 


parseoUnSoloTipo :: (String, String) -> Tipo
parseoUnSoloTipo (atk, def) = Nombre nombre (attak, defense)
    where
        --Parsea el String atk a una tupla de tres valores (Debilidades, Fortalezas, Inmunidades)
        (atk_d, atk_f, atk_i) = parseoDebilFuerteInmune atk

        --Parsea el String def a una tupla de tres valores (Debilidades, Fortalezas, Inmunidades)
        (def_d, def_f, def_i) = parseoDebilFuerteInmune def

        --Crea el Tipo Ataques [ Debilidades [], Fuerte [], Inmune []]
        attak = Ataques [Debil atk_d, Fuerte atk_f, Inmune atk_i]

        --Crea el Tipo Defensas [ Debilidades [], Fuerte [], Inmune []]
        defense = Defensas [Debil def_d, Fuerte def_f, Inmune def_i]

        --Obtiene el nombre del Tipo
        nombre = head (split (=='|') atk)


--Parsea un String a una tupla de lista de Strings divididos de la siguiente forma:
--      
--      - (Debilidades, Fortalezas, Inmunidades)
parseoDebilFuerteInmune :: String -> ([String],[String],[String])
parseoDebilFuerteInmune s = (debilidades, fortalezas, inmunidades)
    where
        --No nos interesa el nombre por lo tanto lo dropeamos
        lista = drop 1 (split (=='|') s) 

        -- de la lista de strings cogemos los respectivos valores
        debilidades = (words.head) lista
        fortalezas = words (lista!!1)
        inmunidades = words (lista!!2)