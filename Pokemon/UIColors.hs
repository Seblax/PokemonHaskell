module UIColors where
import PokemonData

type Color = String

clear :: Color
clear = "\ESC[2J"

none::Color
none = "\ESC[0m"
black :: Color
black = "\ESC[30m"
red :: Color
red = "\ESC[31m"
green :: Color
green = "\ESC[32m"
yellow :: Color
yellow = "\ESC[33m"
blue :: Color
blue = "\ESC[34m"
magenta :: Color
magenta = "\ESC[35m"
cyan :: Color
cyan = "\ESC[36m"
white :: Color
white = "\ESC[37m"
black2 :: Color
black2 = "\ESC[90m"
red2 :: Color
red2 = "\ESC[91m"
green2 :: Color
green2 = "\ESC[92m"
yellow2 :: Color
yellow2 = "\ESC[93m"
blue2 :: Color
blue2 = "\ESC[94m"
magenta2 :: Color
magenta2 = "\ESC[95m"
cyan2 :: Color
cyan2 = "\ESC[96m"
white2 :: Color
white2 ="\ESC[97m"

--Colores de los pokemons

colorAcero :: String
colorAcero = "\ESC[38;5;103m"

colorAgua :: String
colorAgua = "\ESC[38;5;4m"

colorBicho :: String
colorBicho = "\ESC[38;5;106m"

colorDragón :: String
colorDragón = "\ESC[38;5;20m"

colorEléctrico :: String
colorEléctrico = "\ESC[38;5;226m"

colorFantasma :: String
colorFantasma = "\ESC[38;5;129m"

colorFuego :: String
colorFuego = "\ESC[38;5;1m"

colorHada :: String
colorHada = "\ESC[38;5;219m"

colorHielo :: String
colorHielo = "\ESC[38;5;123m"

colorLucha :: String
colorLucha = "\ESC[38;5;202m"

colorNormal :: String
colorNormal = "\ESC[38;5;250m"

colorPlanta :: String
colorPlanta = "\ESC[38;5;82m"

colorPsíquico :: String
colorPsíquico = "\ESC[38;5;201m"

colorRoca :: String
colorRoca = "\ESC[38;5;130m"

colorSiniestro :: String
colorSiniestro = "\ESC[38;5;238m"

colorTierra :: String
colorTierra = "\ESC[38;5;215m"

colorVeneno :: String
colorVeneno = "\ESC[38;5;53m"

colorVolador :: String
colorVolador = "\ESC[38;5;15m"


--Dado un tipo devuelve un color
setColorTipo :: String -> String
setColorTipo s
  | s == "Acero" = colorAcero
  | s == "Aguas" = colorAgua
  | s == "Bicho" = colorBicho
  | s == "Dragon" = colorDragón
  | s == "Electrico" =colorEléctrico
  | s == "Fantasma" = colorFantasma
  | s == "Fuego" = colorFuego
  | s == "Hadas" = colorHada
  | s == "Hielo" = colorHielo
  | s == "Lucha" = colorLucha
  | s == "Normal" = colorNormal
  | s == "Planta" = colorPlanta
  | s == "Psiquico" = colorPsíquico
  | s == "Piedra" = colorRoca
  | s == "Siniestro" =colorSiniestro
  | s == "Tierra" = colorTierra
  | s == "Veneno" = colorVeneno
  | s == "Volador" = colorVolador
  | otherwise = error $ "No existe color para el tipo: \n" ++ s


setTipoColorPokemonBatalla :: Tipo -> String
setTipoColorPokemonBatalla (Nombre s _) = setColorTipo s ++ take 2 s ++ none
setTipoColorPokemonBatalla _ = ""