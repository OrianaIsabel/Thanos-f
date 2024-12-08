module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- Punto 1

data Guantelete = UnGuatelete {
    material :: String,
    gemas :: [Gema]
} deriving (Show)

data Personaje = UnPersonaje {
    nombre :: String,
    edad :: Number,
    energia :: Number,
    habilidades :: [String],
    planeta :: String
} deriving (Show, Eq)

type Universo = [Personaje]

thom :: Personaje
thom = UnPersonaje "Thom" 45 100 ["cantar", "escribir", "respirar bajo el agua"] "tierra"

jonny :: Personaje
jonny = UnPersonaje "Jonny" 45 150 ["cantar", "respirar bajo el agua", "ser gotica"] "tierra"

collin :: Personaje
collin = UnPersonaje "Collin" 20 60 ["tocar el bajo", "escribir"] "marte"

ed :: Personaje
ed = UnPersonaje "Ed" 30 100 ["bailar"] "venus"

phil :: Personaje
phil = UnPersonaje "Phil" 15 30 ["tocar la bateria", "ser gotica", "espiar"] "saturno"

radiohead :: [Personaje]
radiohead = [thom, jonny, collin, ed, phil]

mtv :: Guantelete
mtv = UnGuatelete "uru" [(tiempo), (alma "cantar"), (loca (alma "respirar bajo el agua")), (espacio "pluton"), (tiempo), (mente 10)]

estaCompleto :: Guantelete -> Bool
estaCompleto guantelete = ((== 6).length.gemas) guantelete && ((== "uru").material) guantelete

reducirALaMitad :: Universo -> Universo
reducirALaMitad universo = take (div (length universo) 2) universo

chasquear :: Guantelete -> Universo -> Universo
chasquear guantelete universo
    | estaCompleto guantelete = reducirALaMitad universo
    | otherwise = universo

-- Punto 2 Orden Superior

aptoParaPendex :: Universo -> Bool
aptoParaPendex = any ((< 45).edad)

energiaTotal :: Universo -> Number
energiaTotal = sum.(map energia).(filter ((> 1).length.habilidades))

-- Punto 3

type Gema = Personaje -> Personaje

reducirEnergia :: Number -> Personaje -> Personaje
reducirEnergia n personaje = personaje {energia = (energia personaje) - n}

anularEnergia :: Personaje -> Personaje
anularEnergia personaje = personaje {energia = 0}

poseeHabilidad :: String -> Personaje -> Bool
poseeHabilidad habilidad = (elem habilidad).(habilidades)

quitarHabilidad :: String -> Personaje -> Personaje
quitarHabilidad habilidad personaje = personaje {habilidades = filter (/= habilidad) (habilidades personaje)}

anularHabilidades :: Personaje -> Personaje
anularHabilidades personaje = personaje {habilidades = []}

teletransportar :: String -> Personaje -> Personaje
teletransportar otroPlaneta personaje = personaje {planeta = otroPlaneta}

dividirEdad :: Personaje -> Personaje
dividirEdad personaje = personaje {edad = max 18 (div (edad personaje) 2)}

mente :: Number -> Gema
mente = reducirEnergia

alma :: String -> Gema
alma habilidad = ((reducirEnergia 10).(quitarHabilidad habilidad))

espacio :: String -> Gema
espacio otroPlaneta = ((reducirEnergia 20).(teletransportar otroPlaneta))

poder :: Gema
poder personaje
    | ((== 2).length.habilidades) personaje = (anularEnergia.anularHabilidades) personaje
    | otherwise = anularEnergia personaje

tiempo :: Gema
tiempo = ((reducirEnergia 50).dividirEdad)

loca :: Gema -> Gema
loca gema = (gema.gema)

-- Punto 4

guante :: Guantelete
guante = UnGuatelete "goma" [(tiempo), (alma "cantar"), (loca (alma "respirar bajo el agua"))]

-- Punto 5

utilizarGema :: Gema -> Personaje -> Personaje
utilizarGema gema personaje = gema personaje

utilizar :: [Gema] -> Personaje -> Personaje
utilizar listaGemas personaje = foldr utilizarGema personaje listaGemas

-- Punto 6

masPoderosa :: [Gema] -> Personaje -> Gema
masPoderosa [gema] _ = gema
masPoderosa (gema1:gema2:resto) personaje
    | energia (gema1 personaje) >= energia (gema2 personaje) = masPoderosa (gema1:resto) personaje
    | otherwise = masPoderosa (gema2:resto) personaje

gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa guantelete = masPoderosa (gemas guantelete)
