import Text.Show.Functions
data Personaje = UnPersonaje {nombre::String, ataqueFavorito::Ataque, elementos::[String], energia::Int} deriving Show
type Ataque = Personaje -> Personaje


hulk = UnPersonaje "hulk" superFuerza ["pantalones"] 90
thor = UnPersonaje "thor" (relampagos 50) ["mjolnir"] 100
viuda = UnPersonaje "viuda negra" artesMarciales [] 90
capitan = UnPersonaje "capitán américa" arrojarEscudo ["escudo"] 80
halcon = UnPersonaje "ojo de halcón" arqueria ["arco", "flechas"] 70
vision = UnPersonaje "vision" (proyectarRayos 5) ["gema del infinito"] 100
ironMan = UnPersonaje "iron man" (ironia (relampagos (-50))) ["armadura", "jarvis", "plata"] 60
ultron = UnPersonaje "robot ultron"  corromperTecnologia [] 100 

funcionIdentidad :: Ataque
funcionIdentidad atacado = atacado

esRobot :: Personaje -> Bool
esRobot personaje = take 5 (nombre personaje) == "robot"

poseeElem ::  String -> Personaje -> Bool
poseeElem elemento personaje = any (== elemento) (elementos personaje)

calcularPotenciaPersonaje :: Personaje -> Int
calcularPotenciaPersonaje personaje = length (elementos personaje) * energia personaje

atacar :: Personaje -> Personaje -> Personaje
atacar atacante atacado = (ataqueFavorito atacante) atacado

responder :: Personaje -> Personaje -> Personaje
responder responde atacante = (ataqueFavorito responde) atacante

ganaAtacante :: Personaje -> Personaje -> Bool
ganaAtacante atacante atacado = energia (responder (atacar atacante atacado) atacante) > energia (atacar atacante atacado)

modificarEnergia :: Personaje -> Int -> Personaje
modificarEnergia atacado cantidad = atacado{energia = cantidad}

superFuerza :: Ataque
superFuerza atacado = modificarEnergia atacado 0

relampagos :: Int -> Personaje -> Personaje
relampagos intesidad atacado = modificarEnergia atacado (energia atacado - intesidad)

tieneEscudo :: Personaje -> Int
tieneEscudo personaje | poseeElem "escudo" personaje = energia personaje
                      | otherwise = 0

arqueria :: Ataque
arqueria atacado = modificarEnergia atacado (tieneEscudo atacado)

proyectarRayos = relampagos

arrojarEscudo :: Ataque
arrojarEscudo atacado = atacado{elementos = []}

artesMarciales :: Ataque
artesMarciales atacado = atacado{ataqueFavorito = funcionIdentidad}

ironia :: Ataque -> Personaje -> Personaje
ironia ataqueModificado atacado = atacado{ataqueFavorito = ataqueModificado}

hacerEsclavo :: Ataque
hacerEsclavo atacado = atacado{nombre = "ESCLAVO " ++ (nombre atacado)}

corromperTecnologia :: Ataque
corromperTecnologia atacado = (hacerEsclavo.superFuerza.arrojarEscudo) atacado

tieneGema :: [Personaje] -> Bool
tieneGema personajes = any (== True) (map (poseeElem "gema del infinito")  personajes)

aplicarCombate :: (Personaje, Personaje) -> Bool
aplicarCombate (atacante, atacado) = ganaAtacante atacante atacado

batallar :: [Personaje] -> [Personaje] -> [Bool]
batallar atacantes defensores = (map aplicarCombate (zip atacantes defensores))

ganoMasVeces :: [Personaje] -> [Personaje] -> Bool
ganoMasVeces atacantes defensores = length ( filter (== True) (batallar atacantes defensores)) > length ( filter (== False) (batallar atacantes defensores))

batallaFinal :: [Personaje] -> [Personaje] -> [Personaje]
batallaFinal atacantes defensores
    |tieneGema atacantes && not (tieneGema defensores) = atacantes
    |tieneGema defensores && not (tieneGema atacantes) = defensores
    |ganoMasVeces atacantes defensores = atacantes
    |otherwise = defensores
