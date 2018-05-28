import Text.Show.Functions

--------------------------------------------------------
-- A1) Modelar a los ayudantes
data Ayudante = UnAyudante {nombre::String,
                            conceptos::[Concepto]} deriving (Eq,Show)

guille = UnAyudante {nombre = "Guille", conceptos = [("ordenSuperior",6),("expresionLambda",7),("fold",8)] }
elChacal = UnAyudante {nombre = "ElChacal",conceptos = [("aplicacionParcial",9),("fold",6),("sinonimosDeTipo",7)] }
vicky = UnAyudante {nombre = "Vicky", conceptos = [("clasesDeTipo",5),("aplicacionParcial",8),("tuplas",9),("ordenSuperior",8)] }

type Concepto = (String,Int)
nivel = snd
nombreConcepto = fst
--------------------------------------------------------
-- A2) Cuántos tienen determinado nivel.
cuantosTienen nivel ayus = (length . filter (tieneNivel nivel)) ayus

tieneNivel niv ayudante = any ((==niv).nivel) (conceptos ayudante)

--------------------------------------------------------
-- A3) Fue aprendiendo cada vez más (niveles crecientes)
aprendioCadaVezMas = sonCrecientes.niveles   
niveles ayudante = map nivel (conceptos ayudante)

sonCrecientes [] = True
sonCrecientes [x] = True
sonCrecientes (x:y:xs) = x <= y && sonCrecientes (y:xs) 

--------------------------------------------------------
--------------------------------------------------------
--------------------------------------------------------
-- B1) Jueces
type Juez = Ayudante -> Int

gise :: Juez
gise ayudante = (promedio.niveles) ayudante
-- con point free:
-- gise = promedio.niveles

-- Para no repetir la idea de promedio en dos lugares distintos, creo una función
promedio lista = div (sum lista) (length lista)

marche :: Juez
marche ayudante | tieneConcepto "ordenSuperior" ayudante = 9
                | otherwise = 5

tieneConcepto conoc ayudante = any ((==conoc).nombreConcepto) (conceptos ayudante)

hernan :: Bool -> Juez
hernan buenDia ayudante = (length.conceptos) ayudante + plus buenDia

plus esBuenDia | esBuenDia = 2
               | otherwise = 0

--------------------------------------------------------
-- B2) El promedio de puntaje de un ayudante para los jueces

promedioPuntajes ayudante jueces = promedio (puntajes ayudante jueces)

puntajes ayudante jueces = (sum.map (aplicar ayudante)) jueces
aplicar ayudante juez = juez ayudante

--------------------------------------------------------
-- B3) Es buen ayudante cuando aprueba con 7 o mas
esBuenAyudante ayudante jueces = all (esBueno ayudante) jueces
esBueno ayudante juez = juez ayudante >= 7

--------------------------------------------------------
-- B4) Ejemplo invocación con una lambda

-- *Main> promedioPuntajes guille [gise,marche,hernan True,(\ayud -> maximum (niveles ayud))]
-- 7

--------------------------------------------------------
--------------------------------------------------------
--------------------------------------------------------
-- C0) Hacer una funcion que saque el maximo de una lista segun un criterio

maximumBy :: Ord b => (a -> b) -> [a] -> a
maximumBy criterio lista = foldl1 (elMejorEntre criterio) lista

elMejorEntre criterio uno otro | criterio uno > criterio otro = uno
                               | otherwise = otro


--------------------------------------------------------
-- C1) Podio de los 3 mejores
podio :: (Ayudante -> Int) -> [Ayudante] -> [Ayudante]
podio criterio ayudantes = take 3 (sortBy criterio ayudantes)

-- Para ordenar, pongo el mejor adelante, y ordeno el resto (los peores)
sortBy :: (Eq a, Ord b) => (a -> b) -> [a] -> [a]
sortBy _ [] = []
sortBy criterio ayus = maximumBy criterio ayus : sortBy criterio (losPeores criterio ayus)

-- Los peores son los que no son el mejor
losPeores :: (Eq a, Ord b) => (a -> b) -> [a] -> [a]
losPeores criterio ayus = filter (/= maximumBy criterio ayus) ayus

------------

podioBuenos :: [Ayudante] -> [Ayudante]
podioBuenos ayudantes = podio cantTemasBuenos ayudantes
cantTemasBuenos ayudante = (length . filter ((>7).nivel)) (conceptos ayudante)

podioJuez :: Juez -> [Ayudante] ->  [Ayudante]
podioJuez juez ayudantes = podio juez ayudantes

podioTema :: String -> [Ayudante] -> [Ayudante]
podioTema tema ayudantes = podio (nivelEn tema) ayudantes
nivelEn tema ayudante | tieneConcepto tema ayudante = (nivel . head . filter ((==tema).nombreConcepto)) (conceptos ayudante)
                      | otherwise = 0
--------------------------------------------------------
-- C3)
-- Con la lista de ayudantes infinita, si quiero un podio, no puedo encontrar ni siquiera el primero:
-- Cuando quiero hacer un máximo de esa lista, independientemente del criterio necesito conocer la lista
-- en su totalidad para poder asegurarme de que sea el máximo, (hasta el final podría haber otro mejor).
-- Por más que Haskell tenga Lazy Evaluation, la consulta del podio con una lista infinita de ayudantes se cuelga.