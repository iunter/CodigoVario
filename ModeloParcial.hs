import Data.List

f :: (Ord a) => a -> (b -> a) -> (c,[b]) -> Bool 
f  h  p = any (<h). map p . snd
-----------------------------------------------------------------------------------------------------------------------------------

data Ayudante = UnAyudante {nombre :: String , conceptos :: [Concepto]} deriving (Eq,Show)
type Concepto = (String,Int)
type Juez = (Ayudante -> Int)
type Jurado = [Juez]

guille = UnAyudante{nombre = "Guille", conceptos = [("orden superior",6),("expresiones lambda",7),("fold",8)]} 
chacal = UnAyudante{nombre = "El chacal", conceptos = [("aplicacion parcial",9),("fold",6),("sinonimos de tipo",7)]}
vicky = UnAyudante{nombre = "Vicky", conceptos = [("clases de tipo",5),("aplicacion parcial",8),("tuplas",9),("orden superior", 8)]}

listaAyudantes = [guille, chacal, vicky]

jurado = [(hernan True), gise, marche, (\(UnAyudante _ x)-> 7)]

cuantosTienenNivel :: [Ayudante] -> Int -> Int
cuantosTienenNivel ayudantes nivel = length (filter (poseeNivel nivel) ayudantes)

poseeNivel :: Int -> Ayudante -> Bool
poseeNivel nivel ayudante = any (== nivel) (listarConceptosPorNivel ayudante)

listarConceptosPorNivel :: Ayudante -> [Int]
listarConceptosPorNivel ayudante = map snd (conceptos ayudante)

listarConceptosPorNombre :: Ayudante -> [String]
listarConceptosPorNombre ayudante = map fst (conceptos ayudante)

fueAprendiendo :: Ayudante -> Bool
fueAprendiendo ayudante  = sort (listarConceptosPorNivel ayudante) == listarConceptosPorNivel ayudante

promedio ::  [Int] -> Int
promedio lista = div (sum lista) (length lista)

gise :: Juez
gise ayudante = promedio (listarConceptosPorNivel ayudante)

marche :: Juez
marche ayudante | any (== "orden superior") (listarConceptosPorNombre ayudante) = 9
                | otherwise = 5

changui :: Bool -> Int
changui esBuenDia | esBuenDia = 2
                  | otherwise = 0

hernan :: Bool -> Ayudante -> Int
hernan esBuenDia ayudante = length (conceptos ayudante) + changui esBuenDia

aplicarJuezAyudante :: Ayudante -> Juez -> Int 
aplicarJuezAyudante ayudante juez = juez ayudante

promedioPuntajes :: Jurado -> Ayudante -> Int
promedioPuntajes jueces ayudante = promedio (map (aplicarJuezAyudante ayudante) jueces)

esBuenAyudante :: Jurado -> Ayudante -> Bool
esBuenAyudante jueces ayudante = length jurado == conCuantosAprobo ayudante jueces

conCuantosAprobo :: Ayudante -> Jurado -> Int
conCuantosAprobo ayudante jueces = length(filter (>= 7) (map (aplicarJuezAyudante ayudante) jueces))

cantNivelTemasMayorA :: Int -> Ayudante -> Int
cantNivelTemasMayorA nivel ayudante = length (filter (> nivel) (listarConceptosPorNivel ayudante))


notaTema :: String -> Ayudante -> Int
notaTema tema ayudante | not ( any (== tema) (listarConceptosPorNombre ayudante)) = 0
                       | otherwise = (snd.head) (filter ((== tema).fst) (conceptos ayudante)) 

maximoAyudanteSegun :: Ord b =>  (a -> b) -> [a] ->a
maximoAyudanteSegun criterio ayudantes = foldl1 (elMejorEntre criterio) ayudantes

elMejorEntre criterio uno otro | criterio uno > criterio otro = uno
                               | otherwise = otro

podio :: (Ayudante -> Int) -> [Ayudante] -> [Ayudante]
podio criterio ayudantes = take 3 (sortPor criterio ayudantes)

podioTema :: String -> [Ayudante] -> [Ayudante]
podioTema tema ayudantes = podio (notaTema tema) ayudantes

podioNivelTemas :: Int -> [Ayudante] -> [Ayudante]
podioNivelTemas nivel ayudantes = podio (cantNivelTemasMayorA nivel) ayudantes

sortPor :: (Eq a, Ord b) => (a -> b) -> [a] -> [a]
sortPor _ [] = []
sortPor criterio ayudantes = maximoAyudanteSegun criterio ayudantes : sortPor criterio (losPeores criterio ayudantes)

losPeores :: (Eq a, Ord b)=> (a -> b) -> [a] -> [a]
losPeores criterio ayudantes = filter (/= maximoAyudanteSegun criterio ayudantes) ayudantes