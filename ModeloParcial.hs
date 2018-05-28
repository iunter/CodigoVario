import Data.List

f :: (Ord a) => a -> (b -> a) -> (c,[b]) -> Bool 
f  h  p = any (<h). map p . snd
-----------------------------------------------------------------------------------------------------------------------------------

data Ayudante = UnAyudante {nombre :: String , conceptos :: [Concepto]}deriving Show
type Concepto = (String,Int)
type Juez = (Ayudante -> Int)
type Jurado = [Juez]

guille = UnAyudante{nombre = "Guille", conceptos = [("orden superior",6),("expresiones lambda",7),("fold",8)]} 
chacal = UnAyudante{nombre = "El chacal", conceptos = [("aplicacion parcial",9),("fold",6),("sinonimos de tipo",7)]}
vicky = UnAyudante{nombre = "Vicky", conceptos = [("clases de tipo",5),("aplicacion parcial",8),("tuplas",9),("orden superior", 8)]}

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

aproboParaTodos :: Jurado -> Ayudante -> Bool
aproboParaTodos jueces ayudante = length jurado == conCuantosAprobo ayudante jueces

conCuantosAprobo :: Ayudante -> Jurado -> Int
conCuantosAprobo ayudante jueces = length(filter (>= 7) (map (aplicarJuezAyudante ayudante) jueces))