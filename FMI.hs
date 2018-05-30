data Pais = UnPais{nombre :: String, ingPerCapita :: Float, sectorPublico:: Int, sectorPrivado :: Int, recursos :: [String], deuda :: Float} deriving Show
--La deuda se representa en millones (50 = 50000000)
namibia = UnPais{nombre = "Namibia", ingPerCapita = 4140, sectorPublico = 400000, sectorPrivado = 650000, recursos = ["Mineria", "Ecoturismo"], deuda = 50}
paisConPetroleo = UnPais{nombre = "paisConPetroleo", ingPerCapita = 4140, sectorPublico = 400000, sectorPrivado = 650000, recursos = ["Petroleo", "Ecoturismo"], deuda = 50}
paisPoderoso = UnPais{nombre = "Namibia", ingPerCapita = 4140, sectorPublico = 400000, sectorPrivado = 650000, recursos = recursosNaturalesInfinitos , deuda = 50}
type Estrategia = Pais -> Pais
type Receta = [Estrategia]

prestarNMillonesDolares :: Float -> Estrategia
prestarNMillonesDolares cant pais = pais{deuda = (deuda pais) + cant * 1.5 }

reducirXPuestosPublico :: Int -> Estrategia
reducirXPuestosPublico cant pais | cant > 100 = pais{sectorPublico = (sectorPublico pais) - cant, ingPerCapita = (ingPerCapita pais) * 0.8}
                                 | otherwise =  pais{sectorPublico = (sectorPublico pais) - cant, ingPerCapita = (ingPerCapita pais) * 0.85}

porcentaje :: Int -> Float
porcentaje cant | cant > 100 = 0.8
                | otherwise = 0.85

darEmpresa :: String -> Estrategia
darEmpresa recurso pais = pais{recursos = filter (/= recurso) (recursos pais), deuda = (deuda pais) - 2}

blindaje :: Estrategia
blindaje pais = ((prestarNMillonesDolares ((calcpbi pais) / 2)).(reducirXPuestosPublico 500)) pais

calcpbi :: Pais -> Float
calcpbi pais = (ingPerCapita pais) * fromIntegral (sectorPublico pais + sectorPrivado pais)

prestar200YDarMineria :: Receta
prestar200YDarMineria = [(prestarNMillonesDolares 200),(darEmpresa "Mineria")]

aplicarEstrategia :: Pais -> Estrategia -> Pais
aplicarEstrategia pais estrategia = estrategia pais

aplicarReceta :: Pais -> Receta -> Pais
aplicarReceta pais receta = foldl aplicarEstrategia pais receta

puedenZafar :: [Pais] -> [Pais]
puedenZafar paises = filter ((any (== "Petroleo")).recursos) paises

saberDeudaAFavor :: [Pais] -> Float
saberDeudaAFavor = sum.(map deuda)

recetasDePeorAMenor :: Pais -> [Receta] -> Bool
recetasDePeorAMenor _ [] = True
recetasDePeorAMenor _ [receta] = True
recetasDePeorAMenor pais (receta1:receta2:recetas) = calcpbi (aplicarReceta pais receta1) <= calcpbi (aplicarReceta pais receta2) && recetasDePeorAMenor pais (receta2:recetas)

recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos

