import Text.Show.Functions
type Dia = Int
type Mes = Int
type Anio = Int
type Condicion = Viaje -> Bool
data Chofer = UnChofer {nombreChofer :: String, kilometraje :: Int, viajes :: [Viaje], condicion :: Condicion} deriving Show
data Viaje = UnViaje {cliente :: Cliente, fecha :: (Dia,Mes,Anio) , costo :: Int} deriving Show
data Cliente = UnCliente {nombreCliente :: String, hogar :: String} deriving Show
lucas = UnCliente {nombreCliente = "Lucas", hogar = "Victoria"}
viajeConLucas = UnViaje {cliente = lucas, fecha = (20,04,2017), costo = 150}
daniel = UnChofer {nombreChofer = "Daniel", kilometraje = 23500, viajes = [viajeConLucas], condicion = (noVivaEnXZona "Olivos")}
alejandra = UnChofer {nombreChofer = "Alejandra", kilometraje = 180000, viajes = [], condicion = (cualquierViaje)}
nito = UnChofer {nombreChofer = "Nito Infy", kilometraje = 70000, viajes = repetirViaje viajeRepetitivo , condicion = (nombreMasDeNLetras 3)}
viajeRepetitivo = UnViaje {cliente = lucas, fecha = (11,03,2017), costo = 50}
otroViaje = UnViaje {cliente = lucas, fecha = (2,5,2017), costo = 500}

cualquierViaje :: Condicion
cualquierViaje _ = True

masDe200 :: Condicion
masDe200 viaje = costo viaje > 200

nombreMasDeNLetras :: Int -> Condicion
nombreMasDeNLetras cant =  (> cant).length.nombreCliente.cliente

noVivaEnXZona :: String -> Condicion
noVivaEnXZona zona = (/= zona).hogar.cliente

puedeTomarViaje :: Chofer -> Condicion
puedeTomarViaje chofer viaje = (condicion chofer) viaje

liquidacionChofer :: Chofer -> Int
liquidacionChofer = sum.(map costo).viajes

aplicarCondicion :: Viaje -> Chofer -> Bool
aplicarCondicion viaje chofer = puedeTomarViaje chofer viaje

realizarViaje :: Viaje -> [Chofer] -> Chofer
realizarViaje viaje choferes = ((agregarViaje viaje).quienTieneMenosViaje.(quienesPuedenTomarViaje viaje)) choferes

agregarViaje :: Viaje -> Chofer -> Chofer
agregarViaje viaje chofer = chofer{viajes = (viajes chofer) ++ [viaje]}

quienesPuedenTomarViaje :: Viaje -> [Chofer] -> [Chofer]
quienesPuedenTomarViaje viaje choferes = filter (aplicarCondicion viaje) choferes

quienTieneMenosViaje :: [Chofer] -> Chofer
quienTieneMenosViaje [chofer] = chofer
quienTieneMenosViaje (chofer1:chofer2:choferes) = elQueMenosViajesHizo (elQueMenosViajesHizo chofer1 chofer2) (quienTieneMenosViaje (chofer2:choferes))

elQueMenosViajesHizo :: Chofer -> Chofer -> Chofer
elQueMenosViajesHizo chofer1 chofer2 | cantDeViajes chofer1 < cantDeViajes chofer2 = chofer1
                                     | otherwise = chofer2

cantDeViajes :: Chofer -> Int
cantDeViajes = length.viajes

repetirViaje viaje = viaje : repetirViaje viaje

gongNeng :: (Ord a) => a -> (a -> Bool) -> (b -> a) -> [b] -> a
gongNeng arg1 arg2 arg3 = max arg1 . head . (filter arg2) . (map arg3)

