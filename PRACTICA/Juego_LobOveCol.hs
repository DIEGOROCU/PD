module Juegos.Juego_LobOveCol where

import Jugable 
import qualified Data.Map as M

data Objeto = Lobo | Oveja | Col deriving (Eq, Ord, Enum, Show)
data PosBarquero = Origen | Destino deriving (Eq, Ord, Enum, Show)

type Posicion = (PosBarquero, M.Map Objeto Int, M.Map Objeto Int)

data LobOveCol = Juego {
    capacidadBarca :: Int,
    posicionActual :: Posicion
} deriving (Eq, Show)

-- Aqui tenemos que agregar las relaciones de ganancia entre los objetos
-- si añadimos más objetos, debemos actualizar esta función
gana :: (Objeto, Int) -> (Objeto, Int) -> Bool
gana (Lobo, c1) (Oveja, c2) = c1 > 0 && c2 > 0
gana (Oveja, c1) (Col, c2) = c1 >= c2 && c2 > 0
gana _ _ = False

-- Dado un diccionario de objetos y sus cantidades (representando un lado del río)
-- determina si hay conflictos entre los objetos presentes en ese lado del río
ladoSeguro :: M.Map Objeto Int -> Bool
ladoSeguro objetos =
    let objetosList = M.toList objetos
        combinaciones = [ ((obj1, c1), (obj2, c2)) | (obj1, c1) <- objetosList, (obj2, c2) <- objetosList, obj1 /= obj2 ]
    in not (any (uncurry gana) combinaciones)

-- Dado un estado del juego, determina si es un estado válido (sin conflictos)
estadoValido :: LobOveCol -> Bool
estadoValido (Juego _ (posBarquero, origen, destino))
    | posBarquero == Origen  = ladoSeguro destino
    | posBarquero == Destino = ladoSeguro origen

-- Dado un lado del río, devuelve el otro lado
otroLado :: PosBarquero -> PosBarquero
otroLado Origen = Destino
otroLado Destino = Origen

-- Generador de posibles combinaciones de carga limitadas por la capacidad
generarViajes :: Int -> M.Map Objeto Int -> [M.Map Objeto Int]
generarViajes cap banco = 
    let items = [ (k, c) | (k, c) <- M.toList banco, c > 0 ]
        generar [] = [M.empty]
        generar ((obj, maxC):resto) = [ M.insert obj n mapaResto | n <- [0..maxC], mapaResto <- generar resto ]
    in filter (\m -> let tot = sum (M.elems m) in tot >= 1 && tot <= cap) (generar items)


-- Juego
instance Jugable LobOveCol where

    -- Diccionario origen vacío
    esExito (Juego _ (_, origen, _)) = 
        all (== 0) (M.elems origen)

    -- Quitar combinaciones y ponerlas en destino
    opciones (Juego cap (posBarquero, origen, destino)) = 
        let 
            bancoActual = if posBarquero == Origen then origen else destino
            viajesPosibles = generarViajes cap bancoActual
            
            aplicarViaje viaje = 
                let restar = M.unionWith (-)
                    sumar  = M.unionWith (+)
                    (nuevoOrigen, nuevoDestino) = if posBarquero == Origen
                    then (restar origen viaje, sumar destino viaje)
                    else (sumar origen viaje, restar destino viaje)
                in Juego cap (otroLado posBarquero, nuevoOrigen, nuevoDestino)
            
            nuevosEstados = map aplicarViaje viajesPosibles
            
        -- Filtramos los movimientos generados para devolver solo los que no tienen conflictos
        in filter estadoValido nuevosEstados

-- Ejemplos con solucion
conSolucion :: LobOveCol
conSolucion = 
    [
        -- Ejemplo 1
        Juego 2 (Origen, M.fromList [(Lobo, 2), (Oveja, 2), (Col, 2)], M.empty), 

        -- Ejemplo 2
        Juego 3 (Origen, M.fromList [(Lobo, 1), (Oveja, 1), (Col, 1)], M.empty),
    ]

-- Ejemplos sin solucion
sinSolucion :: LobOveCol
    [
        -- Ejemplo 1
        Juego 1 (Origen, M.fromList [(Lobo, 1), (Oveja, 1), (Col, 1)], M.empty),

        -- Ejemplo 2
        Juego 1 (Origen, M.fromList [(Lobo, 2), (Oveja, 2)], M.empty),
    ]