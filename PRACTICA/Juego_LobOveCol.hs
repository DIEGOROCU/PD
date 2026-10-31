module Juegos.Juego_LobObeCol where

import Jugable
import qualified Data.Map as M

data Objeto = Lobo | Oveja | Col deriving (Eq, Ord, Enum, Show)
data PosBarquero = Origen | Destino deriving (Eq, Ord, Enum, Show)

type Posicion = (PosBarquero, M.Map Objeto Int, M.Map Objeto Int)

gana :: Objeto -> Objeto -> Bool
gana o1 o2 =
    | o1 == Lobo && o2 == Oveja = True
    | o1 == Oveja && o2 == Col = True
    | otherwise = False

ganaConCantidad :: (Objeto, Int) -> (Objeto, Int) -> Bool
ganaConCantidad (o1, c1) (o2, c2) =
    | c2 > 0 && c1 >= c2 && gana o1 o2 = True
    | otherwise = False

instance Jugable LobObeCol where

    opciones Posicion pos 
        | -- TODO: Quitar todas las posibles combinaciones y cantidades de objetos de origen limitadas al tamaño de la barca y ponerlas en destino
        | otherwise = []
        where
            origen = fst pos
            destino = snd pos
    esExito Posicion pos = -- TODO: Diccionario origen vacio y diccionario distinto lleno

    ladoSeguro :: M.Map Objeto Int -> Bool
    ladoSeguro objetos =
        let 
            -- Pasamos el diccionario a una lista de tuplas para poder hacer combinaciones
            objetosList = M.toList objetos
            -- Hacemos todas las combinaciones posibles de objetos y cantidades (sin repetir)
            combinaciones = [ ((obj1, c1), (obj2, c2)) | (obj1, c1) <- objetosList, (obj2, c2) <- objetosList, obj1 /= obj2]
        
        -- El lado es seguro si ninguna combinacion de objetos gana a otra
        in not (any (uncurry gana) combinaciones)

    esValida :: Posicion -> Bool
    esValida Posicion (posBarquero, origen, destino)
        -- Solo comprobamos el lado donde no esta el barquero, ya que el otro lado es seguro por definicion
        | posBarquero == Origen = ladoSeguro destino
        | posBarquero == Destino = ladoSeguro origen

    -- TODO: Ver donde guardar y manejar el tamaño maximo de la barca
    -- TODO: Implementar correctamente el uso de la posicion del barquero (se pueden alternar con succ)
    -- TODO: Revisar como evitar bucles