module Juegos.Juego_LobObeCol where

import Jugable
import qualified Data.Map as M

data Objeto = Lobo | Oveja | Col deriving (Eq, Ord, Enum, Show)
data PosBarquero = Origen | Destino deriving (Eq, Ord, Enum, Show)

type Posicion = (M.Map Objeto Int, M.Map Objeto Int)

gana :: Objeto -> Objeto -> Bool
gana o1 o2 =
    | o1 == Lobo && o2 == Oveja = True
    | o1 == Oveja && o2 == Col = True
    | otherwise = False

instance Jugable LobObeCol where
    opciones Posicion pos 
        | -- Quitar todas las posibles combinaciones y cantidades de objetos de origen limitadas al tamaño de la barca y ponerlas en destino
        | otherwise = []
        where
            origen = fst pos
            destino = snd pos
    esExito Posicion pos = -- Diccionario origen vacio y diccionario distinto lleno
    -- TODO: Convendria tener una funcion es valida para validar que ninguna isla se quede como no deberia
    -- TODO: Ver donde guardar y manejar el tamaño maximo de la barca
    -- TODO: Implementar correctamente el uso de la posicion del barquero (se pueden alternar con succ)
    -- TODO: Revisar como evitar bucles