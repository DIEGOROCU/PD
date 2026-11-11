--------------------------------------------------
-- Imports y modulos
--------------------------------------------------

module Juegos.Juego_Sokoban where

import Jugable 
import qualified Data.Map as M

--------------------------------------------------
-- Definiciones de tipos
--------------------------------------------------

data Casilla = Pared | Placa | Suelo
    deriving (Eq, Ord, Enum, Show)

data Objeto = Caja | Barril 
    deriving (Eq, Ord, Enum, Show)
-- Nota: las cajas se pueden empujar, los barriles se pueden empujar y tirar

data Direccion = Arriba | Abajo | Izquierda | Derecha
    deriving (Eq, Ord, Enum, Show)

type Pos = (Int, Int)

-- Posicion del jugadory y la dirección en la que se mueve
type Accion = (Pos, Direccion)

type Posicion =
    (
        -- Lista de posiciones de los jugadores
        [Pos],
        -- Diccionario que describe el mapa del juego
        M.Map Pos Casilla,
        -- Diccionario que describe la posición de los objetos
        M.Map Pos Objeto
    )

-- Juego
newtype Sokoban = Sok Posicion 
    deriving (Eq, Show)

--------------------------------------------------
-- Instancia de Jugable
--------------------------------------------------

instance Jugable Sokoban where

    -- Función esExito: se gana cuando todas las placas están ocupadas por un jugador o un objeto
    esExito (Sok (jugadores, mapa, objetos)) =
        all estaPisada placas
        where
            placas      = listaPosicionesCasilla (jugadores, mapa, objetos) Placa
            ocupadas    = jugadores ++ M.keys objetos
            estaPisada p = p `elem` ocupadas

    opciones (Sok (jugadores, mapa, objetos)) =
        map
        (\j -> (j, Sok (jugadores, mapa, objetos)))
        jugadores
        where

--------------------------------------------------
-- Fuciones Auxiliares
--------------------------------------------------

listaPosicionesCasilla :: Posicion -> Casilla -> [Pos]
listaPosicionesCasilla (_, mapa, _) casilla =
    M.keys $ M.filter (== casilla) mapa

listaPosicionesObjeto :: Posicion -> Objeto -> [Pos]
listaPosicionesObjeto (_, _, objetos) objeto =
    M.keys $ M.filter (== objeto) objetos

accionPosible :: Posicion -> Accion -> Bool