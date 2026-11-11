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

-- Listas de Casillas y Objetos
listaPosicionesCasilla :: Posicion -> Casilla -> [Pos]
listaPosicionesCasilla (_, mapa, _) casilla =
    M.keys $ M.filter (== casilla) mapa

listaPosicionesObjeto :: Posicion -> Objeto -> [Pos]
listaPosicionesObjeto (_, _, objetos) objeto =
    M.keys $ M.filter (== objeto) objetos

-- Suma de una posición y una dirección
sumaDireccion :: Pos -> Direccion -> Pos
sumaDireccion (x, y) dir = case dir of
    Arriba      -> (x, y - 1)
    Abajo       -> (x, y + 1)
    Izquierda   -> (x - 1, y)
    Derecha     -> (x + 1, y)

-- Verifica si una acción es posible en la posición actual
accionPosible :: Posicion -> Accion -> Bool
accionPosible (jugadores, mapa, objetos) (posJugador, direccion) =
    -- Revisamos que el inicio de la acción sea un jugador
    posJugador `elem` jugadores &&
    -- Revisamos que la casilla a la que se quiere mover el jugador es pisable
    esPisable (mapa M.! posSiguiente) &&
    -- Revisamos que la casilla a la que se quiere mover el jugador no esté ocupada por un objeto
    not (M.member posSiguiente objetos)
    where
        posSiguiente = sumaDireccion posJugador direccion

-- Verifica si una casilla es pisable (no es pared)
esPisable :: Casilla -> Bool
esPisable 
    | Pared   -> False
    | otherwise -> True