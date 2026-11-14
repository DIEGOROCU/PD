--------------------------------------------------
-- Imports y modulos
--------------------------------------------------

module Juegos.Juego_Sokoban where

import Jugable 
import qualified Data.Map as M

--------------------------------------------------
-- Definiciones de tipos
--------------------------------------------------

data Casilla = Pared | Placa | Suelo | PuertaBoton | PuertaPalanca
    deriving (Eq, Ord, Enum, Show)

data Objeto = Caja | Barril | Cuerda | CajaMetal | Boton Pos | Palanca Pos Bool
    deriving (Eq, Ord, Show)
-- Nota:
    -- La caja se puede empujar en fila recta (tantas como se pueda)
    -- El barril se puede empujar y tirar en fila recta (tantos como se pueda)
    -- La cuerda solo se puede tirar en fila recta (tantas como se pueda)
    -- La caja de metal se puede empujar, pero no puede seguir empujando a otro objeto (solo puede empujar uno)

-- Posicion del jugadory y la dirección en la que se mueve

data Direccion = Arriba | Abajo | Izquierda | Derecha
    deriving (Eq, Ord, Enum, Show)

type Pos = (Int, Int)

-- Las accioes las hace el jugador, las reacciones los objetos
data Accion
    = Avanzar Pos Direccion
    | MoverO Pos Direccion
    | InteractuarO Pos Direccion
    deriving (Eq, Show)

type Reaccion = (Pos, Direccion)

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
    deriving (Eq, Ord, Show)

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

    opciones (Sok posicion@(jugadores, _, _)) =
        [ Sok nuevaPosicion
        | jugador <- jugadores
        , direccion <- [Arriba, Abajo, Izquierda, Derecha]
        , accion <- [Avanzar jugador direccion, MoverO jugador direccion,
                     InteractuarO jugador direccion]
        , Just nuevaPosicion <- [ejecutarAccion posicion accion]
        ]

--------------------------------------------------
-- Fuciones Auxiliares simplies
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

-- Verifica si una casilla es pisable (no es pared)
esPisable :: Casilla -> Bool
esPisable Pared = False
-- La puerta del botón y la palanca son solidas cuando están cerradas
esPisable PuertaBoton = False
esPisable PuertaPalanca = False
esPisable _ = True

--------------------------------------------------
-- Metodos auxiliares complejos
--------------------------------------------------

-- Un objeto puede empujar a otro si esta función devuelve True.
puedeEmpujar :: Objeto -> Bool
puedeEmpujar Caja = True
puedeEmpujar Barril = True
puedeEmpujar Cuerda = False
puedeEmpujar CajaMetal = False
-- El botón y la palanca son inamovibles, por lo que no se pueden empujar
puedeEmpujar (Boton _) = False
puedeEmpujar (Palanca _ _) = False

-- Un objeto puede ser tirado por el jugador si esta función devuelve True.
puedeTirar :: Objeto -> Bool
puedeTirar Caja = False
puedeTirar Barril = True
puedeTirar Cuerda = True
puedeTirar CajaMetal = False
-- El botón y la palanca son inamovibles, por lo que no se pueden tirar de ellos
puedeTirar (Boton _) = False
puedeTirar (Palanca _ _) = False

-- Un objeto puede moverse si esta función devuelve True.
puedeMover :: Objeto -> Bool
puedeMover Caja = True
puedeMover Barril = True
puedeMover Cuerda = True
puedeMover CajaMetal = True
-- El botón y la palanca son inamovibles
puedeMover (Boton _) = False
puedeMover (Palanca _ _) = False

-- Una cadena es válida si todos sus objetos pueden desplazarse y cada objeto, salvo el último, puede empujar al siguiente.
cadenaEmpujable :: [Objeto] -> Bool
cadenaEmpujable [] = False
cadenaEmpujable objetos =
    all puedeMover objetos && all puedeEmpujar (init objetos)

-- Obtiene los objetos consecutivos que hay delante del jugador.
cadenaObjetos :: M.Map Pos Objeto -> Pos -> Direccion -> [ (Pos, Objeto) ]
cadenaObjetos objetos posicion direccion =
    case M.lookup posicion objetos of
        Nothing -> []
        Just objeto -> (posicion, objeto) :
            cadenaObjetos objetos (sumaDireccion posicion direccion) direccion

-- Comprueba si una cadena de objetos puede desplazarse una casilla.
empujePosible :: M.Map Pos Casilla -> M.Map Pos Objeto -> Pos -> Direccion -> Bool
empujePosible mapa objetos posicion direccion =
    not (null cadena) &&
    cadenaEmpujable (map snd cadena) &&
    esPisable (casilla destino) &&
    not (M.member destino objetos)
    where
        cadena = cadenaObjetos objetos posicion direccion
        destino = sumaDireccion (fst (last cadena)) direccion
        casilla p = M.findWithDefault Pared p mapa

-- Desplaza todos los objetos de una cadena, empezando por el último.
empujar :: M.Map Pos Objeto -> Pos -> Direccion -> M.Map Pos Objeto
empujar objetos posicion direccion =
    foldr (desplazarObjeto direccion) objetos
        (cadenaObjetos objetos posicion direccion)

-- Comprueba si una cadena de objetos puede ser tirada en una dirección.
tirarPosible :: M.Map Pos Casilla -> M.Map Pos Objeto -> Pos -> Direccion -> Bool
tirarPosible mapa objetos posicion direccion =
    not (null cadena) &&
    all puedeMover objetosCadena &&
    all puedeTirar objetosCadena &&
    esPisable (casilla destino) &&
    not (M.member destino objetos)
    where
        cadena = cadenaObjetos objetos posicion (direccionContraria direccion)
        objetosCadena = map snd cadena
        destino = sumaDireccion (fst (last cadena)) direccion
        casilla p = M.findWithDefault Pared p mapa

-- Desplaza hacia el jugador todos los objetos de una cadena que está detrás.
tirar :: M.Map Pos Objeto -> Pos -> Direccion -> M.Map Pos Objeto
tirar objetos posicion direccion =
    foldr (desplazarObjeto direccion) objetos
        (cadenaObjetos objetos posicion (direccionContraria direccion))

ejecutarAccion :: Posicion -> Accion -> Maybe Posicion
ejecutarAccion posicion (Avanzar posJugador direccion) =
    avanzar posicion posJugador direccion
ejecutarAccion posicion (MoverO posJugador direccion) =
    moverObjetoAccion posicion posJugador direccion
ejecutarAccion posicion (InteractuarO posJugador direccion) =
    interactuarObjeto posicion posJugador direccion

avanzar :: Posicion -> Pos -> Direccion -> Maybe Posicion
avanzar (jugadores, mapa, objetos) posJugador direccion
    -- Revisamos que el jugador esté en la lista de jugadores
    | posJugador `notElem` jugadores = 
        -- Si no, la acción no es posible
        Nothing
    -- Revisamos que la casilla siguiente sea pisable (no se puede mover a una pared)
    | not (esPisable (casilla posSiguiente)) = 
        -- Si no, la acción no es posible
        Nothing
    -- Revisamos que no haya un objeto delante del jugador
    | M.member posSiguiente objetos = 
        -- Si hay un objeto delante, la acción no es posible (no se puede empujar ni tirar con "Avanzar")
        Nothing
    -- En caso de que no haya objetos delante ni detrás, simplemente movemos al jugador
    | otherwise =
        Just 
        (
        moverJugador jugadores posJugador posSiguiente, 
        mapa, 
        objetos
        )
    where
        posSiguiente = sumaDireccion posJugador direccion
        casilla p = M.findWithDefault Pared p mapa

-- Mueve un objeto empujándolo o tirando de él.
moverObjetoAccion (jugadores, mapa, objetos) posJugador direccion
    -- Revisamos que el jugador esté en la lista de jugadores
    | posJugador `notElem` jugadores =
        -- Si no, la acción no es posible
        Nothing
    -- Revisamos que la casilla siguiente sea pisable
    | not (esPisable (casilla posSiguiente)) =
        -- Si no, la acción no es posible
        Nothing
    -- Si hay un objeto delante, intentamos empujarlo
    | M.member posSiguiente objetos =
        if empujePosible mapa objetos posSiguiente direccion
        then Just
            (
                moverJugador jugadores posJugador posSiguiente,
                mapa,
                empujar objetos posSiguiente direccion
            )
        -- Si no se puede empujar, la acción no es posible
        else
            Nothing
    -- Si hay un objeto detrás, intentamos tirar de él
    | M.member posAnterior objetos =
        if tirarPosible mapa objetos posAnterior direccion
        then Just
            (
                moverJugador jugadores posJugador posSiguiente,
                mapa,
                tirar objetos posAnterior direccion
            )
        -- Si no se puede tirar, la acción no es posible
        else
            Nothing
    -- Caso de que no haya objetos delante ni detrás, por lo que no puede ser una accion de "Mover Objeto"
    | otherwise =
        Nothing
    where
        posSiguiente = sumaDireccion posJugador direccion
        posAnterior = sumaDireccion posJugador (direccionContraria direccion)
        casilla p = M.findWithDefault Pared p mapa

-- Interactúa con un botón o una palanca situados delante del jugador.
interactuarObjeto :: Posicion -> Pos -> Direccion -> Maybe Posicion
interactuarObjeto (jugadores, mapa, objetos) posJugador direccion
    -- Revisamos que el jugador esté en la lista de jugadores
    | posJugador `notElem` jugadores =
        -- Si no, la acción no es posible
        Nothing
    -- Si el jugador tiene un objeto interactuable delante, se interactua
    | otherwise =
        case M.lookup posObjeto objetos of
            -- Si hay un botón delante, buscamos su puerta asociada
            Just (Boton posPuerta)
                -- Revisamos que la puerta asociada esté cerrada
                | M.lookup posPuerta mapa == Just PuertaBoton ->
                    -- Abrimos la puerta sin mover al jugador ni al botón
                    Just
                    (
                        jugadores, 
                        M.insert posPuerta Suelo mapa, 
                        objetos
                    )
            -- Si hay una palanca delante, cambiamos su estado y el de su puerta
            Just (Palanca posPuerta activada) ->
                if M.lookup posPuerta mapa ==
                    -- Revisamos que concuerde el estado de la palanca con el estado de la puerta asociada
                    Just (if activada then Suelo else PuertaPalanca)
                then
                    -- Cambiamos el estado de la palanca y de la puerta asociada
                    let nuevaActivacion = not activada
                        nuevaPuerta = if nuevaActivacion
                            then Suelo
                            else PuertaPalanca
                        nuevosObjetos = M.insert posObjeto
                            (Palanca posPuerta nuevaActivacion) objetos
                    in Just 
                    -- Cambiamos el estado de la palanca y de la puerta asociada, sin mover al jugador
                    (
                        jugadores, 
                        M.insert posPuerta nuevaPuerta mapa,
                        nuevosObjetos
                    )
                -- Si la palanca y la puerta no concuerdan, la acción no es posible
                else
                    Nothing
            -- Si no hay botón/palanca o la puerta del botón no está cerrada, la acción no es posible
            _ -> Nothing
    where
        posObjeto = sumaDireccion posJugador direccion

-- Indica si el jugador puede realizar la acción.
accionPosible :: Posicion -> Accion -> Bool
accionPosible posicion accion =
    case ejecutarAccion posicion accion of
        Nothing -> False
        Just _ -> True

moverJugador :: [Pos] -> Pos -> Pos -> [Pos]
moverJugador jugadores origen destino =
    map (desplazar origen destino) jugadores

moverObjeto :: M.Map Pos Objeto -> Pos -> Pos -> M.Map Pos Objeto
moverObjeto objetos origen destino =
    case M.lookup origen objetos of
        Nothing -> objetos
        Just objeto -> M.insert destino objeto (M.delete origen objetos)

desplazarObjeto :: Direccion -> (Pos, Objeto) -> M.Map Pos Objeto -> M.Map Pos Objeto
desplazarObjeto direccion (posicionObjeto, objeto) objetos =
    moverObjeto objetos posicionObjeto
        (sumaDireccion posicionObjeto direccion)

desplazar :: Pos -> Pos -> Pos -> Pos
desplazar origen destino posicion =
    if posicion == origen then destino else posicion

direccionContraria :: Direccion -> Direccion
direccionContraria Arriba = Abajo
direccionContraria Abajo = Arriba
direccionContraria Izquierda = Derecha
direccionContraria Derecha = Izquierda
