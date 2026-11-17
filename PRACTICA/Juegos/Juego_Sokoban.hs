--------------------------------------------------
-- Imports y modulos
--------------------------------------------------

module Juegos.Juego_Sokoban where

import Jugable 
import qualified Data.Map as M

--------------------------------------------------
-- Definiciones de tipos
--------------------------------------------------

data Casilla
    = Pared
    | Limite
    | Placa
    | Aire
    | PuertaBoton
    | PuertaPalanca
    | Escalera
    | PlacaActivadoraTemporizada Posicion
    | PuertaTemporizada Bool Int Int
    deriving (Eq, Ord, Show)

data Objeto = Caja | Barril | Cuerda | CajaMetal | Globo | Boton Posicion | Palanca Posicion Bool
    deriving (Eq, Ord, Show)
-- Nota:
    -- La caja se puede empujar en fila recta (tantas como se pueda)
    -- El barril se puede empujar en fila recta (tantos como se pueda) y tirar de uno en uno
    -- La cuerda se puede empujar y tirar en fila recta y sirve como adhesivo para unir objetos al tirar
    -- La caja de metal solo se puede empujar y únicamente se mueve sola (prioridad sobre adhesivos)
    -- El globo no tiene gravedad, se puede empujar y tirar (también hacia arriba y abajo) y empujar en cadena
    -- El botón se puede activar para abrir una puerta asociada definitivamente (no se puede cerrar)
    -- La palanca se puede activar para abrir o cerrar una puerta asociada (se puede activar varias veces)
    -- Todos los objetos son sólidos (se puede caminar sobre ellos) y tienen gravedad salvo el globo y los mecanismos fijos

-- Direcciones en el espacio 3D:
--   Eje X: Izquierda (x - 1) / Derecha (x + 1)
--   Eje Y: Atras (y - 1)     / Adelante (y + 1)
--   Eje Z: Abajo (z - 1)     / Arriba (z + 1)
data Direccion = Arriba | Abajo | Izquierda | Derecha | Adelante | Atras
    deriving (Eq, Ord, Enum, Bounded, Show)

type Posicion = (Int, Int, Int)
type Jugador = Posicion
type MapaCasillas = M.Map Posicion Casilla
type MapaObjetos = M.Map Posicion Objeto

-- Las acciones las hace el jugador, las reacciones los objetos
data Accion
    = AccionAvanzar Jugador Direccion
    | AccionMoverObjeto Jugador Direccion
    | AccionInteractuarObjeto Jugador Direccion
    deriving (Eq, Show)

type Reaccion = (Posicion, Direccion)

type Estado =
    (
        -- Lista de posiciones de los jugadores
        [Jugador],
        -- Diccionario que describe el mapa del juego
        MapaCasillas,
        -- Diccionario que describe la posición de los objetos
        MapaObjetos
    )

-- Juego
newtype Sokoban = Sok Estado 
    deriving (Eq, Ord, Show)

--------------------------------------------------
-- Instancia de Jugable
--------------------------------------------------

instance Jugable Sokoban where

    -- Función esExito: se gana cuando todas las placas están ocupadas por un jugador o un objeto
    esExito (Sok (jugadores, mapaJuego, objetos)) =
        all estaPisada placas
        where
            placas       = listaPosicionesCasilla (jugadores, mapaJuego, objetos) Placa
            ocupadas     = jugadores ++ M.keys objetos
            estaPisada p = p `elem` ocupadas

    opciones (Sok estado@(jugadores, _, _))
            | estadoEstable /= estado = [Sok estadoEstable]
            | otherwise =
                [ Sok nuevaPosicion
                | jugador <- jugadores
                , direccion <- [minBound .. maxBound]
                , accion <- [AccionAvanzar jugador direccion,
                            AccionMoverObjeto jugador direccion,
                            AccionInteractuarObjeto jugador direccion]
                , Just nuevaPosicion <- [ejecutarAccion estado accion]
                ]
            where
                estadoEstable = aplicarGravedadPaso estado

--------------------------------------------------
-- Funciones Auxiliares simples
--------------------------------------------------

-- Listas de Casillas y Objetos
listaPosicionesCasilla :: Estado -> Casilla -> [Posicion]
listaPosicionesCasilla (_, mapaJuego, _) casilla =
    M.keys $ M.filter (== casilla) mapaJuego

listaPosicionesObjeto :: Estado -> Objeto -> [Posicion]
listaPosicionesObjeto (_, _, objetos) objeto =
    M.keys $ M.filter (== objeto) objetos

-- Suma de una posición 3D y una dirección
sumaDireccion :: Posicion -> Direccion -> Posicion
sumaDireccion (x, y, z) dir = case dir of
    Derecha   -> (x + 1, y, z)
    Izquierda -> (x - 1, y, z)
    Adelante  -> (x, y + 1, z)
    Atras     -> (x, y - 1, z)
    Arriba    -> (x, y, z + 1)
    Abajo     -> (x, y, z - 1)

esDireccionVertical :: Direccion -> Bool
esDireccionVertical Arriba = True
esDireccionVertical Abajo  = True
esDireccionVertical _      = False

esDireccionHorizontal :: Direccion -> Bool
esDireccionHorizontal = not . esDireccionVertical

-- Verifica si una casilla es pisable/atravesable
esPisable :: Casilla -> Bool
esPisable Pared                          = False
esPisable Limite                         = False
esPisable PuertaBoton                    = False
esPisable PuertaPalanca                  = False
esPisable Escalera                       = False
esPisable (PuertaTemporizada abierta _ _) = abierta
esPisable _                              = True

-- Verifica si una casilla actúa como suelo sólido que soporta peso encima
esSueloSolido :: Casilla -> Bool
esSueloSolido Pared                          = True
esSueloSolido Limite                         = True
esSueloSolido PuertaBoton                    = True
esSueloSolido PuertaPalanca                  = True
esSueloSolido Escalera                       = True
esSueloSolido (PuertaTemporizada abierta _ _) = not abierta
esSueloSolido _                              = False

-- Un objeto puede ser empujado si esta función devuelve True.
esEmpujable :: Objeto -> Bool
esEmpujable Caja      = True
esEmpujable Barril    = True
esEmpujable Cuerda    = True
esEmpujable CajaMetal = True
esEmpujable Globo     = True
esEmpujable _         = False

-- Un objeto puede ser tirado directamente por el jugador si esta función devuelve True.
esTirable :: Objeto -> Bool
esTirable Caja          = False
esTirable Barril        = True
esTirable Cuerda        = True
esTirable CajaMetal     = False
esTirable Globo         = True
esTirable (Boton _)     = False
esTirable (Palanca _ _) = False

-- Un objeto puede desplazarse en el plano horizontal (X o Y) si esta función devuelve True.
esMovibleHorizontalmente :: Objeto -> Bool
esMovibleHorizontalmente Caja      = True
esMovibleHorizontalmente Barril    = True
esMovibleHorizontalmente Cuerda    = True
esMovibleHorizontalmente CajaMetal = True
esMovibleHorizontalmente Globo     = True
esMovibleHorizontalmente _         = False

-- Un objeto puede empujarse o tirarse en dirección vertical (Z: Arriba o Abajo).
esMovibleVerticalmente :: Objeto -> Bool
esMovibleVerticalmente Globo = True
esMovibleVerticalmente _     = False

-- Un objeto actúa como adhesivo al tirar (une objetos contiguos en la cadena de tirón).
esAdhesivo :: Objeto -> Bool
esAdhesivo Cuerda = True
esAdhesivo _      = False

-- Un objeto únicamente puede moverse solo (tiene prioridad sobre las reglas de adhesión).
soloSeMueveSola :: Objeto -> Bool
soloSeMueveSola CajaMetal = True
soloSeMueveSola _         = False

-- Un objeto se ve afectado por la gravedad (cae en -Z) si esta función devuelve True.
tieneGravedad :: Objeto -> Bool
tieneGravedad Caja      = True
tieneGravedad Barril    = True
tieneGravedad Cuerda    = True
tieneGravedad CajaMetal = True
tieneGravedad _         = False

--------------------------------------------------
-- Metodos auxiliares complejos
--------------------------------------------------

-- Una cadena es válida si todos sus objetos son empujables,
-- y no contiene ningún objeto que solo pueda moverse solo dentro de una cadena de varios elementos.
cadenaEmpujable :: [Objeto] -> Bool
cadenaEmpujable [] = False
cadenaEmpujable objetos =
    all esEmpujable objetos &&
    (length objetos == 1 || not (any soloSeMueveSola objetos))

-- Obtiene los objetos consecutivos que hay delante del jugador.
cadenaObjetos :: MapaObjetos -> Posicion -> Direccion -> [ (Posicion, Objeto) ]
cadenaObjetos objetos posicion direccion =
    case M.lookup posicion objetos of
        Nothing -> []
        Just objeto -> (posicion, objeto) :
            cadenaObjetos objetos (sumaDireccion posicion direccion) direccion

-- Construye la cadena de objetos que se arrastran al tirar:
-- el primer objeto debe ser tirable y solo se encadenan los siguientes si hay una cuerda adhesiva uniendo ambos.
cadenaTirar :: MapaObjetos -> Posicion -> Direccion -> [ (Posicion, Objeto) ]
cadenaTirar objetos posInicial dirTirar =
    case M.lookup posInicial objetos of
        Nothing -> []
        Just primero
            | not (esTirable primero) -> []
            | otherwise -> (posInicial, primero) : encadenar primero (sumaDireccion posInicial dirAtras)
    where
        dirAtras = direccionContraria dirTirar
        encadenar objAnterior posActual =
            case M.lookup posActual objetos of
                Nothing -> []
                Just objActual
                    | esAdhesivo objAnterior || esAdhesivo objActual ->
                        (posActual, objActual) : encadenar objActual (sumaDireccion posActual dirAtras)
                    | otherwise -> []

-- Comprueba si una cadena de objetos puede desplazarse una casilla en 3D.
empujePosible :: [Jugador] -> MapaCasillas -> MapaObjetos -> Posicion -> Direccion -> Bool
empujePosible jugadores mapaJuego objetos posicion direccion =
    not (null cadena) &&
    cadenaEmpujable objetosCadena &&
    (esDireccionHorizontal direccion || all esMovibleVerticalmente objetosCadena) &&
    esPisable (casilla destino) &&
    not (M.member destino objetos) &&
    destino `notElem` jugadores
    where
        cadena = cadenaObjetos objetos posicion direccion
        objetosCadena = map snd cadena
        destino = sumaDireccion (fst (last cadena)) direccion
        casilla p = M.findWithDefault Limite p mapaJuego

-- Desplaza todos los objetos de una cadena, empezando por el último.
empujar :: MapaObjetos -> Posicion -> Direccion -> MapaObjetos
empujar objetos posicion direccion =
    foldr (desplazarObjeto direccion) objetos
        (cadenaObjetos objetos posicion direccion)

-- Comprueba si una cadena de objetos puede ser tirada en una dirección en 3D.
tirarPosible :: MapaCasillas -> MapaObjetos -> Posicion -> Direccion -> Bool
tirarPosible mapaJuego objetos posicion direccion =
    not (null cadena) &&
    (esDireccionHorizontal direccion && all esMovibleHorizontalmente objetosCadena ||
     esDireccionVertical direccion && all esMovibleVerticalmente objetosCadena) &&
    (length objetosCadena == 1 || not (any soloSeMueveSola objetosCadena)) &&
    esPisable (casilla destino) &&
    not (M.member destino objetos)
    where
        cadena = cadenaTirar objetos posicion direccion
        objetosCadena = map snd cadena
        destino = sumaDireccion posicion direccion
        casilla p = M.findWithDefault Limite p mapaJuego

-- Desplaza hacia el jugador todos los objetos enganchados en la cadena de tirón, empezando por el más cercano.
tirar :: MapaObjetos -> Posicion -> Direccion -> MapaObjetos
tirar objetos posicion direccion =
    foldl (flip (desplazarObjeto direccion)) objetos
        (cadenaTirar objetos posicion direccion)

-- Comprueba si una posición tiene una escalera adyacente en el plano horizontal (X o Y).
estaAgarradoAEscalera :: MapaCasillas -> Posicion -> Bool
estaAgarradoAEscalera mapaJuego pos =
    any (\dir -> M.findWithDefault Limite (sumaDireccion pos dir) mapaJuego == Escalera)
        [Derecha, Izquierda, Adelante, Atras]

-- Sube un único bloque por turno: si la escalera continúa arriba, sube 1 en Z junto a ella;
-- si es el último tramo, corona colocándose encima.
subirPorEscalera :: [Jugador] -> MapaCasillas -> MapaObjetos -> Posicion -> Posicion -> Maybe Posicion
subirPorEscalera jugadores mapaJuego objetos posJugador posEscalera =
    let arribaJugador  = sumaDireccion posJugador Arriba
        arribaEscalera = sumaDireccion posEscalera Arriba
    in if not (esLibre arribaJugador)
       then Nothing
       else case M.findWithDefault Limite arribaEscalera mapaJuego of
           Escalera -> Just arribaJugador
           _        -> if esLibre arribaEscalera then Just arribaEscalera else Nothing
    where
        esLibre p = esPisable (M.findWithDefault Limite p mapaJuego)
                    && not (M.member p objetos)
                    && p `notElem` jugadores

-- Comprueba si una posición tiene apoyo real hasta tocar suelo sólido, escalera o un objeto sin gravedad (ej. Globo)
tieneApoyo :: [Jugador] -> MapaCasillas -> MapaObjetos -> Posicion -> Bool
tieneApoyo jugadores mapaJuego objetos pos
    | esSueloSolido casillaDebajo = True
    | pos `elem` jugadores && estaAgarradoAEscalera mapaJuego pos = True
    | Just objDebajo <- M.lookup debajo objetos =
        not (tieneGravedad objDebajo) || tieneApoyo jugadores mapaJuego objetos debajo
    | debajo `elem` jugadores =
        tieneApoyo jugadores mapaJuego objetos debajo
    | otherwise = False
    where
        debajo = sumaDireccion pos Abajo
        casillaDebajo = M.findWithDefault Limite debajo mapaJuego

-- Aplica un único paso de gravedad (1 casilla en -Z) a los jugadores y objetos sin apoyo.
aplicarGravedadPaso :: Estado -> Estado
aplicarGravedadPaso estado@(jugadores, mapaJuego, objetos) =
    (nuevosJugadores, mapaJuego, nuevosObjetos)
    where
        caerJugador j
            | tieneApoyo jugadores mapaJuego objetos j = j
            | otherwise = sumaDireccion j Abajo
        nuevosJugadores = map caerJugador jugadores

        nuevosObjetos = M.foldrWithKey moverSiCae M.empty objetos
        moverSiCae pos obj acc
            | tieneGravedad obj && not (tieneApoyo jugadores mapaJuego objetos pos) =
                M.insert (sumaDireccion pos Abajo) obj acc
            | otherwise =
                M.insert pos obj acc

-- Actualiza el estado de las puertas temporizadas después de cada acción.
actualizarPuertas :: Estado -> Estado
actualizarPuertas (jugadores, mapaJuego, objetos) =
    (jugadores, M.mapWithKey actualizar mapaJuego, objetos)
    where
        ocupadas = jugadores ++ M.keys objetos

        actualizar posicion (PuertaTemporizada abierta tiempo duracion) =
            actualizarPuerta posicion abierta tiempo duracion
        actualizar _ casilla = casilla

        actualizarPuerta posicion abierta tiempo duracion
            | abierta && tiempo > 1 =
                PuertaTemporizada True (tiempo - 1) duracion
            -- Si el tiempo se agota pero alguien ocupa el hueco de la puerta, espera 1 turno a que salga
            | abierta && posicion `elem` ocupadas =
                PuertaTemporizada True 1 duracion
            -- Cuando se consume la última acción y el hueco está libre, la puerta vuelve a cerrarse
            | abierta =
                PuertaTemporizada False (-1) duracion
            -- Una puerta cerrada se abre si todas sus placas asociadas están ocupadas
            | placasActivadas posicion =
                PuertaTemporizada True duracion duracion
            | otherwise =
                PuertaTemporizada False (-1) duracion

        placasActivadas posicion =
            not (null placas) && all (`elem` ocupadas) placas
            where
                placas = [ posicionPlaca
                         | (posicionPlaca, PlacaActivadoraTemporizada puerta) <- M.toList mapaJuego
                         , puerta == posicion
                         ]

-- Actualiza las puertas temporizadas al terminar la acción del jugador;
-- la caída paso a paso la gestiona `opciones` en los estados transitorios siguientes.
actualizacionFinal :: Estado -> Estado
actualizacionFinal = actualizarPuertas

-- Ejecuta una acción y aplica gravedad y actualización de puertas temporizadas.
ejecutarAccion :: Estado -> Accion -> Maybe Estado
ejecutarAccion estado accion =
    fmap actualizacionFinal (ejecutarAccionSinGravedad estado accion)

-- Ejecuta una acción sin aplicar gravedad ni actualización de puertas temporizadas.
ejecutarAccionSinGravedad :: Estado -> Accion -> Maybe Estado
ejecutarAccionSinGravedad estado (AccionAvanzar posJugador direccion) =
    accionAvanzar estado posJugador direccion
ejecutarAccionSinGravedad estado (AccionMoverObjeto posJugador direccion) =
    accionMoverObjeto estado posJugador direccion
ejecutarAccionSinGravedad estado (AccionInteractuarObjeto posJugador direccion) =
    accionInteractuarObjeto estado posJugador direccion

accionAvanzar :: Estado -> Jugador -> Direccion -> Maybe Estado
accionAvanzar (jugadores, mapaJuego, objetos) posJugador direccion
    -- El jugador debe existir en el estado.
    | posJugador `notElem` jugadores =
        Nothing
    -- Si está agarrado a una escalera, puede bajar 1 peldaño en Z con Abajo.
    | direccion == Abajo && estaAgarradoAEscalera mapaJuego posJugador =
        if esPisable (casilla posSiguiente) && not (M.member posSiguiente objetos) && posSiguiente `notElem` jugadores
        then Just (moverJugador jugadores posJugador posSiguiente, mapaJuego, objetos)
        else Nothing
    -- El resto de movimientos verticales directos (Arriba, o Abajo sin escalera) no están permitidos.
    | esDireccionVertical direccion =
        Nothing
    -- Si hay una escalera delante, se intenta subir 1 peldaño por ella.
    | casilla posSiguiente == Escalera =
        case subirPorEscalera jugadores mapaJuego objetos posJugador posSiguiente of
            Just destinoSubida ->
                Just
                (
                    moverJugador jugadores posJugador destinoSubida,
                    mapaJuego,
                    objetos
                )
            Nothing -> Nothing
    -- No se puede avanzar hacia una casilla sólida.
    | not (esPisable (casilla posSiguiente)) =
        Nothing
    -- No se puede atravesar un objeto.
    | M.member posSiguiente objetos =
        Nothing
    -- Los jugadores no pueden ocupar la misma posición.
    | posSiguiente `elem` jugadores =
        Nothing
    -- Si ninguna restricción lo impide, el jugador avanza.
    | otherwise =
        Just 
        (
            moverJugador jugadores posJugador posSiguiente, 
            mapaJuego, 
            objetos
        )
    where
        posSiguiente = sumaDireccion posJugador direccion
        casilla p = M.findWithDefault Limite p mapaJuego

-- Mueve un objeto empujándolo o tirando de él.
accionMoverObjeto :: Estado -> Jugador -> Direccion -> Maybe Estado
accionMoverObjeto (jugadores, mapaJuego, objetos) posJugador direccion
    -- El jugador debe existir en el estado.
    | posJugador `notElem` jugadores =
        Nothing
    -- No se puede mover hacia una casilla sólida.
    | not (esPisable (casilla posSiguiente)) =
        Nothing
    -- Los jugadores no pueden ocupar la misma posición.
    | posSiguiente `elem` jugadores =
        Nothing
    -- Si hay un objeto delante, se intenta empujarlo.
    | M.member posSiguiente objetos && empujePosible jugadores mapaJuego objetos posSiguiente direccion =
        Just
            (
                moverJugador jugadores posJugador posSiguiente,
                mapaJuego,
                empujar objetos posSiguiente direccion
            )
    -- Si hay un objeto detrás, se intenta tirar de él.
    | not (M.member posSiguiente objetos) && M.member posAnterior objetos && tirarPosible mapaJuego objetos posAnterior direccion =
        Just
            (
                moverJugador jugadores posJugador posSiguiente,
                mapaJuego,
                tirar objetos posAnterior direccion
            )
    -- Si no se puede empujar ni tirar, la acción falla.
    | otherwise =
        Nothing
    where
        posSiguiente = sumaDireccion posJugador direccion
        posAnterior = sumaDireccion posJugador (direccionContraria direccion)
        casilla p = M.findWithDefault Limite p mapaJuego

-- Interactúa con un botón o una palanca situados delante del jugador.
accionInteractuarObjeto :: Estado -> Jugador -> Direccion -> Maybe Estado
accionInteractuarObjeto (jugadores, mapaJuego, objetos) posJugador direccion
    -- El jugador debe existir en el estado.
    | posJugador `notElem` jugadores =
        Nothing
    -- Se comprueba el mecanismo situado delante del jugador.
    | otherwise =
        case M.lookup posObjeto objetos of
            Just (Boton posPuerta)
                -- Un botón abre una puerta cerrada de forma permanente.
                | M.lookup posPuerta mapaJuego == Just PuertaBoton ->
                    Just
                    (
                        jugadores, 
                        M.insert posPuerta Aire mapaJuego, 
                        objetos
                    )
            Just (Palanca posPuerta activada) ->
                -- La palanca solo cambia si coincide con el estado de su puerta.
                if M.lookup posPuerta mapaJuego ==
                    Just (if activada then Aire else PuertaPalanca)
                    && (not activada || (posPuerta `notElem` jugadores && not (M.member posPuerta objetos)))
                then
                    let nuevaActivacion = not activada
                        nuevaPuerta = if nuevaActivacion
                            then Aire
                            else PuertaPalanca
                        nuevosObjetos = M.insert posObjeto
                            (Palanca posPuerta nuevaActivacion) objetos
                    in Just 
                    (
                        jugadores, 
                        M.insert posPuerta nuevaPuerta mapaJuego,
                        nuevosObjetos
                    )
                else
                    Nothing
            -- Cualquier otro objeto no permite interacción.
            _ -> Nothing
    where
        posObjeto = sumaDireccion posJugador direccion

-- Indica si el jugador puede realizar la acción.
accionPosible :: Estado -> Accion -> Bool
accionPosible estado accion =
    case ejecutarAccion estado accion of
        Nothing -> False
        Just _ -> True

moverJugador :: [Jugador] -> Jugador -> Jugador -> [Jugador]
moverJugador jugadores origen destino =
    map (desplazar origen destino) jugadores

moverObjeto :: MapaObjetos -> Posicion -> Posicion -> MapaObjetos
moverObjeto objetos origen destino =
    case M.lookup origen objetos of
        Nothing -> objetos
        Just objeto -> M.insert destino objeto (M.delete origen objetos)

desplazarObjeto :: Direccion -> (Posicion, Objeto) -> MapaObjetos -> MapaObjetos
desplazarObjeto direccion (posicionObjeto, objeto) objetos =
    moverObjeto objetos posicionObjeto
        (sumaDireccion posicionObjeto direccion)

desplazar :: Posicion -> Posicion -> Posicion -> Posicion
desplazar origen destino posicion =
    if posicion == origen then destino else posicion

direccionContraria :: Direccion -> Direccion
direccionContraria Arriba    = Abajo
direccionContraria Abajo     = Arriba
direccionContraria Izquierda = Derecha
direccionContraria Derecha   = Izquierda
direccionContraria Adelante  = Atras
direccionContraria Atras     = Adelante

--------------------------------------------------
-- Ejemplos de Sokoban 3D
--------------------------------------------------

-- Genera un volumen 3D de aire (Aire) de dimensiones ancho (X) x fondo (Y) x alto (Z)
mapa :: Int -> Int -> Int -> MapaCasillas
mapa ancho fondo alto =
    M.fromList [((x, y, z), Aire) | x <- [0..ancho - 1], y <- [0..fondo - 1], z <- [0..alto - 1]]

-- Ejemplos de Sokoban 3D con solución
conSolucion :: [Sokoban]
conSolucion =
    [
    Sok ( [(0, 0, 0)]
        , M.unions
            [
              M.fromList [ ((3, 0, 0), Placa)                                  -- 1. Objetivo CajaMetal
                         , ((8, 0, 0), Placa)                                  -- 6. Objetivo Barril que cae desde z=4
                         , ((5, 2, 0), PuertaPalanca)                          -- Puerta controlada por Palanca (3,1,0)
                         , ((4, 2, 0), PlacaActivadoraTemporizada (6, 2, 0))   -- Placa temporizada (abre (6,2,0) 7 turnos)
                         , ((6, 2, 0), PuertaTemporizada False (-1) 7)         -- Puerta temporizada antes del Globo
                         , ((6, 1, 0), Escalera)                               -- Base Escalera 1
                         , ((0, 1, 0), Pared), ((1, 1, 0), Pared)
                         , ((0, 2, 0), Pared), ((1, 2, 0), Pared)
                         , ((4, 0, 0), Pared), ((5, 0, 0), Pared), ((6, 0, 0), Pared), ((7, 0, 0), Pared), ((9, 0, 0), Pared)
                         , ((4, 1, 0), Pared), ((5, 1, 0), Pared), ((7, 1, 0), Pared), ((8, 1, 0), Pared), ((9, 1, 0), Pared)
                         , ((8, 2, 0), Pared), ((9, 2, 0), Pared)
                         ]

            , M.fromList [ ((6, 1, 1), Escalera)
                         , ((1, 1, 1), Pared), ((2, 1, 1), Pared), ((3, 1, 1), Pared), ((4, 1, 1), Pared), ((5, 1, 1), Pared)
                         , ((5, 0, 1), Pared), ((6, 0, 1), Pared), ((7, 0, 1), Pared), ((7, 1, 1), Pared)
                         ]

            , M.fromList [ ((2, 1, 2), Placa), ((3, 1, 2), Placa), ((4, 1, 2), Placa) -- 2, 3, 4. Cadena Caja-Cuerda-Barril
                         , ((6, 0, 2), Escalera)      -- Base Escalera 2
                         , ((0, 1, 2), Pared), ((8, 1, 2), Pared)
                         , ((4, 0, 2), Pared), ((8, 0, 2), Pared)
                         ]

            , M.fromList [ ((6, 0, 3), Escalera)
                         , ((8, 2, 3), Escalera)      -- Escalera final desde el Globo hasta z=4
                         , ((6, 1, 3), Pared), ((6, 2, 3), Pared)
                         , ((8, 1, 3), Pared), ((8, 0, 3), Pared)
                         ]

            , M.fromList [ ((8, 2, 4), PuertaBoton)   -- Puerta controlada por Boton (5,0,2)
                         , ((8, 1, 4), Placa)         -- 5. Objetivo final del jugador
                         , ((5, 0, 4), Pared), ((5, 1, 4), Pared), ((5, 2, 4), Pared)
                         , ((7, 0, 4), Pared), ((7, 1, 4), Pared)
                         , ((9, 1, 4), Pared), ((9, 2, 4), Pared)
                         ]

            , mapa 10 3 6
            ]
        , M.fromList
            [ ((1, 0, 0), CajaMetal)
            , ((3, 1, 0), Palanca (5, 2, 0) False)
            , ((7, 2, 1), Globo)
            , ((1, 1, 2), Caja)
            , ((2, 1, 2), Cuerda)
            , ((3, 1, 2), Barril)
            , ((5, 0, 2), Boton (8, 2, 4))
            , ((8, 0, 4), Barril)
            ]
        ),

    -- La fila de barril, cuerda, barril y cuerda se puede tirar desde la segunda columna.
    Sok ([(1, 1, 0)],
        M.union
        (M.fromList [((1, 0, 0), Placa), ((1, 1, 0), Placa)])
        (mapa 6 2 1),
        M.fromList [((2, 1, 0), Barril), ((3, 1, 0), Cuerda),
                    ((4, 1, 0), Barril), ((5, 1, 0), Cuerda)]),

    -- Los dos jugadores empujan sus cajas y luego pisan sus respectivas placas.
    Sok ([(1, 0, 0), (6, 0, 0)],
        M.union
        (M.fromList [((0, 0, 0), Placa), ((3, 0, 0), Placa),
                        ((4, 0, 0), Placa), ((7, 0, 0), Placa)])
        (mapa 8 1 1),
        M.fromList [((2, 0, 0), Caja), ((5, 0, 0), Caja)]),

    Sok ( [(4, 3, 0)]
        , M.union
            (M.fromList
              [ ((0, 3, 0), Placa)
              , ((1, 3, 0), Pared)
              , ((2, 2, 0), PuertaTemporizada False (-1) 10)
              , ((3, 2, 0), Pared)
              , ((3, 2, 1), Pared)
              , ((3, 2, 2), Pared)
              , ((3, 3, 1), Pared)
              , ((3, 3, 2), Pared)
              , ((4, 0, 0), Escalera)
              , ((4, 2, 0), Pared)
              , ((4, 2, 1), Pared)
              , ((4, 2, 2), Pared)
              , ((4, 3, 1), Pared)
              , ((4, 3, 2), Pared)
              , ((5, 1, 2), Pared)
              , ((5, 2, 0), Pared)
              , ((5, 2, 1), Pared)
              , ((5, 2, 2), Pared)
              , ((5, 3, 0), PlacaActivadoraTemporizada (2, 2, 0))
              , ((5, 3, 1), Pared)
              , ((5, 3, 2), Pared)
              , ((5, 3, 3), Placa)
              ])
            (mapa 6 4 4)
        , M.fromList
            [ ((0, 0, 0), Barril)
            , ((5, 0, 0), Globo)
            ]
        ),
            
    -- El jugador tiene que usar la cuerda para sacar la caja de la esquina y empujarla hasta la placa.
    Sok ( [(4, 3, 0)]
        , M.union
            (M.fromList
              [ ((0, 0, 0), Placa)
              , ((2, 0, 0), Placa)
              , ((4, 0, 0), Placa)
              ])
            (mapa 5 4 1)
        , M.fromList
            [ ((0, 3, 0), Caja)
            , ((3, 0, 0), Cuerda)
            ]
        ),

    -- El jugador tiene que usar dos globos para subir a la placa.
    Sok ( [(1, 1, 0)]
        , M.union
            (M.fromList
              [ ((0, 0, 0), Escalera)
              , ((0, 1, 5), Pared)
              , ((0, 1, 6), Placa)
              ])
            (mapa 2 2 7)
        , M.fromList
            [ ((1, 0, 0), Globo)
            , ((1, 1, 2), Globo)
            ]
        ),

    Sok ( [(5, 2, 0)]
        , M.union
            (M.fromList
              [ ((0, 0, 0), Escalera)
              , ((0, 0, 1), Escalera)
              , ((0, 0, 2), Escalera)
              , ((0, 0, 3), Escalera)
              , ((0, 0, 4), Escalera)
              , ((0, 1, 4), Pared)
              , ((0, 5, 0), Escalera)
              , ((1, 1, 0), Pared)
              , ((1, 1, 1), Pared)
              , ((1, 1, 2), Pared)
              , ((1, 1, 3), Pared)
              , ((1, 1, 4), Pared)
              , ((1, 2, 0), Pared)
              , ((1, 2, 1), Pared)
              , ((1, 2, 2), Pared)
              , ((1, 2, 3), Pared)
              , ((1, 2, 4), Pared)
              , ((1, 3, 0), Pared)
              , ((1, 3, 1), Pared)
              , ((1, 3, 2), Pared)
              , ((1, 3, 3), Pared)
              , ((1, 3, 4), Pared)
              , ((1, 4, 0), Pared)
              , ((1, 4, 1), Pared)
              , ((1, 4, 2), Pared)
              , ((1, 4, 3), Pared)
              , ((1, 4, 4), Pared)
              , ((2, 1, 0), Pared)
              , ((2, 1, 1), Pared)
              , ((2, 1, 2), Pared)
              , ((2, 1, 3), Pared)
              , ((2, 1, 4), Pared)
              , ((2, 3, 0), Placa)
              , ((2, 4, 0), Pared)
              , ((2, 4, 1), Pared)
              , ((2, 4, 2), Pared)
              , ((2, 4, 3), Pared)
              , ((2, 4, 4), Pared)
              , ((3, 1, 0), Pared)
              , ((3, 1, 1), Pared)
              , ((3, 1, 2), Pared)
              , ((3, 1, 3), Pared)
              , ((3, 1, 4), Pared)
              , ((3, 4, 0), Pared)
              , ((3, 4, 1), Pared)
              , ((3, 4, 2), Pared)
              , ((3, 4, 3), Pared)
              , ((3, 4, 4), Pared)
              , ((4, 1, 0), Pared)
              , ((4, 1, 1), Pared)
              , ((4, 1, 2), Pared)
              , ((4, 1, 3), Pared)
              , ((4, 1, 4), Pared)
              , ((4, 2, 0), Pared)
              , ((4, 2, 1), Pared)
              , ((4, 2, 2), Pared)
              , ((4, 2, 3), Pared)
              , ((4, 2, 4), Pared)
              , ((4, 3, 0), Pared)
              , ((4, 3, 1), Pared)
              , ((4, 3, 2), Pared)
              , ((4, 3, 3), Pared)
              , ((4, 3, 4), Pared)
              , ((4, 4, 0), Pared)
              , ((4, 4, 1), Pared)
              , ((4, 4, 2), Pared)
              , ((4, 4, 3), Pared)
              , ((4, 4, 4), Pared)
              , ((5, 0, 0), Escalera)
              , ((5, 5, 0), Escalera)
              ])
            (mapa 6 6 7)
        , M.fromList
            [ ((1, 5, 0), Globo)
            , ((4, 0, 0), Globo)
            , ((5, 4, 0), Globo)
            ]
        )
    ]

-- Ejemplos sencillos de Sokoban 3D sin solución.
sinSolucion :: [Sokoban]
sinSolucion =
    [
-- La placa está aislada por una pared.
        Sok ([(0, 0, 0)], M.fromList [((0, 0, 0), Aire), ((2, 0, 0), Placa)], M.empty),

        -- La caja está contra una pared y no puede llegar a la placa.
        Sok ([(0, 0, 0)],
            M.fromList
            [((0, 0, 0), Aire), ((1, 0, 0), Aire), ((2, 0, 0), Pared),
             ((3, 0, 0), Placa)],
            M.fromList [((1, 0, 0), Caja)]),

        -- La puerta está cerrada y no hay ningún botón que pueda abrirla.
        Sok ([(0, 0, 0)],
            M.fromList
            [((0, 0, 0), Aire), ((1, 0, 0), PuertaBoton), ((2, 0, 0), Placa)],
            M.empty),

        -- La caja de metal no puede empujar la caja que tiene delante hasta la placa.
        Sok ([(0, 0, 0)],
            M.fromList
            [((0, 0, 0), Aire), ((1, 0, 0), Aire), ((2, 0, 0), Aire),
             ((3, 0, 0), Placa)],
            M.fromList [((1, 0, 0), CajaMetal), ((2, 0, 0), Caja)]),

        -- La pared separa al segundo jugador de su placa y no puede completar el nivel.
        Sok ([(1, 0, 0), (6, 0, 0)],
            M.union
            (M.fromList [((0, 0, 0), Placa), ((3, 0, 0), Placa),
                         ((4, 0, 0), Placa), ((7, 0, 0), Pared),
                         ((8, 0, 0), Placa)])
            (mapa 9 1 1),
            M.fromList [((2, 0, 0), Caja), ((5, 0, 0), Caja)]),

        -- La puerta se cierra antes de que el jugador superior pueda llegar a la placa final.
        Sok ([(0, 0, 0), (0, 1, 0)],
            M.fromList
            [((0, 0, 0), Aire), ((1, 0, 0), Aire),
             ((2, 0, 0), PlacaActivadoraTemporizada (6, 0, 0)),
             ((3, 0, 0), Aire), ((4, 0, 0), Aire), ((5, 0, 0), Aire),
             ((6, 0, 0), PuertaTemporizada False (-1) 2),
             ((7, 0, 0), Aire), ((8, 0, 0), Placa),
             ((0, 1, 0), Aire), ((1, 1, 0), Aire),
             ((2, 1, 0), PlacaActivadoraTemporizada (6, 0, 0))],
            M.empty)
    ]



--------------------------------------------------
-- Exportación de solución a JSON para visor.html
--------------------------------------------------

casillaJson :: (Posicion, Casilla) -> String
casillaJson ((x, y, z), cas) =
    "{\"pos\":[" ++ show x ++ "," ++ show y ++ "," ++ show z ++ "]," ++ extra cas ++ "}"
    where
        extra (PuertaTemporizada abierta tiempo duracion) =
            "\"tipo\":\"PuertaTemporizada\",\"abierta\":" ++ (if abierta then "true" else "false")
            ++ ",\"tiempo\":" ++ show tiempo ++ ",\"duracion\":" ++ show duracion
        extra (PlacaActivadoraTemporizada _) =
            "\"tipo\":\"PlacaTemporizada\""
        extra c =
            "\"tipo\":\"" ++ show c ++ "\""

objetoJson :: (Posicion, Objeto) -> String
objetoJson ((x, y, z), obj) =
    "{\"pos\":[" ++ show x ++ "," ++ show y ++ "," ++ show z ++ "],\"tipo\":\"" ++ nombre obj ++ "\"}"
    where
        nombre Caja          = "Caja"
        nombre Barril        = "Barril"
        nombre Cuerda        = "Cuerda"
        nombre CajaMetal     = "CajaMetal"
        nombre Globo         = "Globo"
        nombre (Boton _)     = "Boton"
        nombre (Palanca _ b) = if b then "PalancaAbierta" else "PalancaCerrada"

estadoAJson :: Sokoban -> String
estadoAJson (Sok (jugadores, mapaJuego, objetos)) =
    "{"
    ++ "\"jugadores\":[" ++ intercalar "," (map posJson jugadores) ++ "],"
    ++ "\"mapa\":[" ++ intercalar "," (map casillaJson (M.toList mapaJuego)) ++ "],"
    ++ "\"objetos\":[" ++ intercalar "," (map objetoJson (M.toList objetos)) ++ "]"
    ++ "}"
    where
        posJson (x, y, z) = "[" ++ show x ++ "," ++ show y ++ "," ++ show z ++ "]"
        intercalar _ []     = ""
        intercalar _ [x]    = x
        intercalar s (x:xs) = x ++ s ++ intercalar s xs

guardarSolucion :: FilePath -> [Sokoban] -> IO ()
guardarSolucion ruta pasos =
    writeFile ruta ("[\n" ++ unlines' (map estadoAJson pasos) ++ "]\n")
    where
        unlines' []     = ""
        unlines' [x]    = "  " ++ x
        unlines' (x:xs) = "  " ++ x ++ ",\n" ++ unlines' xs

-- Exporta una lista de soluciones (varios niveles) en un único archivo JSON
guardarSoluciones :: FilePath -> [[Sokoban]] -> IO ()
guardarSoluciones ruta listaPasos =
    writeFile ruta ("[\n" ++ intercalar ",\n" (map nivelJson (zip [1..] listaPasos)) ++ "\n]\n")
    where
        nivelJson (i, pasos) =
            "  {\n"
            ++ "    \"nombre\": \"Nivel " ++ show (i :: Int) ++ " (" ++ show (max 0 (length pasos - 1)) ++ " movs)\",\n"
            ++ "    \"pasos\": [\n"
            ++ intercalar ",\n" (map (\p -> "      " ++ estadoAJson p) pasos)
            ++ "\n    ]\n"
            ++ "  }"
        intercalar _ []     = ""
        intercalar _ [x]    = x
        intercalar s (x:xs) = x ++ s ++ intercalar s xs




{- Para compilar

ghci

:set -i.
:l Juegos/Juego_Sokoban.hs Resolutores/TakyonDFS.hs

:r
:m + Resolutores.TakyonDFS Juegos.Juego_Sokoban Data.Maybe
guardarSoluciones "soluciones.json" (mapMaybe resolutor conSolucion)



:r
:m + Resolutores.TakyonDFS Juegos.Juego_Sokoban
let Just camino = resolutor (last conSolucion)
length camino
guardarSolucion "solucion.json" camino


:r
:m + Resolutores.TakyonDFS Juegos.Juego_Sokoban Data.Maybe
guardarSoluciones "soluciones.json" (mapMaybe resolutor sinSolucion)

-}  