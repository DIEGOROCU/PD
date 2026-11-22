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

data Objeto
    = Caja
    | Barril
    | Cuerda
    | CajaMetal
    | Globo
    | Boton Posicion
    | Palanca Posicion Bool
    deriving (Eq, Ord, Show)
-- Nota:
    -- La caja se puede empujar en fila recta (tantas como se pueda)
    -- El barril se puede empujar en fila recta (tantos como se pueda) y tirar de uno en uno
    -- La cuerda se puede empujar y tirar en fila recta y sirve como adhesivo para unir objetos al tirar
    -- La caja de metal solo se puede empujar y únicamente se mueve sola (prioridad sobre adhesivos)
    -- El globo no tiene gravedad, se puede empujar y tirar (también hacia arriba y abajo) y empujar en cadena
    -- El jetpack es un item: al pisarlo el jugador recoge el jetpack y cambia a su rol
    -- El botón se puede activar para abrir una puerta asociada definitivamente (no se puede cerrar)
    -- La palanca se puede activar para abrir o cerrar una puerta asociada (se puede activar varias veces)
    -- Todos los objetos son sólidos (se puede caminar sobre ellos) y tienen gravedad salvo el globo y los mecanismos fijos

-- Un item no tiene la gravedad ni la solidez de un objeto.
data Item = JetpackItem | PicoItem
    deriving (Eq, Ord, Show)

-- Direcciones en el espacio 3D:
--   Eje X: Izquierda (x - 1) / Derecha (x + 1)
--   Eje Y: Atras (y - 1)     / Adelante (y + 1)
--   Eje Z: Abajo (z - 1)     / Arriba (z + 1)
data Direccion = Arriba | Abajo | Izquierda | Derecha | Adelante | Atras
    deriving (Eq, Ord, Enum, Bounded, Show)

type Posicion = (Int, Int, Int)
type JugadorId = Int
type Jugador = (JugadorId, Posicion)
type MapaCasillas = M.Map Posicion Casilla
type MapaObjetos = M.Map Posicion Objeto
type MapaItems = M.Map Posicion Item
type MapaRoles = M.Map JugadorId Rol

-- Define el comportamiento especial que puede tener un jugador.
data Rol = Normal | Jetpack | Minero
    deriving (Eq, Ord, Show)

-- Las acciones las hace el jugador, las reacciones los objetos
data Accion
    = AccionAvanzar Jugador Direccion
    | AccionMoverObjeto Jugador Direccion
    | AccionInteractuarObjeto Jugador Direccion
    | AccionUsarRol Jugador Direccion
    deriving (Eq, Ord, Show)

type Reaccion = (Posicion, Direccion)

-- Registro que encapsula el estado completo del juego, desacoplando sus componentes posicionales.
data Estado = Estado
    { -- Lista de posiciones de los jugadores
      stJugadores :: [Jugador]
      -- Diccionario que describe el mapa del juego
    , stMapa      :: MapaCasillas
      -- Diccionario que describe la posición de los objetos
    , stObjetos   :: MapaObjetos
      -- Rol activo asociado a la posición de cada jugador
    , stRoles     :: MapaRoles
      -- Items recogibles situados en el mapa
    , stItems     :: MapaItems
    }
    deriving (Eq, Ord, Show)

-- Juego
newtype Sokoban = Sok Estado
    deriving (Eq, Ord, Show)

--------------------------------------------------
-- Perfiles de dominio
--------------------------------------------------

-- Agrupa las propiedades físicas de una casilla en un único lugar.
data PerfilCasilla = PerfilCasilla
    { cPisable     :: Bool
    , cSueloSolido :: Bool
    }

-- Agrupa las propiedades físicas y de interacción de un objeto.
data PerfilObjeto = PerfilObjeto
    { oEmpujable       :: Bool
    , oTirable         :: Bool
    , oMovHorizontal   :: Bool
    , oMovVertical     :: Bool
    , oAdhesivo        :: Bool
    , oSoloSeMueveSola :: Bool
    , oTieneGravedad   :: Bool
    , oInteractuar     :: Estado -> Posicion -> Maybe Estado
    }

-- Agrupa las capacidades y reglas físicas asociadas a un rol sin acoplarlas al motor.
data PerfilRol = PerfilRol
    { pAfectadoPorGravedad :: Bool
    , pPuedePasarCasilla   :: Casilla -> Bool
    , pPuedeEmpujar        :: Objeto -> Bool
    , pPuedeTirar          :: Objeto -> Bool
    , pIgnoraPesoMetal     :: Bool
    , pHabilidadEspecial   :: Estado -> Jugador -> Direccion -> Maybe Estado
    }

--------------------------------------------------
-- Instancia de Jugable
--------------------------------------------------

instance Jugable Sokoban where

    -- Función esExito: se gana cuando todas las placas están ocupadas por un jugador o un objeto
    esExito (Sok estado) =
        all estaPisada placas
        where
            placas       = listaPosicionesCasilla estado Placa
            ocupadas     = posicionesJugadores (stJugadores estado) ++ M.keys (stObjetos estado)
            estaPisada p = p `elem` ocupadas

    opciones (Sok estado)
            | estadoEstable /= estado = [Sok estadoEstable]
            | otherwise =
                [ Sok nuevaPosicion
                | jugador <- stJugadores estado
                , direccion <- [minBound .. maxBound]
                , accion <- [AccionAvanzar jugador direccion,
                            AccionMoverObjeto jugador direccion,
                            AccionInteractuarObjeto jugador direccion,
                            AccionUsarRol jugador direccion]
                , Just nuevaPosicion <- [ejecutarAccion estado accion]
                ]
            where
                estadoEstable = aplicarGravedadPaso estado

--------------------------------------------------
-- Funciones Auxiliares simples
--------------------------------------------------

-- Listas de Casillas y Objetos
listaPosicionesCasilla :: Estado -> Casilla -> [Posicion]
listaPosicionesCasilla estado casilla =
    M.keys $ M.filter (== casilla) (stMapa estado)

listaPosicionesObjeto :: Estado -> Objeto -> [Posicion]
listaPosicionesObjeto estado objeto =
    M.keys $ M.filter (== objeto) (stObjetos estado)

-- Suma de una posición 3D y una dirección
sumaDireccion :: Posicion -> Direccion -> Posicion
sumaDireccion (x, y, z) dir = case dir of
    Derecha   -> (x + 1, y, z)
    Izquierda -> (x - 1, y, z)
    Adelante  -> (x, y + 1, z)
    Atras     -> (x, y - 1, z)
    Arriba    -> (x, y, z + 1)
    Abajo     -> (x, y, z - 1)

-- Obtiene la identificación y la posición de un jugador.
identificadorJugador :: Jugador -> JugadorId
identificadorJugador (identificador, _) = identificador

posicionJugador :: Jugador -> Posicion
posicionJugador (_, posicion) = posicion

posicionesJugadores :: [Jugador] -> [Posicion]
posicionesJugadores = map posicionJugador

esDireccionVertical :: Direccion -> Bool
esDireccionVertical Arriba = True
esDireccionVertical Abajo  = True
esDireccionVertical _      = False

esDireccionHorizontal :: Direccion -> Bool
esDireccionHorizontal = not . esDireccionVertical

--------------------------------------------------
-- Consultas espaciales y de estado
--------------------------------------------------

-- Devuelve la casilla en una posición (Limite si está fuera del mapa).
casillaEn :: Estado -> Posicion -> Casilla
casillaEn estado pos =
    M.findWithDefault Limite pos (stMapa estado)

-- Indica si una posición está ocupada por algún objeto sólido o por algún jugador.
estaOcupada :: Estado -> Posicion -> Bool
estaOcupada estado pos =
    M.member pos (stObjetos estado) || pos `elem` posicionesJugadores (stJugadores estado)

-- Indica si un jugador con un determinado perfil puede entrar en una posición libre.
esTransitablePara :: PerfilRol -> Estado -> Posicion -> Bool
esTransitablePara perfil estado pos =
    pPuedePasarCasilla perfil (casillaEn estado pos) && not (estaOcupada estado pos)

-- Devuelve el perfil activo del jugador indicado.
perfilDeJugador :: Estado -> Jugador -> PerfilRol
perfilDeJugador estado jugador =
    perfilDe (rolJugador (stRoles estado) jugador)

-- Actualiza la posición de un jugador dentro del estado.
actualizarPosicionJugador :: Estado -> Jugador -> Posicion -> Estado
actualizarPosicionJugador estado origen destino =
    estado { stJugadores = moverJugador (stJugadores estado) origen destino }

--------------------------------------------------
-- Definición de Perfiles
--------------------------------------------------

-- Proyecta cada tipo de casilla a sus propiedades físicas.
perfilCasilla :: Casilla -> PerfilCasilla
perfilCasilla Pared                          = PerfilCasilla False True
perfilCasilla Limite                         = PerfilCasilla False True
perfilCasilla PuertaBoton                    = PerfilCasilla False True
perfilCasilla PuertaPalanca                  = PerfilCasilla False True
perfilCasilla Escalera                       = PerfilCasilla False True
perfilCasilla (PuertaTemporizada abierta _ _) = PerfilCasilla abierta (not abierta)
perfilCasilla _                              = PerfilCasilla True False

-- Verifica si una casilla es pisable/atravesable
esPisable :: Casilla -> Bool
esPisable = cPisable . perfilCasilla

-- Verifica si una casilla actúa como suelo sólido que soporta peso encima
esSueloSolido :: Casilla -> Bool
esSueloSolido = cSueloSolido . perfilCasilla

-- Perfil base por defecto para objetos inertes.
perfilObjBase :: PerfilObjeto
perfilObjBase = PerfilObjeto
    { oEmpujable       = False
    , oTirable         = False
    , oMovHorizontal   = False
    , oMovVertical     = False
    , oAdhesivo        = False
    , oSoloSeMueveSola = False
    , oTieneGravedad   = False
    , oInteractuar     = \_ _ -> Nothing
    }

-- Proyecta cada objeto a su conjunto de propiedades físicas e interacciones.
perfilObjeto :: Objeto -> PerfilObjeto
perfilObjeto Caja = perfilObjBase
    { oEmpujable     = True
    , oMovHorizontal = True
    , oTieneGravedad = True
    }
perfilObjeto Barril = perfilObjBase
    { oEmpujable     = True
    , oTirable       = True
    , oMovHorizontal = True
    , oTieneGravedad = True
    }
perfilObjeto Cuerda = perfilObjBase
    { oEmpujable     = True
    , oTirable       = True
    , oMovHorizontal = True
    , oAdhesivo      = True
    , oTieneGravedad = True
    }
perfilObjeto CajaMetal = perfilObjBase
    { oEmpujable       = True
    , oMovHorizontal   = True
    , oSoloSeMueveSola = True
    , oTieneGravedad   = True
    }
perfilObjeto Globo = perfilObjBase
    { oEmpujable     = True
    , oTirable       = True
    , oMovHorizontal = True
    , oMovVertical   = True
    }
perfilObjeto (Boton posPuerta) = perfilObjBase
    { oInteractuar = interactuarBoton posPuerta
    }
perfilObjeto (Palanca posPuerta activada) = perfilObjBase
    { oInteractuar = interactuarPalanca posPuerta activada
    }

-- Predicados delegados al perfil del objeto para mantener compatibilidad y legibilidad.
esEmpujable :: Objeto -> Bool
esEmpujable = oEmpujable . perfilObjeto

esTirable :: Objeto -> Bool
esTirable = oTirable . perfilObjeto

esMovibleHorizontalmente :: Objeto -> Bool
esMovibleHorizontalmente = oMovHorizontal . perfilObjeto

esMovibleVerticalmente :: Objeto -> Bool
esMovibleVerticalmente = oMovVertical . perfilObjeto

esAdhesivo :: Objeto -> Bool
esAdhesivo = oAdhesivo . perfilObjeto

soloSeMueveSola :: Objeto -> Bool
soloSeMueveSola = oSoloSeMueveSola . perfilObjeto

tieneGravedad :: Objeto -> Bool
tieneGravedad = oTieneGravedad . perfilObjeto

-- Obtiene el rol activo de un jugador; la ausencia equivale al rol normal.
rolJugador :: MapaRoles -> Jugador -> Rol
rolJugador roles jugador =
    M.findWithDefault Normal (identificadorJugador jugador) roles

-- Asocia cada item recogible con el rol que otorga al jugador.
rolDeItem :: Item -> Rol
rolDeItem JetpackItem = Jetpack
rolDeItem PicoItem    = Minero

-- Perfil por defecto correspondiente al comportamiento estándar del jugador.
perfilBase :: PerfilRol
perfilBase = PerfilRol
    { pAfectadoPorGravedad = True
    , pPuedePasarCasilla   = esPisable
    , pPuedeEmpujar        = esEmpujable
    , pPuedeTirar          = esTirable
    , pIgnoraPesoMetal     = False
    , pHabilidadEspecial   = \_ _ _ -> Nothing
    }

-- Proyecta cada rol puro a su conjunto de capacidades.
perfilDe :: Rol -> PerfilRol
perfilDe Normal = perfilBase
perfilDe Jetpack = perfilBase
    { pAfectadoPorGravedad = False
    , pHabilidadEspecial   = habilidadJetpack
    }
perfilDe Minero = perfilBase
    { pHabilidadEspecial   = habilidadMinero
    }

--------------------------------------------------
-- Habilidades de Roles e Interacciones de Objetos
--------------------------------------------------

-- Habilidad especial del jetpack: permite desplazarse verticalmente en Z y mover globos en vertical.
habilidadJetpack :: Estado -> Jugador -> Direccion -> Maybe Estado
habilidadJetpack estado posJugador direccion
    | not (esDireccionVertical direccion) = Nothing
    | otherwise = case M.lookup destino (stObjetos estado) of
        Nothing ->
            if esTransitablePara perfil estado destino
            then Just (actualizarPosicionJugador estado posJugador destino)
            else Nothing
        Just objeto
            | esMovibleVerticalmente objeto ->
                if esTransitablePara perfil estado destinoObjeto
                then Just (actualizarPosicionJugador estado posJugador destino)
                    { stObjetos = M.insert destinoObjeto objeto (M.delete destino (stObjetos estado)) }
                else Nothing
        _ -> Nothing
    where
        perfil = perfilDeJugador estado posJugador
        destino = sumaDireccion (posicionJugador posJugador) direccion
        destinoObjeto = sumaDireccion destino direccion

-- Habilidad especial del minero: rompe una casilla de tipo Pared adyacente y la convierte en Aire.
-- Al comprobar específicamente `Pared`, respeta los bordes del mapa (`Limite`) y las puertas.
habilidadMinero :: Estado -> Jugador -> Direccion -> Maybe Estado
habilidadMinero estado posJugador direccion
    | casillaEn estado destino == Pared && not (estaOcupada estado destino) =
        Just estado { stMapa = M.insert destino Aire (stMapa estado) }
    | otherwise = Nothing
    where
        destino = sumaDireccion (posicionJugador posJugador) direccion

-- Lógica de activación irreversible de un botón sobre su puerta asociada.
interactuarBoton :: Posicion -> Estado -> Posicion -> Maybe Estado
interactuarBoton posPuerta estado _
    | casillaEn estado posPuerta == PuertaBoton =
        Just estado { stMapa = M.insert posPuerta Aire (stMapa estado) }
    | otherwise = Nothing

-- Lógica de conmutación reversible de una palanca sobre su puerta asociada.
interactuarPalanca :: Posicion -> Bool -> Estado -> Posicion -> Maybe Estado
interactuarPalanca posPuerta activada estado posObjeto
    | casillaEn estado posPuerta == puertaEsperada
      && (not activada || not (estaOcupada estado posPuerta)) =
        let nuevaActivacion = not activada
            nuevaPuerta = if nuevaActivacion then Aire else PuertaPalanca
            nuevosObjetos = M.insert posObjeto (Palanca posPuerta nuevaActivacion) (stObjetos estado)
        in Just estado
            { stMapa    = M.insert posPuerta nuevaPuerta (stMapa estado)
            , stObjetos = nuevosObjetos
            }
    | otherwise = Nothing
    where
        puertaEsperada = if activada then Aire else PuertaPalanca

--------------------------------------------------
-- Metodos auxiliares complejos
--------------------------------------------------

-- Una cadena es válida si todos sus objetos son empujables según el perfil,
-- y no contiene ningún objeto que solo pueda moverse solo dentro de una cadena de varios elementos.
cadenaEmpujable :: PerfilRol -> [Objeto] -> Bool
cadenaEmpujable _ [] = False
cadenaEmpujable perfil objetos =
    all (pPuedeEmpujar perfil) objetos &&
    (length objetos == 1 || pIgnoraPesoMetal perfil || not (any soloSeMueveSola objetos))

-- Obtiene los objetos consecutivos que hay delante del jugador.
cadenaObjetos :: MapaObjetos -> Posicion -> Direccion -> [ (Posicion, Objeto) ]
cadenaObjetos objetos posicion direccion =
    case M.lookup posicion objetos of
        Nothing -> []
        Just objeto -> (posicion, objeto) :
            cadenaObjetos objetos (sumaDireccion posicion direccion) direccion

-- Construye la cadena de objetos que se arrastran al tirar:
-- el primer objeto debe ser tirable según el perfil y solo se encadenan los siguientes si hay una cuerda adhesiva uniendo ambos.
cadenaTirar :: PerfilRol -> MapaObjetos -> Posicion -> Direccion -> [ (Posicion, Objeto) ]
cadenaTirar perfil objetos posInicial dirTirar =
    case M.lookup posInicial objetos of
        Nothing -> []
        Just primero
            | not (pPuedeTirar perfil primero) -> []
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
empujePosible :: PerfilRol -> Estado -> Posicion -> Direccion -> Bool
empujePosible perfil estado posicion direccion =
    not (null cadena) &&
    cadenaEmpujable perfil objetosCadena &&
    (esDireccionHorizontal direccion || all esMovibleVerticalmente objetosCadena) &&
    esTransitablePara perfil estado destino
    where
        cadena = cadenaObjetos (stObjetos estado) posicion direccion
        objetosCadena = map snd cadena
        destino = sumaDireccion (fst (last cadena)) direccion

-- Desplaza todos los objetos de una cadena, empezando por el último.
empujar :: MapaObjetos -> Posicion -> Direccion -> MapaObjetos
empujar objetos posicion direccion =
    foldr (desplazarObjeto direccion) objetos
        (cadenaObjetos objetos posicion direccion)

-- Comprueba si una cadena de objetos puede ser tirada en una dirección en 3D.
tirarPosible :: PerfilRol -> Estado -> Posicion -> Direccion -> Bool
tirarPosible perfil estado posicion direccion =
    not (null cadena) &&
    (esDireccionHorizontal direccion && all esMovibleHorizontalmente objetosCadena ||
     esDireccionVertical direccion && all esMovibleVerticalmente objetosCadena) &&
    (length objetosCadena == 1 || pIgnoraPesoMetal perfil || not (any soloSeMueveSola objetosCadena)) &&
    pPuedePasarCasilla perfil (casillaEn estado destino) &&
    not (M.member destino (stObjetos estado))
    where
        cadena = cadenaTirar perfil (stObjetos estado) posicion direccion
        objetosCadena = map snd cadena
        destino = sumaDireccion posicion direccion

-- Desplaza hacia el jugador todos los objetos enganchados en la cadena de tirón, empezando por el más cercano.
tirar :: PerfilRol -> MapaObjetos -> Posicion -> Direccion -> MapaObjetos
tirar perfil objetos posicion direccion =
    foldl (flip (desplazarObjeto direccion)) objetos
        (cadenaTirar perfil objetos posicion direccion)

-- Comprueba si una posición tiene una escalera adyacente en el plano horizontal (X o Y).
estaAgarradoAEscalera :: Estado -> Posicion -> Bool
estaAgarradoAEscalera estado pos =
    any (\dir -> casillaEn estado (sumaDireccion pos dir) == Escalera)
        [Derecha, Izquierda, Adelante, Atras]

-- Sube un único bloque por turno: si la escalera continúa arriba, sube 1 en Z junto a ella;
-- si es el último tramo, corona colocándose encima.
subirPorEscalera :: PerfilRol -> Estado -> Posicion -> Posicion -> Maybe Posicion
subirPorEscalera perfil estado posJugador posEscalera =
    let arribaJugador  = sumaDireccion posJugador Arriba
        arribaEscalera = sumaDireccion posEscalera Arriba
    in if not (esTransitablePara perfil estado arribaJugador)
       then Nothing
       else case casillaEn estado arribaEscalera of
           Escalera -> Just arribaJugador
           _        -> if esTransitablePara perfil estado arribaEscalera then Just arribaEscalera else Nothing

-- Comprueba si una posición tiene apoyo real hasta tocar suelo sólido, escalera o un objeto sin gravedad (ej. Globo)
tieneApoyo :: Estado -> Posicion -> Bool
tieneApoyo estado pos
    | esSueloSolido (casillaEn estado debajo) = True
    | pos `elem` posicionesJugadores (stJugadores estado) && estaAgarradoAEscalera estado pos = True
    | Just objDebajo <- M.lookup debajo (stObjetos estado) =
        not (tieneGravedad objDebajo) || tieneApoyo estado debajo
    | debajo `elem` posicionesJugadores (stJugadores estado) =
        tieneApoyo estado debajo
    | otherwise = False
    where
        debajo = sumaDireccion pos Abajo

-- Aplica un único paso de gravedad (1 casilla en -Z) a los jugadores y objetos sin apoyo.
aplicarGravedadPaso :: Estado -> Estado
aplicarGravedadPaso estado =
    estado
        { stJugadores = nuevosJugadores
        , stObjetos   = nuevosObjetos
        }
    where
        caerJugador j
            | not (pAfectadoPorGravedad (perfilDeJugador estado j)) = j
            | tieneApoyo estado pos = j
            | otherwise = (identificadorJugador j, sumaDireccion pos Abajo)
            where pos = posicionJugador j
        nuevosJugadores = map caerJugador (stJugadores estado)

        nuevosObjetos = M.foldrWithKey moverSiCae M.empty (stObjetos estado)
        moverSiCae pos obj acc
            | tieneGravedad obj && not (tieneApoyo estado pos) =
                M.insert (sumaDireccion pos Abajo) obj acc
            | otherwise =
                M.insert pos obj acc

-- Actualiza el estado de las puertas temporizadas después de cada acción.
actualizarPuertas :: Estado -> Estado
actualizarPuertas estado =
    estado { stMapa = M.mapWithKey actualizar (stMapa estado) }
    where
        ocupadas = posicionesJugadores (stJugadores estado) ++ M.keys (stObjetos estado)

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
                         | (posicionPlaca, PlacaActivadoraTemporizada puerta) <- M.toList (stMapa estado)
                         , puerta == posicion
                         ]

-- Aplica las actualizaciones comunes después de una acción válida.
-- Primero cae cualquier elemento afectado por gravedad y después se actualizan las puertas temporizadas.
actualizacionFinal :: Estado -> Estado
actualizacionFinal =
    actualizarPuertas . aplicarGravedadPaso

-- Ejecuta directamente la acción correspondiente y aplica la actualización final.
ejecutarAccion :: Estado -> Accion -> Maybe Estado
ejecutarAccion estado accion =
    fmap actualizacionFinal $ case accion of
        AccionAvanzar posJugador direccion           -> accionAvanzar estado posJugador direccion
        AccionMoverObjeto posJugador direccion       -> accionMoverObjeto estado posJugador direccion
        AccionInteractuarObjeto posJugador direccion -> accionInteractuarObjeto estado posJugador direccion
        AccionUsarRol posJugador direccion           -> accionUsarRol estado posJugador direccion

accionAvanzar :: Estado -> Jugador -> Direccion -> Maybe Estado
accionAvanzar estado posJugador direccion
    -- El jugador debe existir en el estado.
    | posJugador `notElem` stJugadores estado =
        Nothing
    -- Si está agarrado a una escalera, puede bajar 1 peldaño en Z con Abajo.
    | direccion == Abajo && estaAgarradoAEscalera estado posActual =
        if esTransitablePara perfil estado posSiguiente
        then Just (actualizarPosicionJugador estado posJugador posSiguiente)
        else Nothing
    -- El resto de movimientos verticales directos (Arriba, o Abajo sin escalera) no están permitidos.
    | esDireccionVertical direccion =
        Nothing
    -- Si hay una escalera delante, se intenta subir 1 peldaño por ella.
    | casillaEn estado posSiguiente == Escalera =
        fmap (actualizarPosicionJugador estado posJugador)
            (subirPorEscalera perfil estado posActual posSiguiente)
    -- No se puede avanzar hacia una casilla sólida ni ocupada según el perfil del rol.
    | not (esTransitablePara perfil estado posSiguiente) =
        Nothing
    -- Al pisar un item recogible, el jugador lo recoge y adopta su rol asociado.
    | Just item <- M.lookup posSiguiente (stItems estado) =
        Just (actualizarPosicionJugador estado posJugador posSiguiente)
            { stRoles = M.insert (identificadorJugador posJugador) (rolDeItem item) (stRoles estado)
            , stItems = M.delete posSiguiente (stItems estado)
            }
    -- Si ninguna restricción lo impide, el jugador avanza.
    | otherwise =
        Just (actualizarPosicionJugador estado posJugador posSiguiente)
    where
        perfil = perfilDeJugador estado posJugador
        posActual = posicionJugador posJugador
        posSiguiente = sumaDireccion posActual direccion

-- Mueve un objeto empujándolo o tirando de él.
accionMoverObjeto :: Estado -> Jugador -> Direccion -> Maybe Estado
accionMoverObjeto estado posJugador direccion
    -- El jugador debe existir en el estado.
    | posJugador `notElem` stJugadores estado =
        Nothing
    -- No se puede mover hacia una casilla sólida ni ocupada por otro jugador según el perfil del rol.
    | not (pPuedePasarCasilla perfil (casillaEn estado posSiguiente)) =
        Nothing
    | posSiguiente `elem` posicionesJugadores (stJugadores estado) =
        Nothing
    -- Si hay un objeto delante, se intenta empujarlo.
    | M.member posSiguiente (stObjetos estado) && empujePosible perfil estado posSiguiente direccion =
        Just (actualizarPosicionJugador estado posJugador posSiguiente)
            { stObjetos = empujar (stObjetos estado) posSiguiente direccion }
    -- Si hay un objeto detrás, se intenta tirar de él.
    | not (M.member posSiguiente (stObjetos estado))
      && M.member posAnterior (stObjetos estado)
      && tirarPosible perfil estado posAnterior direccion =
        Just (actualizarPosicionJugador estado posJugador posSiguiente)
            { stObjetos = tirar perfil (stObjetos estado) posAnterior direccion }
    -- Si no se puede empujar ni tirar, la acción falla.
    | otherwise =
        Nothing
    where
        perfil = perfilDeJugador estado posJugador
        posActual = posicionJugador posJugador
        posSiguiente = sumaDireccion posActual direccion
        posAnterior = sumaDireccion posActual (direccionContraria direccion)

-- Interactúa con un botón o una palanca situados delante del jugador delegando en su perfil.
accionInteractuarObjeto :: Estado -> Jugador -> Direccion -> Maybe Estado
accionInteractuarObjeto estado posJugador direccion
    -- El jugador debe existir en el estado.
    | posJugador `notElem` stJugadores estado =
        Nothing
    -- Se comprueba el mecanismo situado delante del jugador.
    | otherwise =
        case M.lookup posObjeto (stObjetos estado) of
            Just objeto -> oInteractuar (perfilObjeto objeto) estado posObjeto
            Nothing     -> Nothing
    where
        posObjeto = sumaDireccion (posicionJugador posJugador) direccion

-- Usa la habilidad especial definida en el perfil del rol activo del jugador.
accionUsarRol :: Estado -> Jugador -> Direccion -> Maybe Estado
accionUsarRol estado posJugador direccion
    | posJugador `notElem` stJugadores estado =
        Nothing
    | otherwise =
        pHabilidadEspecial (perfilDeJugador estado posJugador) estado posJugador direccion

-- Indica si el jugador puede realizar la acción.
accionPosible :: Estado -> Accion -> Bool
accionPosible estado accion =
    case ejecutarAccion estado accion of
        Nothing -> False
        Just _ -> True

moverJugador :: [Jugador] -> Jugador -> Posicion -> [Jugador]
moverJugador jugadores origen destino =
    map mover jugadores
    where
        mover jugador
            | identificadorJugador jugador == identificadorJugador origen =
                (identificadorJugador jugador, destino)
            | otherwise =
                jugador

moverObjeto :: MapaObjetos -> Posicion -> Posicion -> MapaObjetos
moverObjeto objetos origen destino =
    case M.lookup origen objetos of
        Nothing -> objetos
        Just objeto -> M.insert destino objeto (M.delete origen objetos)

desplazarObjeto :: Direccion -> (Posicion, Objeto) -> MapaObjetos -> MapaObjetos
desplazarObjeto direccion (posicionObjeto, objeto) objetos =
    moverObjeto objetos posicionObjeto
        (sumaDireccion posicionObjeto direccion)

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

-- Crea un jugador con una ID única y una posición inicial.
jugadorConId :: JugadorId -> Posicion -> Jugador
jugadorConId identificador posicion =
    (identificador, posicion)

-- Constructor auxiliar para instanciar niveles de Sokoban con el registro Estado.
crearNivel :: [Jugador] -> MapaCasillas -> MapaObjetos -> MapaRoles -> MapaItems -> Sokoban
crearNivel js mc mo mr mi = Sok (Estado js mc mo mr mi)

-- Ejemplos de Sokoban 3D con solución
conSolucion :: [Sokoban]
conSolucion =
    [
    crearNivel [jugadorConId 1 (0, 0, 0)]
        (M.unions
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
            ])
        (M.fromList
            [ ((1, 0, 0), CajaMetal)
            , ((3, 1, 0), Palanca (5, 2, 0) False)
            , ((7, 2, 1), Globo)
            , ((1, 1, 2), Caja)
            , ((2, 1, 2), Cuerda)
            , ((3, 1, 2), Barril)
            , ((5, 0, 2), Boton (8, 2, 4))
            , ((8, 0, 4), Barril)
            ])
        M.empty
        M.empty,

    -- La fila de barril, cuerda, barril y cuerda se puede tirar desde la segunda columna.
    crearNivel [jugadorConId 1 (1, 1, 0)]
        (M.union
        (M.fromList [((1, 0, 0), Placa), ((1, 1, 0), Placa)])
        (mapa 6 2 1))
        (M.fromList [((2, 1, 0), Barril), ((3, 1, 0), Cuerda),
                    ((4, 1, 0), Barril), ((5, 1, 0), Cuerda)])
        M.empty
        M.empty,

    -- Los dos jugadores empujan sus cajas y luego pisan sus respectivas placas.
    crearNivel [jugadorConId 1 (1, 0, 0), jugadorConId 2 (6, 0, 0)]
        (M.union
        (M.fromList [((0, 0, 0), Placa), ((3, 0, 0), Placa),
                        ((4, 0, 0), Placa), ((7, 0, 0), Placa)])
        (mapa 8 1 1))
        (M.fromList [((2, 0, 0), Caja), ((5, 0, 0), Caja)])
        M.empty
        M.empty,

    crearNivel [jugadorConId 1 (4, 3, 0)]
        (M.union
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
            (mapa 6 4 4))
        (M.fromList
            [ ((0, 0, 0), Barril)
            , ((5, 0, 0), Globo)
            ])
        M.empty
        M.empty,
            
    -- El jugador tiene que usar la cuerda para sacar la caja de la esquina y empujarla hasta la placa.
    crearNivel [jugadorConId 1 (4, 3, 0)]
        (M.union
            (M.fromList
              [ ((0, 0, 0), Placa)
              , ((2, 0, 0), Placa)
              , ((4, 0, 0), Placa)
              ])
            (mapa 5 4 1))
        (M.fromList
            [ ((0, 3, 0), Caja)
            , ((3, 0, 0), Cuerda)
            ])
        M.empty
        M.empty,

    -- El jugador tiene que usar dos globos para subir a la placa.
    crearNivel [jugadorConId 1 (1, 1, 0)]
        (M.union
            (M.fromList
              [ ((0, 0, 0), Escalera)
              , ((0, 1, 5), Pared)
              , ((0, 1, 6), Placa)
              ])
            (mapa 2 2 7))
        (M.fromList
            [ ((1, 0, 0), Globo)
            , ((1, 1, 2), Globo)
            ])
        M.empty
        M.empty,

    crearNivel [jugadorConId 1 (5, 2, 0)]
        (M.union
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
            (mapa 6 6 7))
        (M.fromList
            [ ((1, 5, 0), Globo)
            , ((4, 0, 0), Globo)
            , ((5, 4, 0), Globo)
            ])
        M.empty
        M.empty,

    -- El jugador recoge el jetpack y usa su rol para subir hasta la placa.
    crearNivel [jugadorConId 1 (2, 3, 0)]
        (M.union
            (M.fromList
              [ ((0, 3, 1), Pared)
              , ((0, 3, 2), Placa)
              ])
            (mapa 3 4 3))
        M.empty
        M.empty
        (M.fromList
            [ ((0, 0, 0), JetpackItem)
            ]),

    -- El jugador recoge el pico y rompe la pared para llegar a la placa.
    crearNivel [jugadorConId 1 (0, 3, 0)]
        (M.union
            (M.fromList
              [ ((0, 0, 1), Pared)
              , ((0, 0, 2), Placa)
              , ((0, 1, 1), Pared)
              , ((0, 2, 1), Pared)
              , ((0, 3, 1), Pared)
              , ((1, 0, 0), Pared)
              , ((1, 0, 1), Pared)
              , ((1, 0, 2), Placa)
              , ((1, 1, 0), Pared)
              , ((1, 1, 1), Pared)
              , ((1, 2, 0), Pared)
              , ((1, 2, 1), Pared)
              , ((1, 3, 0), Pared)
              , ((1, 3, 1), Pared)
              , ((2, 3, 0), Escalera)
              , ((2, 3, 1), Escalera)
              ])
            (mapa 3 4 3))
        (M.fromList
            [ ((0, 3, 2), Barril)
            ])
        M.empty
        (M.fromList
            [ ((0, 0, 0), PicoItem)
            ])

        
    ]

-- Ejemplos sencillos de Sokoban 3D sin solución.
sinSolucion :: [Sokoban]
sinSolucion =
    [
        -- La placa está aislada por una pared.
        crearNivel [jugadorConId 1 (0, 0, 0)] (M.fromList [((0, 0, 0), Aire), ((2, 0, 0), Placa)]) M.empty M.empty M.empty,

        -- La caja está contra una pared y no puede llegar a la placa.
        crearNivel [jugadorConId 1 (0, 0, 0)]
            (M.fromList
            [((0, 0, 0), Aire), ((1, 0, 0), Aire), ((2, 0, 0), Pared),
             ((3, 0, 0), Placa)])
            (M.fromList [((1, 0, 0), Caja)])
            M.empty
            M.empty,

        -- La puerta está cerrada y no hay ningún botón que pueda abrirla.
        crearNivel [jugadorConId 1 (0, 0, 0)]
            (M.fromList
            [((0, 0, 0), Aire), ((1, 0, 0), PuertaBoton), ((2, 0, 0), Placa)])
            M.empty
            M.empty
            M.empty,

        -- La caja de metal no puede empujar la caja que tiene delante hasta la placa.
        crearNivel [jugadorConId 1 (0, 0, 0)]
            (M.fromList
            [((0, 0, 0), Aire), ((1, 0, 0), Aire), ((2, 0, 0), Aire),
             ((3, 0, 0), Placa)])
            (M.fromList [((1, 0, 0), CajaMetal), ((2, 0, 0), Caja)])
            M.empty
            M.empty,

        -- La pared separa al segundo jugador de su placa y no puede completar el nivel.
        crearNivel [jugadorConId 1 (1, 0, 0), jugadorConId 2 (6, 0, 0)]
            (M.union
            (M.fromList [((0, 0, 0), Placa), ((3, 0, 0), Placa),
                         ((4, 0, 0), Placa), ((7, 0, 0), Pared),
                         ((8, 0, 0), Placa)])
            (mapa 9 1 1))
            (M.fromList [((2, 0, 0), Caja), ((5, 0, 0), Caja)])
            M.empty
            M.empty,

        -- La puerta se cierra antes de que el jugador superior pueda llegar a la placa final.
        crearNivel [jugadorConId 1 (0, 0, 0), jugadorConId 2 (0, 1, 0)]
            (M.fromList
            [((0, 0, 0), Aire), ((1, 0, 0), Aire),
             ((2, 0, 0), PlacaActivadoraTemporizada (6, 0, 0)),
             ((3, 0, 0), Aire), ((4, 0, 0), Aire), ((5, 0, 0), Aire),
             ((6, 0, 0), PuertaTemporizada False (-1) 2),
             ((7, 0, 0), Aire), ((8, 0, 0), Placa),
             ((0, 1, 0), Aire), ((1, 1, 0), Aire),
             ((2, 1, 0), PlacaActivadoraTemporizada (6, 0, 0))])
             M.empty
             M.empty
             M.empty,

        -- Sin jetpack, el jugador normal no puede volar hasta la placa situada dos niveles mas arriba.
        crearNivel [jugadorConId 1 (0, 0, 0)]
             (M.union
             (M.fromList [((1, 0, 2), Placa)])
             (mapa 3 1 3))
             M.empty
             M.empty
             M.empty
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

itemJson :: (Posicion, Item) -> String
itemJson ((x, y, z), JetpackItem) =
    "{\"pos\":[" ++ show x ++ "," ++ show y ++ "," ++ show z ++ "],\"tipo\":\"Jetpack\"}"
itemJson ((x, y, z), PicoItem) =
    "{\"pos\":[" ++ show x ++ "," ++ show y ++ "," ++ show z ++ "],\"tipo\":\"Pico\"}"

estadoAJson :: Sokoban -> String
estadoAJson (Sok estado) =
    "{"
    ++ "\"jugadores\":[" ++ intercalar "," (map jugadorJson (stJugadores estado)) ++ "],"
    ++ "\"mapa\":[" ++ intercalar "," (map casillaJson (M.toList (stMapa estado))) ++ "],"
    ++ "\"objetos\":[" ++ intercalar "," (map objetoJson (M.toList (stObjetos estado))) ++ "],"
    ++ "\"items\":[" ++ intercalar "," (map itemJson (M.toList (stItems estado))) ++ "],"
    ++ "\"roles\":[" ++ intercalar "," (map rolJson (M.toList (stRoles estado))) ++ "]"
    ++ "}"
    where
        posJson (x, y, z) = "[" ++ show x ++ "," ++ show y ++ "," ++ show z ++ "]"
        jugadorJson (identificador, posicion) =
            "{\"id\":" ++ show identificador ++ ",\"pos\":" ++ posJson posicion ++ "}"
        rolJson (identificador, rol) =
            "{\"id\":" ++ show identificador ++ ",\"rol\":\"" ++ show rol ++ "\"}"
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




python3 -m http.server 8000


-}