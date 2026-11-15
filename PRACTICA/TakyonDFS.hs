{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -O2 -funbox-strict-fields -fspec-constr -fdicts-strict #-}
module Resolutores.TakyonDFS (resolutor) where

import Jugable
import qualified Data.Set as S

{-|
  Resolutor: DFS "Takyon"
  Autores: Pau Frangi y Diego Rodríguez

  Hemos implementado un DFS con las siguientes mejoras para aumtentar la velocidad y reducir el consumo de memoria:
  - Uso directo de Data.Set con Ord (antes convertíamos estados con 'show' pero eso era más lento y consumía más memoria).
  - En lugar de llamar a 'member' y luego a 'insert' (recorriendo el árbol dos veces), 
     insertamos directamente y comprobamos si ha cambiado 'S.size'.
  - 'Pila' guarda solo la rama activa y los hermanos por explorar. Así no copiamos listas de caminos en cada paso y solo
     reconstruimos la solución una vez al llegar a la meta.
  - El uso de '!' (BangPatterns) evita acumular cálculos pendientes en memoria, y los flags de GHC (-O2,
     -funbox-strict-fields, -fdicts-strict e INLINABLE) desempaquetan los campos
     de la pila y generan código especializado para cada juego.
-}

-- Pila estricta para retroceder: guarda el nodo padre, sus hijos pendientes y el resto de la rama.
data Pila pos = Fin | Rama !pos ![pos] !(Pila pos)

resolutor :: Jugable pos => Resolutor pos
resolutor inicio
    | esExito inicio = Just [inicio]
    | otherwise      = expandir inicio Fin (S.singleton inicio)
  where
    -- Genera los hijos del estado actual, mira si alguno gana y baja por el primero válido.
    expandir :: Jugable pos => pos -> Pila pos -> S.Set pos -> Maybe [pos]
    expandir !actual !pila !vis =
        case procesar (opciones actual) vis [] of
            Left ganador             -> Just (reconstruir ganador (Rama actual [] pila))
            Right (!vis', [])        -> retroceder pila vis'
            Right (!vis', sig:resto) ->
                expandir sig (Rama actual resto pila) vis'

    -- Vuelve hacia atrás en la pila hasta encontrar un hermano que falte por explorar.
    retroceder :: Jugable pos => Pila pos -> S.Set pos -> Maybe [pos]
    retroceder Fin !_ = Nothing
    retroceder (Rama _ [] anc) !vis = retroceder anc vis
    retroceder (Rama padre (sig:resto) anc) !vis =
        expandir sig (Rama padre resto anc) vis

    -- Recorre la pila desde la meta hasta el inicio para crear la lista solución en orden.
    reconstruir :: pos -> Pila pos -> [pos]
    reconstruir !ganador = go [ganador]
      where
        go !acc Fin = acc
        go !acc (Rama p _ anc) = go (p : acc) anc

    -- Comprueba si algún hijo es meta y filtra los ya visitados en un solo acceso al Set.
    procesar :: Jugable pos
             => [pos]
             -> S.Set pos
             -> [pos]
             -> Either pos (S.Set pos, [pos])
    procesar [] !vis !acc = Right (vis, acc)
    procesar (x:xs) !vis !acc
        | esExito x = Left x
        | otherwise =
            let !vis' = S.insert x vis
            -- S.size cuesta O(1): si no aumenta, 'x' ya estaba visitado.
            in if S.size vis' == S.size vis
               then procesar xs vis acc
               else procesar xs vis' (x : acc)
{-# INLINABLE resolutor #-}