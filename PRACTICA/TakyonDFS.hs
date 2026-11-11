{-# LANGUAGE BangPatterns #-}      -- Permite usar '!' para forzar evaluación estricta
{-# OPTIONS_GHC -O2 #-}            -- Obliga al compilador a aplicar la máxima optimización (inlining y código nativo)
module Resolutores.TakyonDFS (resolutor) where

import Jugable
import Data.Bits (xor)
import qualified Data.IntMap.Strict as IM

{-|
  Resolutor: DFS "Takyon" (Búsqueda en Profundidad Rápida)
  Autores: Pau Frangi y Diego Rodríguez
-}

-- Hash FNV-1a (64 bits). Implementación directa y recursiva para minimizar
-- el uso de memoria.
hashRapido :: String -> Int
hashRapido = go 14695981039346656037
  where
    go !h [] = h
    go !h (c:cs) = go ((h `xor` fromEnum c) * 1099511628211) cs

resolutor :: Jugable pos => Resolutor pos
resolutor inicio
    | esExito inicio = Just [inicio]
    | otherwise      = dfs inicialMemoria [(inicio, [inicio])]
  where
    -- Guardamos el hash del estado inicial en el Radix Tree (IntMap)
    !inicialMemoria = IM.singleton (hashRapido (show inicio)) [inicio]

    -- Motor DFS estricto (!). Fuerza el procesamiento inmediato para que GHC
    -- no acumule basura (thunks) que dispare el Garbage Collector.
    dfs :: Jugable pos => IM.IntMap [pos] -> [(pos, [pos])] -> Maybe [pos]
    dfs !_ [] = Nothing
    dfs !visitados ((actual, camino) : pila) =
        -- Inyección LIFO directa: pasamos la 'pila' entera como acumulador final
        -- (para no usar el operador (++) que es lento)
        let (!encontrado, !nuevosVisitados, !nuevaPila) = 
                expandir (opciones actual) visitados camino pila
        in case encontrado of
             Just sol -> Just sol
             Nothing  -> dfs nuevosVisitados nuevaPila

    -- Motor de expansión que genera hijos, filtra visitados y apila en bloque
    expandir :: Jugable pos 
             => [pos] 
             -> IM.IntMap [pos] 
             -> [pos] 
             -> [(pos, [pos])] 
             -> (Maybe [pos], IM.IntMap [pos], [(pos, [pos])])
    expandir [] !vis _ !pilaAcc = (Nothing, vis, pilaAcc)
    expandir (x:xs) !vis cam !pilaAcc
        -- Poda Temprana: si ganamos, aborta al instante todo el cálculo colateral
        | esExito x = (Just (reverse (x:cam)), vis, [])
        | otherwise =
            let !h = hashRapido (show x)
            -- Búsqueda O(1) usando el hash numérico a nivel de bits
            in case IM.lookup h vis of
                 Just guardados 
                   -- Comprobamos (==) solo si el hash coincide para evitar falsos positivos
                   | x `elem` guardados -> 
                       expandir xs vis cam pilaAcc
                   | otherwise -> 
                       -- Estado nuevo, pero hubo colisión de hash (casi imposible). Inserción O(1)
                       let !vis' = IM.insert h (x : guardados) vis
                       in expandir xs vis' cam ((x, x:cam) : pilaAcc)
                 Nothing -> 
                     -- Estado limpio. Se guarda en el IntMap y se inyecta directamente en la pilaAcc
                     let !vis' = IM.insert h [x] vis
                     in expandir xs vis' cam ((x, x:cam) : pilaAcc)