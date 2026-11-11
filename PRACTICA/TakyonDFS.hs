{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -O2 #-}
module Resolutores.TakyonDFS (resolutor) where

import Jugable
import Data.Bits (xor)
import qualified Data.IntMap.Strict as IM

{-|
  Resolutor: DFS "Takyon" (Búsqueda en Profundidad Rápida)
  Autores: Pau Frangi y Diego Rodríguez
-}

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
    !inicialMemoria = IM.singleton (hashRapido (show inicio)) [inicio]

    dfs :: Jugable pos => IM.IntMap [pos] -> [(pos, [pos])] -> Maybe [pos]
    dfs !_ [] = Nothing
    dfs !visitados ((actual, camino) : pila) =
        -- Pasamos 'pila' directamente como acumulador final
        let (!encontrado, !nuevosVisitados, !nuevaPila) = 
                expandir (opciones actual) visitados camino pila
        in case encontrado of
             Just sol -> Just sol
             Nothing  -> dfs nuevosVisitados nuevaPila

    -- Motor de expansión con inyección LIFO directa
    expandir :: Jugable pos 
             => [pos] 
             -> IM.IntMap [pos] 
             -> [pos] 
             -> [(pos, [pos])] 
             -> (Maybe [pos], IM.IntMap [pos], [(pos, [pos])])
    expandir [] !vis _ !pilaAcc = (Nothing, vis, pilaAcc)
    expandir (x:xs) !vis cam !pilaAcc
        | esExito x = (Just (reverse (x:cam)), vis, [])
        | otherwise =
            let !h = hashRapido (show x)
            in case IM.lookup h vis of
                 Just guardados 
                   | x `elem` guardados -> 
                       expandir xs vis cam pilaAcc
                   | otherwise -> 
                       -- Estado nuevo con colisión (casi imposible)
                       let !vis' = IM.insert h (x : guardados) vis
                       in expandir xs vis' cam ((x, x:cam) : pilaAcc)
                 Nothing -> 
                     -- Estado completamente nuevo, inyección directa en pilaAcc
                     let !vis' = IM.insert h [x] vis
                     in expandir xs vis' cam ((x, x:cam) : pilaAcc)