module Juegos.Juego_LobObeCol where

import Jugable

data Objeto = Lobo | Obeja | Col

gana :: Objeto -> Objeto -> Bool
gana o1 o2 =
    | o1 == Lobo && o2 == Obeja = True
    | o1 == Obeja && o2 == Col = True
    | otherwise = False

instance Jugable LobObeCol where
    