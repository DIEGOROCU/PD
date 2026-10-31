module Resolutores.Resolutor_LobOveCol where

import Jugable

resolutor :: Jugable pos => Resolutor pos
resolutor p | esExito p = Just [p]
            | otherwise = case opciones p of
                            []    -> Nothing
                            (q:_) -> fmap (p:) (resolutor q)