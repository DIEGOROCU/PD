module Jugable where

class (Eq posicion, Show posicion) => Jugable posicion where
    opciones :: posicion -> [posicion]
    esExito  :: posicion -> Bool

type Resolutor posicion = posicion -> Maybe [posicion]