data Estacion = Invierno | Primavera | Verano | Otoño deriving (Eq, Show)

instance Enum Estacion where
    toEnum n
        | n `mod` 4 == 0 = Invierno
        | n `mod` 4 == 1 = Primavera
        | n `mod` 4 == 2 = Verano
        | n `mod` 4 == 3 = Otoño
    fromEnum x
        | x == Invierno = 0
        | x == Primavera = 1
        | x == Verano = 2
        | x == Otoño = 3
    succ e = toEnum (fromEnum e + 1)
    pred e = toEnum (fromEnum e - 1)

main :: IO ()
main = do
    print (succ Invierno) -- Primavera
    print (succ Primavera) -- Verano
    print (succ Verano) -- Otoño
    print (succ Otoño) -- Invierno
    print (pred Invierno) -- Otoño
    print (pred Primavera) -- Invierno
    print (pred Verano) -- Primavera
    print (pred Otoño) -- Verano