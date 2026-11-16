data Franja = Manana | Tarde | Noche deriving (Show, Eq, Enum, Bounded)
data Dia = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving (Show, Eq, Enum, Bounded)

class (Eq a, Enum a, Bounded a) => Ciclico a where
    siguiente, anterior :: a -> a
    siguiente x
        | x == maxBound = minBound
        | otherwise = succ x
    anterior x
        | x == minBound = maxBound
        | otherwise = pred x

instance Enum (Dia, Franja) where
    toEnum n = (toEnum (n `div` 3), toEnum (n `mod` 3))
    fromEnum (d, f) = fromEnum d * 3 + fromEnum f

instance Ciclico Dia
instance Ciclico Franja
instance Ciclico (Dia, Franja) 

main :: IO ()
main = do
    print (siguiente (Lunes, Manana)) -- (Lunes, Tarde)
    print (siguiente (Lunes, Noche)) -- (Martes, Manana)
    print (anterior (Lunes, Manana)) -- (Domingo, Noche)
    print (anterior (Lunes, Tarde)) -- (Lunes, Manana)