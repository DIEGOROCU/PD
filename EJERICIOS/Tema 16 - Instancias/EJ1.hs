data Intervalo a = Intervalo a a deriving (Show)

instance (Eq a, Ord a) => Eq (Intervalo a) where
    (Intervalo i1 f1) == (Intervalo i2 f2) =
                                            (i1 > f1 && i2 > f2) -- Caso vacio
                                            ||
                                            (i1 == i2 && f1 == f2)

instance (Eq a, Ord a) => Ord (Intervalo a) where
    (Intervalo i1 f1) <= (Intervalo i2 f2) =
                                            (i1 < i2)
                                            ||
                                            (i1 == i2 && f1 < f2)

main :: IO ()
main = do
    let i1 = Intervalo 1 5
    let i2 = Intervalo 2 6
    let i3 = Intervalo 1 5
    print (i1 == i2) -- False
    print (i1 == i3) -- True
    print (i1 <= i2) -- True
    print (i2 <= i1) -- False