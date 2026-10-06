data BinArb a = Vacio | Node a (Hijos a) deriving Show
type Hijos a = (BinArb a, BinArb a)

numHojas :: BinArb a -> Int
numHojas Vacio = 0
numHojas (Node a (Vacio, Vacio)) = 1
numHojas (Node a (h1, h2)) = numHojas h1 + numHojas h2

mapAB :: (a -> b) -> BinArb a -> BinArb b
mapAB f Vacio = Vacio
mapAB f (Node a (h1, h2)) = Node (f a) (mapAB f h1, mapAB f h2)

main :: IO ()
main = do
    print (numHojas (Node 1 (Node 2 (Vacio, Vacio), Node 3 (Vacio, Vacio))))
    print (mapAB (+1) (Node 1 (Node 2 (Vacio, Vacio), Node 3 (Vacio, Vacio))))