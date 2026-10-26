data Arbol a = Nodo a (Bosque a)
type Bosque a = [Arbol a]

sumaArbol :: Num a => Arbol a -> a
sumaArbol (Nodo n []) = n
sumaArbol (Nodo n bos) = n + sum (map sumaArbol bos)

main :: IO ()
main = do
    let arbol = Nodo 1 [Nodo 2 [], Nodo 3 [Nodo 4 []]]
    print (sumaArbol arbol)