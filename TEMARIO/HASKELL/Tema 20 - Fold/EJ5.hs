data ArbolBin a = ABVacio | ABHoja a | ABNodo (ArbolBin a) a (ArbolBin a) deriving (Show, Eq)

instance Foldable ArbolBin where
    foldr f v ABVacio = v
    foldr f v (ABHoja x) = f x v
    foldr f v (ABNodo izq x der) = foldr f (f x (foldr f v der)) izq

toList' :: Foldable t => t a -> [a]
toList' x = foldr (:) [] x

main :: IO ()
main = do
    let arbol = ABNodo (ABHoja 1) 2 (ABNodo (ABHoja 3) 4 (ABHoja 5))
    print $ toList' arbol