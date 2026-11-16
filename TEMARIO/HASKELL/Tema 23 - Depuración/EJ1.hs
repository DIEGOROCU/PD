posicion :: Eq a => a -> [a] -> Int
posicion _ [] = -1
posicion x (y:ys) 
    | x == y = 0
    | otherwise = if posicion x ys == -1 then -1 else 1 + posicion x ys

main :: IO ()
main = do
    let lista = [1, 2, 3, 4, 5]
    print $ posicion 3 lista -- Debería imprimir 3
    print $ posicion 6 lista -- Debería imprimir -1