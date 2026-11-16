insertar :: Int -> [Int] -> [Int]
insertar a [] = [a]
insertar a (x:xs) = if a <= x
                    then a : x : xs
                    else x : insertar a xs

main = do
    print (insertar 3 [1, 2, 4, 5])