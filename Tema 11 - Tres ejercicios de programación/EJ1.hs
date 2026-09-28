intercalar :: [a] -> [a] -> [a]
intercalar x y  | null x = y
                | null y = x
                | otherwise = head x : head y : intercalar (tail x) (tail y)

intercalar' :: [a] -> [a] -> [a]
intercalar' [] ys = ys
intercalar' xs [] = xs
intercalar' (x:xs) (y:ys) = x : y : intercalar' xs ys

main = do
    print (intercalar [1,2,3] [4,5,6])
    print (intercalar "abc" "def")
    print (intercalar [1,2] [])
    print (intercalar [] [3,4])