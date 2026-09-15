todos :: [Bool] -> Bool
todos [] = True
todos (x:xs) = x && todos xs

cremallera :: [Int] -> [Int] -> [Int]
cremallera [] ys = ys
cremallera xs [] = xs
cremallera (x:xs) (y:ys) = x : y : cremallera xs ys

cremallera2 :: [Int] -> [Int] -> [(Int, Int)]
cremallera2 [] ys = []
cremallera2 xs [] = []
cremallera2 (x:xs) (y:ys) = (x, y) : cremallera2 xs ys

aplana :: [[a]] -> [a]
aplana [] = []
aplana (xs:xss) = xs ++ aplana xss

main :: IO ()
main = do
    print (todos [True, True, False]) -- False
    print (todos [True, True, True])  -- True
    print (cremallera [1, 2, 3] [4, 5, 6]) -- [1,4,2,5,3,6]
    print (cremallera2 [1, 2, 3] [4, 5, 6]) -- [(1,4),(2,5),(3,6)]
    print (aplana [[1, 2], [3], [4, 5]]) -- [1,2,3,4,5]
