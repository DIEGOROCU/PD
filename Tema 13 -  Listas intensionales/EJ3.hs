esPrimo :: Int -> Bool
esPrimo n = n > 1 && null [ x | x <- [2..floor (sqrt (fromIntegral n))], n `mod` x == 0]

cercanos :: Int -> Int -> [(Int, Int)]
cercanos n dist = [(x, x + d) | d <- [1..dist], x <- [1..(n-d)], esPrimo x, esPrimo (x+d)]
    
--where primos = map esPrimo [1..n]

main = do
    print (cercanos 100 4)