esPrimo :: Int -> Bool
esPrimo n = n > 1 && null [ x | x <- [2..floor (sqrt (fromIntegral n))], n `mod` x == 0]

gemelos :: Int -> [(Int, Int)]
gemelos n = [(x, x + 2) | x <- [1..(n-2)], esPrimo x, esPrimo (x+2)]
    
--where primos = map esPrimo [1..n]

main = do
    print (gemelos 100)