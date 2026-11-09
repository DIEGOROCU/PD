leeIntEnRango :: Int -> Int -> IO Int
leeIntEnRango inf sup =
    do
        putStrLn $ "Introduce un número entre " ++ show inf ++ " y " ++ show sup ++ ":"
        n <- readLn
        if n < inf || n > sup
            then do
                putStrLn $ "El número debe estar entre " ++ show inf ++ " y " ++ show sup
                leeIntEnRango inf sup
            else return n

main :: IO ()
main = do
    putStrLn "Introduce un número entre 1 y 10:"
    n <- leeIntEnRango 1 10
    putStrLn $ "Has introducido: " ++ show n