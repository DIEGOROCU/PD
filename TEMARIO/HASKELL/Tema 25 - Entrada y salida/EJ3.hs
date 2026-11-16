lineasAlReves :: IO ()
lineasAlReves = leerLineas []

leerLineas :: [String] -> IO ()
leerLineas lineas = do
    linea <- getLine
    if null linea then escribirAlReves lineas
    else leerLineas (lineas ++ [linea])

escribirAlReves :: [String] -> IO ()
escribirAlReves [] = return ()
escribirAlReves (linea : lineas) = do
    escribirAlReves lineas
    putStrLn linea

main :: IO ()
main = do
    putStrLn "Introduce líneas de texto (una línea vacía para terminar):"
    lineasAlReves