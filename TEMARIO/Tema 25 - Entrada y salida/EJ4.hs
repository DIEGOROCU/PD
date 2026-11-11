ejecuta :: [IO ()] -> IO ()
ejecuta acciones = foldr (>>) (return ()) acciones

secuencia :: [IO a] -> IO [a]
secuencia [] = return []
secuencia (accion : acciones) = do
    resultado <- accion
    resultados <- secuencia acciones
    return (resultado : resultados)

leeLineas :: Int -> IO [String]
leeLineas n = secuencia (replicate n getLine)

main :: IO ()
main = do
    putStrLn "Introduce el número de líneas a leer:"
    n <- readLn
    lineas <- leeLineas n
    putStrLn "Las líneas introducidas son:"
    ejecuta (map putStrLn lineas)