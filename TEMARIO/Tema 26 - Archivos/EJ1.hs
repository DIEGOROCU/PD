cuentaArchivo :: FilePath -> IO ()
cuentaArchivo archivo = do
    contenido <- readFile archivo
    let lineas = lines contenido
        numLineas = length lineas
        numPalabras = sum (map (length . words) lineas)
        numCaracteres = length contenido
    putStrLn $ "Número de líneas: " ++ show numLineas
    putStrLn $ "Número de palabras: " ++ show numPalabras
    putStrLn $ "Número de caracteres: " ++ show numCaracteres

main :: IO ()
main = do
    putStrLn "Introduce el nombre del archivo:"
    archivo <- getLine
    cuentaArchivo archivo