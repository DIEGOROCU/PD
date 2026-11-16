ponAlFinal :: FilePath -> IO ()
ponAlFinal archivo = do
    char <- getChar
    if char == '\n'
        then return ()
        else do
            appendFile archivo [char]
            ponAlFinal archivo

main :: IO ()
main = do
    putStrLn "Introduce el nombre del archivo:"
    archivo <- getLine
    putStrLn "Introduce caracteres para añadir al final del archivo (una línea vacía para terminar):"
    ponAlFinal archivo