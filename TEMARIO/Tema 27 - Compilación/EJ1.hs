leeNotas :: IO [Double]
leeNotas = do
    nota <- readLn
    if nota == ""
        then return []
        else do
            resto <- leeNotas
            return (nota : resto)

informe :: [Double] -> String
informe notas = "Notas: " ++ show (length notas) ++ "\n" ++
                "Media: " ++ show (sum notas / fromIntegral (length notas)) ++ "\n" ++
                "Máxima: " ++ show (maximum notas) ++ "\n" ++
                "Aprobados: " ++ show (length (filter (>= 5) notas)) ++ "\n" ++

main :: IO ()
main = do
    putStrLn "Introduce las notas (una línea vacía para terminar):"
    notas <- leeNotas
    putStrLn $ informe notas
