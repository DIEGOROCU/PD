import Data.Char (isLower, isUpper)

siguienteLetra :: Char -> Int -> Char
siguienteLetra c n
    | isLower c = toEnum (((((fromEnum c) + n) - aMin) `mod` 26) + aMin)
    | isUpper c = toEnum (((((fromEnum c) + n) - aMay) `mod` 26) + aMay)
    | otherwise = '\0'
    where
        aMin = fromEnum 'a'
        aMay = fromEnum 'A'


cesar :: [Char] -> Int -> [Char]
cesar [] _ = []
cesar (x:xs) n = siguienteLetra x n : cesar xs n

main :: IO ()
main = do
    let s1 = "HolamundoZz"
    let n = 3
    putStrLn $ "Texto original: " ++ s1
    putStrLn $ "Texto cifrado con desplazamiento " ++ show n ++ ": " ++ cesar s1 n