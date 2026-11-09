cuentaPalabras :: String -> Int
cuentaPalabras = length . words

main :: IO ()
main = interact (unlines . map (show . cuentaPalabras) . lines)