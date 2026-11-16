sonIguales :: String -> String -> Bool
sonIguales s1 s2 = concat (words s1) == concat (words s2)

main :: IO ()
main = do
    let s1 = "Hola mundo"
    let s2 = "Hola   mundo"
    let s3 = "Hola mundo!"
    
    putStrLn $ "Comparando \"" ++ s1 ++ "\" y \"" ++ s2 ++ "\": " ++ show (sonIguales s1 s2)
    putStrLn $ "Comparando \"" ++ s1 ++ "\" y \"" ++ s3 ++ "\": " ++ show (sonIguales s1 s3)