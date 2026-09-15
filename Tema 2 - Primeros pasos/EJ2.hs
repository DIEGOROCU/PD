cifraUnidad :: Int -> Int
cifraUnidad n = mod n 10

cifraDecena :: Int -> Int
cifraDecena n = mod (div n 10) 10

cifraCentena :: Int -> Int
cifraCentena n = mod (div n 100) 10

main :: IO ()
main = do
    print (cifraUnidad 123)
    print (cifraDecena 123)
    print (cifraCentena 123)