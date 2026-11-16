escanear :: (b -> a -> b) -> b -> [a] -> [b]
escanear f v [] = [v]
escanear f v (x:xs) = v : escanear f (f v x) xs

main :: IO ()
main = do
    print $ escanear (+) 0 [1,2,3,4]