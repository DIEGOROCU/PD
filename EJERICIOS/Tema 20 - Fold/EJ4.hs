fib :: Int -> Int
fib n = snd (foldl (\(x, y) _ -> (x + y, x)) (1, 1) [1..n])

main :: IO ()
main = do
    print $ fib 10