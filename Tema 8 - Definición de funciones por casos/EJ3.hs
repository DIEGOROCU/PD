collatz :: Int -> Int
collatz n   | n == 1 = 0
            | even n = 1 + collatz (n `div` 2)
            | otherwise = 1 + collatz (3*n + 1)

main :: IO ()
main = do
    print (collatz 1)
    print (collatz 2)
    print (collatz 3)
    print (collatz 4)
    print (collatz 5)
    print (collatz 6)