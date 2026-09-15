mitad :: [a] -> ([a], [a])
mitad x =
    let n = length x `div` 2
    in (take n x, drop n x)

main :: IO ()
main = do
    print (mitad [1, 2, 3, 4]) -- ([1,2],[3,4])
    print (mitad [1, 2, 3])    -- ([1],[2,3])
    print (mitad "abcdef")     -- ("abc","def")
    