safeTail :: [a] -> [a]
safeTail l  | null l = []
            | otherwise = tail l

main :: IO ()
main = do
    print (safeTail [1,2,3])
    print (safeTail ([] :: [Int]))