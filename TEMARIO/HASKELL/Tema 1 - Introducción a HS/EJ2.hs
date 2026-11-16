todos :: [Bool] -> Bool
todos [] = True
todos (x:xs) = x && todos xs

main :: IO ()
main = do
    print (todos [True, True, True])
    print (todos [True, False, True])