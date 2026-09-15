f :: [Int] -> [Int]
f [] = []
f [x1:x2:xs] = xs

main :: IO ()
main = do
    print (f [1, 2, 3, 4])