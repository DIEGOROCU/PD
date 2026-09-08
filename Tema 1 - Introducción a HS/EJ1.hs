qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort menores ++ [x] ++ qsort mayores
    where
    menores = filter (<= x) xs
    mayores = filter (> x) xs

qsortInv :: [Int] -> [Int]
qsortInv [] = []
qsortInv (x:xs) = qsortInv mayores ++ [x] ++ qsortInv menores
    where
    menores = filter (<= x) xs
    mayores = filter (> x) xs

main :: IO ()
main = do
    print (qsort [5, 1, 4, 2, 3])
    print (qsortInv [5, 1, 4, 2, 3])
