ulti :: Eq a => a -> [a] -> Int
ulti x ys = ultiReversed x (reverse ys)


ultiReversed :: Eq a => a -> [a] -> Int
ultiReversed _ [] = -1
ultiReversed x (y:ys) | x == y = length ys
                        | otherwise = ultiReversed x ys

main = do
    print (ulti 3 [1,2,3,4,5])
    print (ulti 'a' "banana")
    print (ulti 10 [1,2,3])
    print (ulti 'z' "hello")