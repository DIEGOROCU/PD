ordenada, ordenada', ordenada'', ordenada''' :: (Ord a) => [a] -> Bool

ordenada [] = True
ordenada [x] = True
ordenada (x:y:xs) = x <= y && ordenada (y:xs)

ordenada' [] = True
ordenada' [x] = True
ordenada' x = and (map (uncurry (<=)) zipeada)
    where 
            l1 = init x
            l2 = tail x
            zipeada = zip l1 l2

ordenada'' [] = True
ordenada'' [x] = True
ordenada'' x = and (map ( \i -> (head drop (i - 1) x) <= (head drop i x) ) [1 .. length x])

main = do
    print (ordenada'' [1,2,3,4,5])
    print (ordenada'' [1,3,2,4,5])
    print (ordenada'' "abcde")
    print (ordenada'' "abced")
    --print (ordenada' [1,2,3,4,5])
    --print (ordenada' [1,3,2,4,5])
    --print (ordenada' "abcde")
    --print (ordenada' "abced")