raices :: Float -> Float -> Float -> [Float]
raices a b c
    | disc < 0 = []
    | disc == 0 = [raizP]
    | otherwise = [raizP, raizN]
    where
        raizP = (-b + sqrt disc) / (2*a)
        raizN = (-b - sqrt disc) / (2*a)
        disc = b^2 - 4*a*c

main :: IO ()
main = do
    print (raices 1 (-3) 2)
    print (raices 1 2 1)
    print (raices 1 0 1)