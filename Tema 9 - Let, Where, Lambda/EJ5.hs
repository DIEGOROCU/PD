deriva :: Float -> (Float -> Float) -> (Float -> Float)
deriva h f = \x -> derivada x
    where
        derivada x = ((f xMh) - (f xmh)) / dh
            where
                xMh = x + h
                xmh = x - h
                dh = 2 * h

main :: IO ()
main = do
    print (deriva 0.0001 (\x -> x^2) 2)
    print (deriva 0.0001 (\x -> sin x) (pi/4))