unaFunc :: (Int, Int) -> Int -- toma un par
unaFunc (x, y) = 2 * x + y

otraFunc :: Int -> Int -> Int -- currificada
otraFunc x y = 2 * x + y

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y

main :: IO ()
main = do
    print (curry' unaFunc 3 4)
    print (uncurry' otraFunc (3, 4))