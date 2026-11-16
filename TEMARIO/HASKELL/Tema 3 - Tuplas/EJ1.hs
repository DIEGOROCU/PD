productoEscalar :: Num a => (a, a, a) -> (a, a, a) -> a
productoEscalar (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

main :: IO ()
main = do
    let v1 = (1, 2, 3)
    let v2 = (4, 5, 6)
    print (productoEscalar v1 v2)