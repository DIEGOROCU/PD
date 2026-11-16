sumaCosas, sumaCosas' :: ((Int, (Int, Int)), (Int, Int)) -> Int
sumaCosas ((x, (y, z)), (u, v)) = x + y + z + u + v
sumaCosas' t = 
    let ((x, (y, z)), (u, v)) = t
    in x + y + z + u + v
suma, suma' :: Int
suma = sumaCosas ((1, (2, 5)), (4, 3)) -- suma = 1+2+5+4+3 = 15
suma' = sumaCosas' ((1, (2, 5)), (4, 3)) -- suma' = 1+2+5+4+3 = 15

main :: IO ()
main = do
    print suma
    print suma'