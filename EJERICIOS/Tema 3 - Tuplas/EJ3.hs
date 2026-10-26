sumaCosas :: ((Int, (Int, Int)), (Int, Int)) -> Int
sumaCosas t = let ((a, (b, c)), (d, e)) = t in a + b + c + d + e

suma :: Int
suma = sumaCosas ((1, (2, 5)), (4, 1)) -- suma = 1+2+5+4+1 = 13

main :: IO ()
main = do
    print suma