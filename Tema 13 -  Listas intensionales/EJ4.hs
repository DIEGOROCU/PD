posiciones :: Int -> [Int] -> [Int]
posiciones num lista = [ p | p <- [0..(length lista) - 1], head (drop p lista) == num]

main = do
    print (posiciones 3 [1,2,3,4,5,3,6,7,3])