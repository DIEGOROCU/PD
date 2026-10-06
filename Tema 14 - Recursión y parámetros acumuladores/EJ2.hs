euclides :: Int -> Int -> Int
euclides a 0 = a
euclides 0 b = b
euclides a b    | a == b = a
                | a > b = euclides (a `mod` b) b
                | a < b = euclides a (b `mod` a)

main :: IO ()
main = do
    print (euclides 12 18)