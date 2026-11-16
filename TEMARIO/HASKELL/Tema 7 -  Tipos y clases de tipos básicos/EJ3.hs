siguiente :: (Enum a, Bounded a, Eq a) => a -> a
siguiente x = if x == maxBound then minBound else succ x

main :: IO ()
main = do
    print (siguiente 'a')
    print (siguiente False) 
    print (siguiente True)
    print (siguiente (maxBound :: Int))
    print (siguiente GT)    
