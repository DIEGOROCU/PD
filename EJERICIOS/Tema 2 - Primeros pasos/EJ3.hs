f1 :: a -> Bool
f1 _ = True

f2 :: a -> Bool
f2 _ = False

g :: a -> a
g x = x

h1 :: a -> a -> a
h1 x _ = x

h2 :: a -> a -> a
h2 _ y = y

main :: IO ()
main = do
    print (f1 "hola")
    print (f2 "hola")
    print (g 42)
    print (h1 "primero" "segundo")
    print (h2 "primero" "segundo")
