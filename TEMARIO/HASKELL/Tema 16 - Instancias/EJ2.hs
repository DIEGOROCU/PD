data Fraccion = Fraccion Int Int deriving (Show)

instance Eq Fraccion where
    (Fraccion n1 d1) == (Fraccion n2 d2) = n1 * d2 == n2 * d1

instance Ord Fraccion where
    (Fraccion n1 d1) <= (Fraccion n2 d2) = (signum d1) * (signum d2) * n1 * d2 <= n2 * d1 * (signum d1) * (signum d2)

instance Num Fraccion where
    (Fraccion n1 d1) + (Fraccion n2 d2) = Fraccion (n1 * d2 + n2 * d1) (d1 * d2)
    (Fraccion n1 d1) * (Fraccion n2 d2) = Fraccion (n1 * n2) (d1 * d2)
    negate (Fraccion n d) = Fraccion (-n) d
    abs (Fraccion n d) = Fraccion (abs n) (abs d)
    signum (Fraccion n d) = Fraccion (signum n * signum d) 1
    fromInteger x = Fraccion (fromInteger x) 1

main :: IO ()
main = do
    let f1 = Fraccion 1 2
    let f2 = Fraccion 3 4
    print (f1 + f2) -- Fraccion 10 8
    print (f1 * f2) -- Fraccion 3 8
    print (negate f1) -- Fraccion -1 2
    print (abs (Fraccion (-1) 2)) -- Fraccion 1 2
    print (signum (Fraccion (-1) 2)) -- Fraccion -1 1
    print (fromInteger 5) -- Fraccion 5 1