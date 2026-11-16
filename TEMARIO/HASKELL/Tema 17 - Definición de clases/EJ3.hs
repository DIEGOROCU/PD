class Punteado a where
    punto :: a

instance Punteado Int where
    punto = 0

instance (Punteado a, Eq b) => Eq (a -> b) where
    f == g = f punto == g punto

main :: IO ()
main = do
    let f1 = (\x -> x + 1) :: Int -> Int
    let f2 = (\x -> x + 1) :: Int -> Int
    let f3 = (\x -> x + 2) :: Int -> Int
    print (f1 == f2) -- True
    print (f1 == f3) -- False