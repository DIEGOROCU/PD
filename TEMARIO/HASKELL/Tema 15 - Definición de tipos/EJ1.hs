data Lista a = Vacia | Cons a (Lista a) deriving Show

map' :: (a -> b) -> Lista a -> Lista b
map' f Vacia = Vacia
map' f (Cons x xs) = Cons (f x) (map' f xs)

main :: IO ()
main = do
    print (map' (+1) (Cons 1 (Cons 2 (Cons 3 Vacia))))
    