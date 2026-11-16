import Data.Char (toUpper)

componer :: [a -> a] -> a -> a
componer = foldr (.) (\x -> x)

main :: IO ()
main = do
    let f = componer [map toUpper, filter (/= ' '), reverse]
    putStrLn $ f "Soy un hacha programando en Haskell"