import Data.Char (toUpper)

iniciales :: String -> String
iniciales = map (toUpper . head) . words

largas :: String -> [String]
largas = filter ((> 5) . length) . words

main :: IO ()
main = do
    putStrLn $ iniciales "Soy un hacha programando en Haskell"
    putStrLn $ show $ largas "Soy un hacha programando en Haskell"