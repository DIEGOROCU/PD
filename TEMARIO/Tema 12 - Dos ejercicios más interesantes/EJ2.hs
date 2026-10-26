import Data.Char (isDigit, digitToInt)

evalua :: [Char] -> [Int] -> Bool -> (Int, Bool)
evalua e p eraDigito | null e = case p of
                [] -> (0, True)
                [_] -> (head p, True)
                (_:_:_) -> (0, False)
            | head e == ' ' = evalua (drop 1 e) p False
            | isDigit (head e) = if eraDigito then evalua (drop 1 e) (digitToInt (head e) + (head p * 10) : drop 1 p) True
                else evalua (drop 1 e) (digitToInt (head e) : p) True
            | otherwise = if length p < 2 then (0, False)
                else case head e of
                    '+' -> evalua (drop 1 e) ( (head (drop 1 p)) + head p : drop 2 p) False
                    '-' -> evalua (drop 1 e) ( (head (drop 1 p)) - head p : drop 2 p) False
                    '*' -> evalua (drop 1 e) ( (head (drop 1 p)) * head p : drop 2 p) False
                    '^' -> evalua (drop 1 e) ( (head (drop 1 p)) ^ head p : drop 2 p) False
                    _ -> (0, False)

main = do
    print (evalua "3 2 ^ 11 2 7 - + *" [] False)