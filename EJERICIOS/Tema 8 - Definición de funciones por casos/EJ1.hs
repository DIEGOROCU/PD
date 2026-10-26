conjuncion :: Bool -> Bool -> Bool
conjuncion a b = if (a && b) == True then True else False

main :: IO ()
main = do
    print (conjuncion True True)
    print (conjuncion True False)
    print (conjuncion False True)
    print (conjuncion False False)