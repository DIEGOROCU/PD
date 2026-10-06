while :: (a -> Bool) -> (a -> a) -> a -> a
while cond cuerpo estado
  | cond estado = while cond cuerpo (cuerpo estado)
  | otherwise = estado

ifThenElse :: Bool -> a -> a -> a
ifThenElse True valor _ = valor
ifThenElse False _ valor = valor

ifThen :: Bool -> (a -> a) -> a -> a
ifThen cond cuerpo estado = ifThenElse cond (cuerpo estado) estado

ejemplo :: (Int, Int, Int) -> (Int, Int, Int)
ejemplo (x, y, _) = while condicion cuerpo (x, y, x)
    where
        condicion (x', y', _) = x' <= y'
        cuerpo (x', y', z') = (x' + 1, y', z' * x')

main = do
    print (ejemplo (2, 3, 1))
    print (ifThen (0 <= 1) (\(x, y, _) -> (x, y, 0)) (2, 3, 1))
