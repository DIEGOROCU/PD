data Fraccion a = Fraccion a a deriving (Show, Eq)

infix 6 +/
(+/) :: (Num a) => Fraccion a -> Fraccion a -> Fraccion a
Fraccion n1 d1 +/ Fraccion n2 d2 = Fraccion (n1*d2 + n2*d1) (d1*d2)

infixr 7 */
(*/) :: (Num a) => Fraccion a -> Fraccion a -> Fraccion a
Fraccion n1 d1 */ Fraccion n2 d2 = Fraccion (n1*n2) (d1*d2)

main :: IO ()
main = do
    print (Fraccion 1 2 +/ Fraccion 1 3 */ Fraccion 2 1)