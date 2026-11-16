segundosAFecha :: Int -> (Int, Int, Int, Int, Int, Int)
segundosAFecha s = (año, mes, dia, hora, minuto, segundo)
  where
    año = s `div` (60 * 60 * 24 * 365)
    mes = (s `mod` (60 * 60 * 24 * 365)) `div` (60 * 60 * 24 * 30)
    dia = (s `mod` (60 * 60 * 24 * 30)) `div` (60 * 60 * 24)
    hora = (s `mod` (60 * 60 * 24)) `div` (60 * 60)
    minuto = (s `mod` (60 * 60)) `div` 60
    segundo = s `mod` 60

main :: IO ()
main = do
    print (segundosAFecha 100000000000000000)