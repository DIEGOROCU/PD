celsiusAFahrenheit :: Float -> Float
celsiusAFahrenheit x = (x * 9 / 5) + 32

main :: IO ()
main = do
    putStrLn "Celsius to Fahrenheit converter"
    print (celsiusAFahrenheit 0)
    print (celsiusAFahrenheit 100)