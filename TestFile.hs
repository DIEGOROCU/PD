main :: IO ()
main = do
  putStrLn "Hello, everybody!"
  putStrLn "Here are the odd numbers from 10 to 20:"
  print (filter odd [10 .. 20])