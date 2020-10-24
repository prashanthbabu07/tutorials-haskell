sayHello :: String -> IO ()
sayHello x =
  putStrLn ("Hello, " ++ x ++ "!")

triple :: Int -> Int
triple x = x * 3

-- square :: Int -> Int
-- square x = x * x

square :: Float -> Float
square x = x * x

area_of_circle :: Float -> Float
area_of_circle r = pi * square r