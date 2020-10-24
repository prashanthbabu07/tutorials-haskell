module Print3 where

myGreeting :: [Char]
myGreeting = "hello" ++ " world"

hello :: String
hello = "hello"

world :: String
world = "world!"

topLevelFunction :: Integer -> Integer
topLevelFunction x =
  x + woot + topLevelValue
  where
    woot :: Integer
    woot = 100

topLevelValue :: Integer
topLevelValue = 100

ared d = pi * (r * r)
  where
    r = d / 2

main :: IO ()
main = do
  putStrLn myGreeting
  putStrLn secondGreeting
  where
    secondGreeting = concat [hello, " ", world]