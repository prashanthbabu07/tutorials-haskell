-- learn.hs

module Learn where

-- add :: Integer -> Integer -> Integer
import Data.Char

add a b = a + b

--functions
in_range min max x =
  x >= min && x <= max

--types name :: <type>

x :: Integer
x = add 1 2

--let bind result of expression to a name
is_in_range min max x =
  let in_lower_bound = min <= x
      in_upper_bound = max >= x
   in in_lower_bound && in_upper_bound

--where expressions first and bind name in where
is_in_range_where min max x = ilb && iub
  where
    ilb = min <= x
    iub = max >= x

--control
in_range_if min max x =
  if ilb then iub else False
  where
    ilb = min <= x
    iub = max >= x

--recursion factorial there is not for or while loops
fac n =
  if n < 1
    then 1
    else n * fac (n -1)

--guards with more conditionals
guard_fac n
  | n <= 1 = 1
  | otherwise = n * fac (n -1)

--pattern matching
is_zeor 0 = True --is_zero of 0 is true
is_zeor _ = False --otherwise/any pattern false

--accumulators (tail recursive function) safter then recursion because of stack overflow
acc_fac n = aux n 1
  where
    aux n acc
      | n <= 1 = acc
      | otherwise = aux (n -1) (n * acc)

--lists

my_list = [1, 2, 3, 4]

--prepend
pp_list = 1 : 2 : 3 : 4 : []

--create list using recursion
ascending_list :: Int -> Int -> [Int]
ascending_list n m
  | m < n = []
  | m == n = [m]
  | m > n = n : ascending_list (n + 1) m --prepending to list

-- (x : xs) prepend x to existing list xs
--list comprehension

--[2 * x | x <- [1, 2, 3], mod x 2 == 0]

-- sum :: [Int] -> Int
-- sum [] = 0
-- sum [x : xs] = x + sum xs

-- tuples
--(1, 2) :: (Int, Int)

--usage add_tuples [(1, 2), (1,3)]
add_tuples :: [(Int, Int)] -> [Int]
add_tuples xs = [x + y | (x, y) <- xs]

--element is in list
is_in_list :: (Eq a) => a -> [a] -> Bool
is_in_list _ [] = False
is_in_list e (x : xs) = (e == x) || (e `is_in_list` xs)

--remove duplcates in the list
remove_dups_in_list :: (Eq a) => [a] -> [a]
remove_dups_in_list [] = []
remove_dups_in_list (x : xs)
  | x `is_in_list` xs = remove_dups_in_list xs
  | otherwise = x : remove_dups_in_list xs

sum_of_elements :: [Int] -> Int
sum_of_elements [] = 0
sum_of_elements (x : xs) = x + sum_of_elements xs

evens_from_list :: [Int] -> [Int]
evens_from_list [] = []
evens_from_list (x : xs)
  | mod x 2 == 0 = x : evens_from_list xs
  | otherwise = evens_from_list xs

is_ascending :: [Int] -> Bool
is_ascending [] = True
is_ascending [_] = True
is_ascending (x : y : xs) = (x <= y) && is_ascending (y : xs)

-- path_is_in_list :: (Eq (a, b)) => (a, b) -> [(a, b)] -> Bool
-- path_is_in_list _ [] = False
-- path_is_in_list (a, b) ((x, y) : xs) = (a == x && b == y) || ((a, b) `path_is_in_list` xs)

has_path :: [(Int, Int)] -> Int -> Int -> Bool
has_path [] x y = x == y
has_path xs x y
  | x == y = True
  | otherwise =
    let xsf = [(n, m) | (n, m) <- xs, n /= x]
     in or [has_path xsf m y | (n, m) <- xs, n == x] --list of bools

-- higer order functions & anonymous functions (no name)
--map and filter
has_path_in_list :: [(Int, Int)] -> Int -> Int -> Bool
has_path_in_list [] x y = x == y
has_path_in_list xs x y
  | x == y = True
  | otherwise =
    let xsf = filter (\(n, _) -> n /= x) xs
     in or [has_path xsf m y | (n, m) <- xs, n == x]

-- partial function application - currying
-- f :: a -> b -> c -> d
-- f :: a -> (b -> (c -> d))

add_curry :: Int -> Int -> Int
-- add_curry x y = x + y
-- add_curry x = (\y -> x + y)
add_curry = (\x -> (\y -> x + y))

-- acc_fac n = aux n 1
--   where
--     aux n acc
--       | n <= 1 = acc
--       | otherwise = aux (n -1) (n * acc)

list_count xs = aux xs 0
  where
    aux xs acc
      | xs == [] = acc
      | otherwise = aux (tail xs) (acc + 1)

count e =
  foldr (\x acc -> if e == x then acc + 1 else acc) 0

is_all e =
  foldr (\x acc -> e == x && acc) True

-- value = is_all [True, False]

-- the below is weird fails if we remove _
length_foldr _ = foldr (\_ -> (+) 1) 0

map_foldr f = foldr ((:) . f) []

-- data types

-- color

data Color
  = Red
  | Blue
  | Green

-- recursive data types
data PeaNum
  = Succ PeaNum
  | Zero

data Calculation
  = Add Int Int
  | Sub Int Int
  | Mul Int Int
  | Div Int Int

calc :: Calculation -> Int
calc (Add x y) = x + y
calc (Sub x y) = x - y
calc (Mul x y) = x * y
calc (Div x y) = div x y

four :: PeaNum
four = Succ $ Succ $ Succ $ Succ $ Zero

-- data Tree a = Leaf | Node (Tree a) a (Tree a)

-- tree :: Tree Int
-- tree = Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 3 Leaf) 4 Leaf)

-- folding exercise
reverse_fold :: [a] -> [a]
reverse_fold = foldl (\acc x -> x : acc) []

list_mapper = map ((:) 5) [[1, 2], [1, 3]]

-- same as above list_mapper
list_mapper_express = map (\x -> 5 : x) [[1, 2], [1, 3]]

-- [[5,1,2],[5,1,3]]

list_prefixes :: [a] -> [[a]]
list_prefixes = foldr (\x acc -> [x] : (map ((:) x) acc)) []

list_prefixes_expressive :: [a] -> [[a]]
list_prefixes_expressive = foldr (\x acc -> [x] : (map (\xs -> x : xs) acc)) []

-- data Trie a = Leaf a | Node a [Trie a]

-- fold_trie :: (b -> a -> b) -> Trie a -> b
-- fold_trie f acc (Leaf x) = f acc x
-- fold_trie f acc (Node x xs) = foldl f' (f acc x) xs
--   where
--     f' acc t = fold_trie f acc t

-- interpolatin lagrange
-- https://en.wikipedia.org/wiki/Lagrange_polynomial

-- lagrange_polynomial :: [(Float, Float)] -> Float -> Float
-- lagrange_polynomial xs x =
--   foldl (\acc (xj, y) -> acc + (y * l xj)) 0
--   where
--     l xj =
--       foldl
--         ( \acc (xk, _) ->
--             if xj == xk
--               then acc
--               else acc * ((x - xk) / (xj - xk))
--         )
--         1
--         xs

-- trie tree for chars
data Trie a = Leaf a | Node a [Trie a]

foldtrie :: (b -> a -> b) -> b -> Trie a -> b
foldtrie f acc (Leaf x) = f acc x
foldtrie f acc (Node x xs) = foldl f' (f acc x) xs
  where
    f' acc t = foldtrie f acc t

t = Node 'c' [(Node 'a' [Leaf 'r', Leaf 't']), (Node 'o' [Node 'o' [Leaf 'l']])]

--records

data Person = Person
  { name :: String,
    age :: Int
  }

greet :: Person -> [Char]
greet person = "Hi " ++ name person

--same as above
-- greet (Person name _) = "Hi " ++ name

data Point
  = D2 {a :: Int, b :: Int}
  | D3 {a :: Int, b :: Int, c :: Int}

--type classes

data Temperature = C Float | F Float

instance Eq Temperature where
  (==) (C n) (C m) = n == m
  (==) (F n) (F m) = n == m
  (==) (F f) (C c) = (1.8 * c + 32) == f
  (==) (C c) (F f) = (1.8 * c + 32) == f

--maybe
-- data Maybe a = Nothing | Just a
-- f x  -> success or failure

safediv :: Integral a => a -> a -> Maybe a
safediv a b = if b == 0 then Nothing else Just $ div a b

--IO finally for pring hello world
hw = putStrLn "hello world"

--hw :: IO () - () is typle with nothing
-- IO is an IO action
-- IO monad

greet_io :: IO ()
greet_io = do
  putStrLn "what is your name?"
  name <- getLine
  let uname = map toUpper name
  putStrLn ("hello " ++ uname ++ ".")

repl :: IO ()
repl = do
  input <- getLine
  if input /= "quit"
    then do
      putStrLn ("input - " ++ input)
      repl
    else return ()

print_count :: Int -> Int -> IO ()
print_count n m = do
  putStrLn (show n)
  if n < m
    then print_count (n + 1) m
    else return ()

-- type inference

-- monad
-- maybeadd

maybeadd :: Num b => Maybe b -> b -> Maybe b
maybeadd mx y = mx >>= (\x -> Just $ x + y)

maybeaddxy :: Num b => Maybe b -> Maybe b -> Maybe b
maybeaddxy mx my = mx >>= (\x -> my >>= (\y -> Just $ x + y))

monadd :: (Monad m, Num b) => m b -> m b -> m b
monadd mx my =
  mx >>= (\x -> my >>= (\y -> return $ x + y))

monad_add :: (Monad m, Num b) => m b -> m b -> m b
monad_add mx my = do
  x <- mx
  y <- my
  return $ x + y

--maybe monad
-- instance Monad Maybe where
--   m >> f = case m of
--     Nothing -> Nothing
--     Just x -> f x
--   return v = Just v

--quick check

prop a b = (a + b) == (b + a)

-- infinite lists
ones = 1 : ones

-- natural numbers
natural_numbers = asc 1
  where
    asc n = n : (asc $ n + 1)

evens = map (* 2) natural_numbers

odds = filter (\x -> mod x 2 == 1) natural_numbers

--fibonacci sequence

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = (fibonacci $ (n -1)) + (fibonacci $ (n -2))

--this is bad need to lazy initilize. use zipWith (need to understand more)

--application
--web crawling

-- Either is like Maybe but hold value for Nothing (example error value and valid value)

-- Thunks - is for lazy evaluation in haskell

func a b = if a `mod` 2 == 0 then a else b

func_val = func (1 + 1) (1 + 2)

--thunk stored in memory
thunk = if (1 + 1) `mod` 2 == 0 then (1 + 1) else (1 + 2)

--strictness force evaluation instead of lazy

--exceptions can be thrown from purely functional code, but may only be caught
-- with in the IO monad

--semigroup
--monaid is a semigroup with identity element

-- category theory
--  functors -> mapping morphisms from one categor to other (types)
--  applicatives
--  monads

