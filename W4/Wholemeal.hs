{-# OPTIONS_GHC -Wall #-}

module Wholemeal where



-- Ex. 1: rewritings offun1 and fun2
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
--fun1' = product . map (pred . pred) . filter even  -- I don't want to use the double pred only because readability for my actual comprehension of Haskell syntax, but I am keeping it because I find it interesting
fun1' = foldr ((*) . (\x -> x - 2)) 1 . filter even

-- Collatz conjecture
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3*n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even .takeWhile (>1) . iterate (\n -> if odd n then 3*n + 1 else n `div` 2)


-- Ex. 2: Binary tree
data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr blossom Leaf

height :: Tree a -> Integer
height Leaf = 0
height (Node h _ _ _) = h

seed :: Tree a -> a -> Tree a -> Tree a
seed left x right  = 
    let h = 1 + max (height left) (height right)
    in Node h left x right

blossom :: a -> Tree a -> Tree a
blossom x Leaf = Node 0 Leaf x Leaf
blossom x (Node _ left y right) = balancer x left y right

ruler :: Tree a -> Tree a -> Ordering
ruler left right = compare (height left) (height right)

balancer :: a -> Tree a -> a -> Tree a -> Tree a
balancer x left y right =
    case ruler left right of
        LT -> seed (blossom x left) y right
        _  -> seed left y (blossom x right)


-- Ex. 3: More folds

-- 3.1: Xor
xor :: [Bool] -> Bool
xor = foldl (/=) False

-- 3.2: Map'
-- map' :: (a -> b) -> [a] -> [b]
-- map' f = foldr (\x acc -> f x : acc) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

-- 4: Sieve of Sundaram


--cartProd :: [a] -> [b] -> [(a, b)]
--cartProd xs ys = [(x,y) | x <- xs, y <- ys]

excludeNumbers :: Integer -> [Integer]
excludeNumbers n = [i + j + 2*i*j | j <- [1..n], i <- [1..j], i + j + 2*i*j <= n]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) $ filter (`notElem` excludeNumbers n) [1..n] 