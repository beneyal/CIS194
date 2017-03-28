module Week4 where

fun1' :: [Integer] -> Integer
fun1' = product . map ((-) 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1) . iterate collatz
        where collatz n = if even n
                          then n `div` 2
                          else 3 * n + 1

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

insert :: a -> Tree a -> Tree a
insert a Leaf = Node 0 Leaf a Leaf
insert a (Node h Leaf a' Leaf) = Node (h + 1) (insert a Leaf) a' Leaf
insert a (Node h Leaf a' right) = Node h (insert a Leaf) a' right
insert a (Node h left a' Leaf) = Node h left a' (insert a Leaf)
insert a (Node h left@(Node hl _ _ _) a' right@(Node hr _ _ _))
  | hl < hr   = Node h (insert a left) a' right
  | otherwise = Node (1 + max hr nhr) left a' nr
                where nr@(Node nhr _ _ _) = insert a right


foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

xor :: [Bool] -> Bool
xor = foldr xor' False
      where xor' True False = True
            xor' False True = True
            xor' _ _ = False
