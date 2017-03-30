{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}
module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = [0, 1] ++ zipWith (+) fibs2 (drop 1 fibs2)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show s = show $ take 20 (streamToList s)

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (+ 1) 1

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

ruler :: Stream Integer
ruler = foldr interleaveStreams undefined (map streamRepeat [0..])

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate = streamMap negate
  (Cons a as) + (Cons b bs) = Cons (a + b) (as + bs)
  (Cons a as) * (Cons b bs) = Cons (a * b) (streamMap (* a) bs + as * (Cons b bs))

instance Fractional (Stream Integer) where
  (Cons a as) / (Cons b bs) = q
    where q = Cons (a `div` b) (streamMap (1 `div` b *) (as - q * bs))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

data Matrix = M Integer Integer Integer Integer

instance Show Matrix where
  show (M a b c d) = show a ++ " " ++ show b ++ "\n" ++ show c ++ " " ++ show d

instance Num Matrix where
  M a b c d * M e f g h = M (a * e + b * g) (a * f + b * h) (c * e + d * g) (c * f + d * h)

fib4 :: Integer -> Integer
fib4 n = let (M _ fib _ _) = (M 1 1 1 0) ^ n in fib
