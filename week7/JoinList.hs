{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import Data.Monoid

import Buffer
import Scrabble
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ i (Append _ left right)
  | i < sizeLeft = indexJ i left
  | otherwise = indexJ (i - sizeLeft) right
  where sizeLeft = getSize . size . tag $ left

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl | n <= 0 = jl
dropJ _ (Single _ _) = Empty
dropJ n (Append _ left right)
  | n < sizeLeft = (dropJ n left) +++ right
  | otherwise = dropJ (n - sizeLeft) right
  where sizeLeft = getSize . size . tag $ left

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n jl | n <= 0 = Empty
takeJ _ s@(Single _ _) = s
takeJ n (Append _ left right)
  | n < sizeLeft = takeJ n left
  | otherwise = left +++ (takeJ (n - sizeLeft) right)
  where sizeLeft = getSize . size . tag $ left

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ s) = s
  toString (Append _ left right) = toString left ++ toString right

  fromString = foldl (+++) Empty . map (\line -> Single (scoreString line, 1) line) . lines

  line = indexJ

  replaceLine n ln buf = takeJ n buf +++ (Single (scoreString ln, 1) ln) +++ (dropJ (n + 1) buf)

  numLines Empty = 0
  numLines (Single _ _) = 1
  numLines (Append (_, size) _ _) = getSize size

  value Empty = 0
  value (Single (score, _) _) = getScore score
  value (Append (score, _) _ _) = getScore score
