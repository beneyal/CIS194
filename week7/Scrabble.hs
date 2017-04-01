{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Char (toUpper)
import Data.Monoid

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score n) = n

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

score :: Char -> Score
score c
  | c' `elem` "AEILNORSTU" = Score 1
  | c' `elem` "DG" = Score 2
  | c' `elem` "BCMP" = Score 3
  | c' `elem` "FHVWY" = Score 4
  | c' `elem` "K" = Score 5
  | c' `elem` "JK" = Score 8
  | c' `elem` "QZ" = Score 10
  | otherwise = Score 0
  where c' = toUpper c

scoreString :: String -> Score
scoreString = foldr mappend mempty . map score
