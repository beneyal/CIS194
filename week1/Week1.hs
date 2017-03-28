module Week1 where

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = n `mod` 10 : (toDigitsRev $ n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . loop . reverse
  where loop [] = []
        loop [x] = [x]
        loop (x:y:xs) = x : (2 * y) : loop xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate = (0 ==) . (flip rem 10) . sumDigits . doubleEveryOther . toDigits

