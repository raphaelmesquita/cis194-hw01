module Lib where

someFunc :: IO ()
someFunc = putStrLn "Hello World!"

toDigits :: Integer -> [Integer]
toDigits n  
  | n <= 0    = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith (*) $ cycle [1,2]

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate cardNum = finalSum `mod` 10 == 0
  where finalSum = sumDigits . doubleEveryOther . toDigitsRev $ cardNum
-- validate = (== 0) . (`mod` 10) . runAlgorithm
--   where runAlgorithm = sumDigits . doubleEveryOther . toDigitsRev

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined
