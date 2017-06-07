module Lib
    ( someFunc
      ,toDigits
      ,toDigitsRev
      ,doubleEveryOther
      ,sumDigits
      ,validate
    ) where

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
validate num = finalSum `mod` 10 == 0
  where finalSum = sumDigits . doubleEveryOther . toDigitsRev $ num