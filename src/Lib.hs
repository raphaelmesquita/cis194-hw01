module Lib
    ( someFunc
      ,toDigits
      ,toDigitsRev
      ,doubleEveryOtherRev
    ) where

someFunc :: IO ()
someFunc = putStrLn "Hello World!"

toDigits :: Integer -> [Integer]
toDigits n  
  | n <= 0    = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev = reverse . doubleEveryOther . reverse 
  where doubleEveryOther = zipWith (*) $ cycle [1,2]