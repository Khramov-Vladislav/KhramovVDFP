module Lib (addMod, reverseWords) where

-- | Сложение по модулю: (x + y) `mod` m
addMod :: Int -> Int -> Int -> Int
addMod x y m = (x + y) `mod` m

-- | Переворачивает порядок слов (слова разделены пробелами)
reverseWords :: String -> String
reverseWords = unwords . reverse . words