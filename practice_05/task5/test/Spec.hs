{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Test.QuickCheck
import Lib (addMod, reverseWords)

-- ------------------------------------------------------------
-- Генераторы для слов и предложений
-- ------------------------------------------------------------
genWord :: Gen String
genWord = listOf1 $ elements (['a'..'z'] ++ ['A'..'Z'])

genSentence :: Gen String
genSentence = do
  n <- choose (0, 10)
  ws <- vectorOf n genWord
  return $ unwords ws

-- ------------------------------------------------------------
-- Свойства для addMod
-- ------------------------------------------------------------
prop_addMod_mod :: Int -> Int -> Int -> Property
prop_addMod_mod x y m = m > 0 ==> addMod x y m === (x + y) `mod` m

prop_addMod_neutral :: Int -> Int -> Property
prop_addMod_neutral x m = m > 0 ==> addMod x 0 m === x `mod` m

prop_addMod_commutative :: Int -> Int -> Int -> Property
prop_addMod_commutative x y m = m > 0 ==> addMod x y m === addMod y x m

-- ------------------------------------------------------------
-- Свойства для reverseWords
-- ------------------------------------------------------------
prop_reverseWords_empty :: Property
prop_reverseWords_empty = reverseWords "" === ""

prop_reverseWords_single :: Property
prop_reverseWords_single = forAll genWord $ \w -> reverseWords w === w

prop_reverseWords_multiple :: Property
prop_reverseWords_multiple = forAll (suchThat genSentence ((>1) . length . words)) $ \s ->
  let ws = words s in reverseWords s === unwords (reverse ws)

prop_reverseWords_involution :: Property
prop_reverseWords_involution = forAll genSentence $ \s ->
  reverseWords (reverseWords s) === s

-- ------------------------------------------------------------
-- Запуск тестов
-- ------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "Testing addMod..."
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_addMod_mod
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_addMod_neutral
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_addMod_commutative

  putStrLn "\nTesting reverseWords..."
  quickCheck prop_reverseWords_empty
  quickCheckWith stdArgs { maxSuccess = 500 } prop_reverseWords_single
  quickCheckWith stdArgs { maxSuccess = 500 } prop_reverseWords_multiple
  quickCheckWith stdArgs { maxSuccess = 500 } prop_reverseWords_involution