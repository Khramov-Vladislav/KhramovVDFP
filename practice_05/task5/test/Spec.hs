{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.QuickCheck
import MyLib (addMod, reverseWords)

-- ------------------------------------------------------------
-- Генераторы для «содержательных» данных
-- ------------------------------------------------------------

-- Модуль должен быть положительным
genPositive :: Gen Int
genPositive = getPositive <$> arbitrary

-- Генератор для (x, y, m) с m > 0
instance Arbitrary (Int, Int, Int) where
  arbitrary = do
    m <- genPositive
    x <- choose (0, m-1)  -- чтобы тестировать в пределах модуля
    y <- choose (0, m-1)
    return (x, y, m)
  shrink (x, y, m) = [(x', y', m) | x' <- shrink x, y' <- shrink y, x' >=0, y' >=0, m>0]

-- Генератор для строк с несколькими словами (без лишних пробелов)
genWord :: Gen String
genWord = listOf1 $ elements (['a'..'z'] ++ ['A'..'Z'])

genSentence :: Gen String
genSentence = do
  wordsCount <- choose (0, 10)
  words <- vectorOf wordsCount genWord
  return $ unwords words

instance Arbitrary String where
  arbitrary = genSentence
  shrink s = [ unwords (drop i ws) | i <- [1..length ws] ] ++
             [ unwords (take i ws) | i <- [0..length ws-1] ]
    where ws = words s

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

prop_reverseWords_single :: String -> Property
prop_reverseWords_single s = not (any (==' ') s) && s /= "" ==> reverseWords s === s

prop_reverseWords_multiple :: String -> Property
prop_reverseWords_multiple s = length (words s) > 1 ==> 
  let ws = words s
  in reverseWords s === unwords (reverse ws)

prop_reverseWords_involution :: String -> Property
prop_reverseWords_involution s = reverseWords (reverseWords s) === s

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