module Main(main) where

-- import MyTypes.MyMaybe
-- import MyTypes.MyEither
-- import MyTypes.MyTree

import qualified MyParsers.MyParser as MP
import qualified MyParsers.MyParsecParser as MPP
import qualified MyParsers.MyAttoparsecParser as MAP

----------------------------------------------
{-
    Выполнено с использованием DeepSeek-V3

    Промт:
    мне нужно импортировать модули MyMaybe, MyEither и MyTree в main. приведи пример реализации main функции, чтобы все imports использовались
-}

-- main :: IO ()
-- main = do
--     let maybeExample = MyJust "Hello" <> MyJust "World" :: MyMaybe String
--     print maybeExample

--     let eitherExample = (MyRight (+10) :: MyEither String (Int -> Int)) <*> MyRight 5
--     print eitherExample

--     let tree = Node 1 (Leaf 2) (Leaf 3) :: MyTree Int
--     let tree2 = fmap (+10) tree
--     print tree2

--     putStrLn "Модули MyMaybe, MyEither, MyTree успешно импортированы и использованы."

----------------------------------------------

main :: IO ()
main = do
    putStrLn "MyParser:"
    putStrLn $ show (MP.runParser MP.plusOrMult "12*345dsf")
    putStrLn $ show (MP.runParser MP.plusOrMult "12+345dsf")
    putStrLn "Parsec:"
    putStrLn $ show (MPP.runParser MPP.plusOrMultParsec "12*345dsf")
    putStrLn $ show (MPP.runParser MPP.plusOrMultParsec "12+345dsf")
    putStrLn "Attoparsec:"
    putStrLn $ show (MAP.runParser MAP.plusOrMultAttoparsec "12*345dsf")
    putStrLn $ show (MAP.runParser MAP.plusOrMultAttoparsec "12+345dsf")