----------------------------------------------
{-
    Выполнено с использованием DeepSeek-V3

    Промт:
    мне нужно импортировать модули MyMaybe, MyEither и MyTree в main. приведи пример реализации main функции, чтобы не было imports без использования
-}

module Main (main) where

import MyTypes.MyMaybe
import MyTypes.MyEither
import MyTypes.MyTree

main :: IO ()
main = do
    let maybeExample = MyJust "Hello" <> MyJust "World" :: MyMaybe String
    print maybeExample

    let eitherExample = (MyRight (+10) :: MyEither String (Int -> Int)) <*> MyRight 5
    print eitherExample

    let tree = Node 1 (Leaf 2) (Leaf 3) :: MyTree Int
    let tree2 = fmap (+10) tree
    print tree2

    putStrLn "Модули MyMaybe, MyEither, MyTree успешно импортированы и использованы."

----------------------------------------------