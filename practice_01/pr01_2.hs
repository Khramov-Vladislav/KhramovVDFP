-------------------------------------
-- Практические задание 1. Часть 2 --
-------------------------------------

module Pr01_2 where

{-

Напишите реализацию функций:
-- myZipSave - попарное объединение двух списков в список пар и сохранение хвоста более длинного списка 
-- myUnzipSave - разделение списка пар на пару списков с восстановлением более длинного списка если исходные списки были разного размера
-- myReverse - разворот списка с использованием сверток
-- myFoldl1 - левая свертка для не пустых списков (без инициирующего значения)
-- myFoldr1 - правая свертка для не пустых списков (без инициирующего значения)
-- myTakeWhile - реализовать с использованием сверток
-- mySpan - реализовать с использованием сверток
-- myMaybe - обработка возможно отсутствующего значения или возвращение значение по умолчанию (maybe)
-- myMap - реализуйте функцию map с использованием типа MyList из материалов лекции
-- myUnFoldr - развертка (операция обратная к свертке)

-- Расширьте типы для выпекания тортов из материалов лекции:
    -- Добавить возможность испечь не менее трех типов тортов
    -- Контроль числа и объема используемых ингредиентов
    -- Обработку недостатка или отсутствия ингредиентов

-}

myZipSave :: [a] -> [b] -> ([(a, b)], [a], [b])
myZipSave [] ys = ([], [], ys)
myZipSave xs [] = ([], xs, [])
myZipSave (x:xs) (y:ys) = ((x,y) : pairs, tailA, tailB)
    where (pairs, tailA, tailB) = myZipSave xs ys

-----------------------------------------------
-- сделано с использованием GPT‑5 mini.

myUnzipSave :: ([(a, b)], [a], [b]) -> ([a], [b]) 
myUnzipSave ([], [], ys) = ([], ys)
myUnzipSave ([], xs, []) = (xs, [])
myUnzipSave ((x, y):pairs, xs, ys) = (x : as, y : bs)
    where (as, bs) = myUnzipSave (pairs, xs, ys)
-----------------------------------------------

myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = foldl f [] xs 
    where f acc x =  x : acc

myFoldl1 :: (a -> a -> a) -> [a] -> a
myFoldl1 _ [x] = x
myFoldl1 f (x : y : xs) = myFoldl1 f (f x y : xs)

myFoldr1 :: (a -> a -> a) -> [a] -> a
myFoldr1 _ [x] = x
myFoldr1 f (x : xs) = f x (myFoldr1 f xs)

myTakeWhile' :: (a -> Bool) -> [a] -> [a]
myTakeWhile' _ [] = []
myTakeWhile' f (x : xs) 
    | f x = x : myTakeWhile' f xs
    | otherwise = []

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f = foldr step []
  where
    step x acc
      | f x = x : acc
      | otherwise = []

mySpan' :: (a -> Bool) -> [a] -> ([a], [a])
mySpan' _ [] = ([], [])
mySpan' f (x : xs) = (head, tail)
    where  
        (head', tail') = mySpan' f xs
        (head, tail) 
            | f x = (x : head', tail')
            | otherwise = ([], x : xs)

-----------------------------------------------
-- сделано с использованием GPT‑5 mini.

mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan f = foldr step ([], [])
    where 
        step x (head, tail)
            | f x = (x : head, tail)
            | otherwise = ([], x : head ++ tail)
-----------------------------------------------

myMaybe :: b -> (a -> b) -> Maybe a -> b
myMaybe defVal f Nothing = defVal
myMaybe defVal f (Just x) = f x

data MyList a = MyEmpty | MyCons a (MyList a)

myMap :: (a -> b) -> MyList a -> MyList b
myMap _ MyEmpty = MyEmpty
myMap f (MyCons x xs) = MyCons (f x) (myMap f xs)

-----------------------------------------------
-- сделано с использованием GPT‑5 mini.

myUnFoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnFoldr f start =
    case f seed of
        Nothing -> []
        Just (x, newSeed) -> x : myUnFoldr f newSeed
-----------------------------------------------
