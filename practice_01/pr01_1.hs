-------------------------------------
-- Практические задание 1. Часть 1 --
-------------------------------------

module Pr01_1 where

{-

Напишите реализацию функций myFST, mySND, myTHRD для кортежа (a,b,c)

Напишите реализацию стандартных функции для работы со списками:
-- myHead - определение (через сопоставление с образцом) функции отделения головы списка
-- myTail - функция отделения хвоста списка
-- myTake - взять первые n элементов списка
-- myDrop - отбросить первые n элементов списка
-- myProduct - перемножить все элементы списка
-- myZip - попарное объединение двух списков в список пар, длина итогового списка по длине самого короткого из входных списков
-- myZip3 объединение трех списков в список троек
-- myUnzip - разделение списка пар на пару списков (две реализации myUNzip)

Напишите реализацию стандартных функции высшего порядка для работы со списками:
-- myFilter - применение предиката к каждому элементу списка (две реализации: с использованием охранных выражений и if-then-else)
-- myMap - применение функции одного аргумента к каждому элементу списка
-- myZipWith - применение функции двух аргументов к двум спискам
-- myZipWith3 - применение функции трех аргументов к трем спискам
-- myAll - проверяет удовлетворяют ли все элементы списка предикату
-- myAny - проверяет удовлетворяют ли хотя бы один элемент списка предикату
-- myComposition - композиция двух функций (.)

-}

myFST :: (a, b) -> a
myFST (a, b) = a

mySND :: (a, b) -> b
mySND (a, b) = b

myTHRD :: (a, b, c) -> c
myTHRD (a, b, c) = c

myHead :: [a] -> a
myHead [] = error "empty list"
myHead (x:xs) = x

myTail :: [a] -> [a]
myTail [] = error "empty list"
myTail (x : xs) = xs

myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake n (x : xs) = 
    if n > 0 
    then x : myTake (n - 1) xs
    else []

myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop n (x : xs) = 
    if n > 0
    then myDrop (n - 1) xs
    else x : xs

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (x : xs) = x * myProduct xs

myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

myZip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
myZip3 [] _ _ = []
myZip3 _ [] _ = []
myZip3 _ _ [] = []
myZip3 (x : xs) (y : ys) (z : zs) = (x, y, z) : myZip3 xs ys zs

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip ((a, b) : xs) = 
    (a : listA, b : listB)
    where 
        (listA, listB) = myUnzip xs

myUNzip' :: [(a, b)] -> ([a], [b])
myUNzip' [] = ([], [])
myUnzip' xs = (map fst xs, map snd xs)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter pred (x : xs) 
    | pred x = x : myFilter pred xs
    | otherwise = myFilter pred xs

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' _ [] = []
myFilter' pred (x : xs) = 
    if pred x
    then x : myFilter' pred xs
    else myFilter' pred xs

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap func (x : xs) = func x : myMap func xs

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith func (x : xs) (y : ys) = func x y : myZipWith func xs ys

myZipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
myZipWith3 _ [] _ _ = []
myZipWith3 _ _ [] _ = []
myZipWith3 _ _ _ [] = []
myZipWith3 func (x : xs) (y : ys) (z : zs) = func x y z : myZipWith3 func xs ys zs

myAll :: (a -> Bool) -> [a] -> Bool
myAll _ [] = True
myAll pred (x : xs) = 
    if pred x
    then myAll pred xs
    else False

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny pred (x : xs) = 
    if pred x
    then True
    else myAny pred xs
    
myComposition :: (b -> c) -> (a -> b) -> (a -> c)
myComposition funcF funcG x  = funcF (funcG x) 
