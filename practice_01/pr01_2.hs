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
myZipSave (x : xs) (y : ys) = ((x, y) : pairs, tailX, tailY) 
    where (pairs, tailX, tailY) = myZipSave xs ys

myUnzipSave :: ([(a, b)], [a], [b]) -> ([a], [b])
myUnzipSave ([], xs, ys) = (xs, ys)
myUnzipSave ((a, b) : list, xs, ys) = (a : listA, b : listB) 
    where (listA, listB) = myUnzipSave (list, xs, ys)

myUnzipSave' :: [(a, b)] -> [a] -> [b] -> ([a], [b])
myUnzipSave' [] xs ys = (xs, ys)
myUnzipSave' ((a, b) : list) xs ys = (a : listA, b : listB) 
    where (listA, listB) = myUnzipSave' list xs ys

myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = foldl func [] xs
    where func acc x  = x : acc

myFoldl1 :: (a -> a -> a) -> [a] -> a
myFoldl1 _ [x] = x
myFoldl1 func (x : y : xs) = myFoldl1 func (func x y  : xs)

myFoldr1 :: (a -> a -> a) -> [a] -> a
myFoldr1 _ [x] = x
myFoldr1 func (x : xs) = func x (myFoldr1 func xs)

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile pred xs = foldr func [] xs
  where 
    func x acc
        | pred x = x : acc
        | otherwise = []

mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan pred xs = foldr func ([], []) xs
    where func x (listA, listB)
            | pred x  = (x : listA, listB)
            | otherwise = ([], x : listA ++ listB)

myMaybe :: b -> (a -> b) -> Maybe a -> b
myMaybe def func (Just x) = func x
myMaybe def func Nothing = def

data MyList a = MyEmpty | MyCons a (MyList a)

myMap :: (a -> b) -> MyList a -> MyList b
myMap _ MyEmpty = MyEmpty
myMap func (MyCons x xs) = MyCons (func x) (myMap func xs)

myUnFoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnFoldr func state = case func state of
    Nothing -> []
    Just (x, newState) -> x : myUnFoldr func newState
    
    
-- Расширьте типы для выпекания тортов из материалов лекции:
-- Добавить возможность испечь не менее трех типов тортов
-- Контроль числа и объема используемых ингредиентов
-- Обработку недостатка или отсутствия ингредиентов

data CakeType = Cheesecake | Strawberrycake | Carrotcake 
    deriving Show
data Ingredients = Oil | Egg | Flour | Sugar | BakingPowder | Curd | Cream | CreamCheese | Strawberry | Carrot | Walnut 
    deriving (Eq, Show)
data Ingredient = Ingredient Ingredients Int
    deriving Show
data Cake = Cake CakeType 
    deriving Show

-----------------------------------------------
-- сделано с использованием GPT‑5 mini.
recipe :: CakeType -> [Ingredient]
recipe Cheesecake = [Ingredient Curd 200, Ingredient Egg 3, Ingredient Flour 100, Ingredient Sugar 150, Ingredient Cream 50]
recipe Strawberrycake = [Ingredient Flour 100, Ingredient Egg 3, Ingredient Sugar 100, Ingredient Strawberry 150, Ingredient Cream 50]
recipe Carrotcake = [Ingredient Flour 120, Ingredient Egg 3, Ingredient Sugar 100, Ingredient Carrot 200, Ingredient Walnut 50]
-----------------------------------------------

enough :: Ingredient -> [Ingredient] -> Bool
enough _ [] = False
enough (Ingredient ing1 n1) (Ingredient ing2 n2 : xs) 
    | ing1 == ing2 && n1 <= n2 = True
    | otherwise = enough (Ingredient ing1 n1) xs

canMakeCake :: [Ingredient] -> [Ingredient] -> Bool
canMakeCake [] ingredient = True
canMakeCake need [] = False
canMakeCake (x:need) ingredient
    | enough x ingredient = canMakeCake need ingredient
    | otherwise = False

-- bakeCake :: CakeType -> [Ingredient] -> Maybe Cake
-- bakeCake cakeType ingredient 
--     | canMakeCake need ingredient = Just (Cake cakeType) 
--     | otherwise = Nothing
--         where  need = recipe cakeType

-----------------------------------------------
-- сделано с использованием GPT‑5 mini.

stock :: [Ingredient]
stock =
  [ Ingredient Curd 200           -- нужно только для Cheesecake
  , Ingredient Egg 9              -- 9 яиц на все три торта
  , Ingredient Flour 320          -- 100 + 100 + 120 = 320
  , Ingredient Sugar 350          -- 150 + 100 + 100 = 350
  , Ingredient Cream 100          -- 50 + 50 + 0 = 100
  , Ingredient Strawberry 150     -- только для Strawberrycake
  , Ingredient Carrot 200         -- только для Carrotcake
  , Ingredient Walnut 50          -- только для Carrotcake
  ]

bakeCake :: CakeType -> [Ingredient] -> Maybe (Cake, [Ingredient])
bakeCake cakeType stock
    | canMakeCake need stock = Just (Cake cakeType, useIngredients need stock)
    | otherwise = Nothing
  where
    need = recipe cakeType

useIngredients :: [Ingredient] -> [Ingredient] -> [Ingredient]
useIngredients [] ingredient = ingredient
useIngredients (x:need) ingredient 
    | enough x ingredient = useIngredients need updateIngredients
    | otherwise = ingredient
        where 
            updateIngredients = map decrease ingredient
            decrease (Ingredient ing n) 
                | ing == ingX = Ingredient ing (n - nX)
                | otherwise = Ingredient ing n
            Ingredient ingX nX = x
-----------------------------------------------
