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

-- Торты
data Cake = Cake CakeType 
    deriving Show

data CakeType = Cheesecake | Strawberrycake | Carrotcake 
    deriving Show
data Ingredients = Oil | Egg | Flour | Sugar | BakingPowder | Curd | Cream | CreamCheese | Strawberry | Carrot | Walnut 
    deriving (Show, Eq)
data Ingredient = Ingredient Ingredients Int 
    deriving Show

----------------------------------------
{-
    реализовано с испольщованием DeepSeek-V3
    Промт: 
    у меня есть некоторые типы, описывающие несколько типов тортов и ингредиенты для них.
    относительно указанных ингредиентов в типе Ingredients напиши функцию-рецепт для каждого торта 

    data Cake = Cake CakeType deriving Show
    data CakeType = ChIngredientseesecake | Strawberrycake | Carrotcake deriving Show
    data  = Oil | Egg | Flour | Sugar | BakingPowder | Curd | Cream | CreamCheese | Strawberry | Carrot | Walnut deriving (Eq)
    data Ingredient = Ingredient Ingredients Int

    recipe :: CakeType -> [Ingredient]
-}

recipe :: CakeType -> [Ingredient]
recipe Cheesecake = [Ingredient CreamCheese 500, Ingredient Sugar 150, Ingredient Egg 3, Ingredient Flour 50, Ingredient Cream 200]
recipe Strawberrycake = [Ingredient Flour 250, Ingredient Sugar 200, Ingredient Egg 4, Ingredient Oil 150, Ingredient BakingPowder 10, Ingredient Strawberry 400, Ingredient Cream 300]
recipe Carrotcake = [Ingredient Flour 300, Ingredient Sugar 250, Ingredient Egg 3, Ingredient Oil 200, Ingredient BakingPowder 15, Ingredient Carrot 400, Ingredient Walnut 150, Ingredient CreamCheese 250]
----------------------------------------

enough :: Ingredient -> [Ingredient] -> Bool
enough _ [] = False
enough (Ingredient ing1 q1) (Ingredient ing2 q2 : xs) = 
    if ing1 == ing2 && q1 <= q2
    then True
    else enough (Ingredient ing1 q1) xs

canMakeCake :: [Ingredient] -> [Ingredient] -> Bool
canMakeCake [] ing = True
canMakeCake need [] = False
canMakeCake (x : need) ing = 
    if enough x ing
    then canMakeCake need ing
    else False

----------------------------------------
{-
    Реализовано с использованием DeepSeek-V3
    Промт: 
    нужно реализовать функцию уменьшения количества ингредиентов 
    useIngredients :: [Ingredient] -> [Ingredient] -> [Ingredient]

    уже реализованы функции проверки наличия ингредиента 
    enough :: Ingredient -> [Ingredient] -> Bool
    enough _ [] = False
    enough (Ingredient ing1 q1) (Ingredient ing2 q2 : xs) = 
        if ing1 == ing2 && q1 <= q2
        then True
        else enough (Ingredient ing1 q1) xs

    и функция проверки возможности создания торта 
    canMakeCake :: [Ingredient] -> [Ingredient] -> Bool
    canMakeCake [] ing = True
    canMakeCake need [] = False
    canMakeCake (x : need) ing = 
        if enough x ing
        then canMakeCake need ing
        else False
-}

useIngredients :: [Ingredient] -> [Ingredient] -> [Ingredient]
useIngredients [] ingredient = ingredient
useIngredients (x : need) ingredient 
    | enough x ingredient = useIngredients need updateIngredients
    | otherwise = ingredient
        where 
            updateIngredients = map decrease ingredient
            decrease (Ingredient ing n) 
                | ing == ingX = Ingredient ing (n - nX)
                | otherwise = Ingredient ing n
            Ingredient ingX nX = x
----------------------------------------

bakeCake :: CakeType -> [Ingredient] -> Maybe (Cake, [Ingredient])
bakeCake cakeType stock
    | canMakeCake need stock = Just (Cake cakeType, useIngredients need stock)
    | otherwise = Nothing
  where
    need = recipe cakeType

----------------------------------------
{-  Реализовано с использованием DeepSeek-V3
    Промт: 
    напиши пример для проверки создания торта
-}

stock :: [Ingredient]
stock = [Ingredient CreamCheese 600, Ingredient Sugar 300, Ingredient Egg 6, Ingredient Flour 300, Ingredient Cream 300, Ingredient Oil 500, Ingredient BakingPowder 20, Ingredient Strawberry 500, Ingredient Carrot 400, Ingredient Walnut 200]
----------------------------------------
