module MyTypes.MyMaybe (MyMaybe(..)) where

data MyMaybe a = MyJust a | MyNothing deriving (Show, Eq)

instance Semigroup a => Semigroup (MyMaybe a) where
    (<>) (MyJust x) MyNothing = MyJust x
    (<>) MyNothing (MyJust x)  = MyJust x
    (<>) MyNothing MyNothing = MyNothing
    (<>) (MyJust x) (MyJust y) =  MyJust (x <> y)

instance Monoid a => Monoid (MyMaybe a) where
  mempty = MyNothing

instance Functor MyMaybe where
    fmap _ MyNothing = MyNothing
    fmap f (MyJust x) = MyJust (f x)

instance Foldable MyMaybe where
    foldr _ z MyNothing = z
    foldr f z (MyJust x) = f x z

instance Applicative MyMaybe where
    pure = MyJust

    MyNothing <*> _ = MyNothing
    (MyJust g) <*> x = fmap g x

----------------------------------------------
{-
    Выполнено с использованием DeepSeek-V3

    Промт:
    Я изучаю программирование на Haskell.  Я реализовал тип MyMaybe, аналогичный стандартному Maybe и  сделал этот тип представителем классов типов Foldable, Semigroup, Monoid, Functor и Applicative. 
    Придумай примеры вызовов соответствующих функций из ghci, чтобы проверить, что реализация классов типов выполнена верно.
    Функции, которые необходимо проверить:
    - fold, foldMap, foldr
    - (<>), sconcat, stimes
    - mappend, mconcat
    - fmap, (<$)
    - pure, (<*>), liftA2, (*>), (<*)
-}

{-
    Примеры вызовов функций и результатов работы из ghci:
    - Foldable: 
        ghci> fold (MyJust (Sum 5))   
        Sum {getSum = 5}

        ghci> fold (MyNothing :: MyMaybe (Sum Int)) 
        Sum {getSum = 0}
        
        ghci> foldMap show (MyJust 5) 
        "5"
        
        ghci> foldMap show MyNothing  
        ""
        
        ghci> foldr (+) 0 (MyJust 5)   
        5
        
        ghci> foldr (+) 0 MyNothing 
        0
    
    - Semigroup:
        ghci> MyJust "Hello" <> MyJust "World" 
        MyJust "HelloWorld"

        ghci> MyJust "Hello" <> MyNothing  
        MyJust "Hello"
        
        ghci> MyNothing <> MyJust "World"
        MyJust "World"
        
        ghci> MyNothing <> MyNothing
        MyNothing

        ghci> sconcat (MyJust "a" :| [MyJust "b", MyJust "c"]) 
        MyJust "abc"
        
        ghci> sconcat (MyJust "a" :| [])  
        MyJust "a"
        
        ghci> sconcat (MyNothing :| [MyJust "b"])   
        MyJust "b"

        ghci> stimes 3 (MyJust "a") 
        MyJust "aaa"

        ghci> stimes 3 MyNothing 
        MyNothing

    - Monoid:
        ghci> mappend (MyJust "Hello") (MyJust "World") 
        MyJust "HelloWorld"

        ghci> mconcat [MyJust "a", MyJust "b"] 
        MyJust "ab"

        ghci> mconcat [] :: MyMaybe String 
        MyNothing

    - Functor:
        ghci> fmap (+1) (MyJust 5) 
        MyJust 6

        ghci> fmap (+1) MyNothing 
        MyNothing
        
        ghci> (1 <$) (MyJust 5) 
        MyJust 1
        
        ghci> (1 <$) MyNothing   
        MyNothing

    - Applicative:
        ghci> pure 5 :: MyMaybe Int  
        MyJust 5
        
        ghci> MyJust (+1) <*> MyJust 5      
        MyJust 6
        
        ghci> MyJust (+1) <*> MyNothing       
        MyNothing
        
        ghci> MyNothing <*> MyJust 5  
        MyNothing
        
        ghci> liftA2 (+) (MyJust 3) (MyJust 4) 
        MyJust 7
        
        ghci> liftA2 (+) MyNothing (MyJust 4) 
        MyNothing
        
        ghci> liftA2 (+) (MyJust 3) MyNothing 
        MyNothing
        
        ghci> MyJust 3 *> MyJust 4   
        MyJust 4
        
        ghci> MyJust 3 *> MyNothing 
        MyNothing
        
        ghci> MyNothing *> MyJust 4 
        MyNothing
        
        ghci> MyJust 3 <* MyJust 4  
        MyJust 3
        
        ghci> MyJust 3 <* MyNothing  
        MyNothing
        
        ghci> MyNothing <* MyJust 4  
        MyNothing 
-}
----------------------------------------------