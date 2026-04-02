module MyTypes.MyEither (MyEither(..)) where

data MyEither a b = MyLeft a | MyRight b deriving(Show, Eq)

instance Semigroup b => Semigroup (MyEither a b) where
    (<>) (MyLeft x) (MyLeft _) = MyLeft x
    (<>) (MyLeft x) (MyRight _) = MyLeft x
    (<>) (MyRight x) (MyLeft _) = MyRight x
    (<>) (MyRight x) (MyRight y) = MyRight (x <> y)

instance Functor (MyEither e) where
    fmap _ (MyLeft x) = MyLeft x
    fmap g (MyRight y) = MyRight (g y)

instance Foldable (MyEither a) where
    foldMap _ (MyLeft _) = mempty
    foldMap f (MyRight y) = f y

    foldr _ z (MyLeft _) = z
    foldr f z (MyRight y) = f y z
    
instance Applicative (MyEither a) where
    pure = MyRight 
    
    (MyLeft x) <*> (MyLeft _) = MyLeft x
    (MyLeft x) <*> (MyRight _) = MyLeft x
    (MyRight _) <*> (MyLeft y) = MyLeft y
    (MyRight f) <*> (MyRight y) = MyRight (f y)

----------------------------------------------
{-
    Выполнено с использованием DeepSeek-V3

    Промт:
    Я реализовал тип MyEither, аналогичный стандартному Either и  сделал этот тип представителем классов типов Foldable, Semigroup, Functor и Applicative. 
    Придумай примеры вызовов соответствующих функций из ghci, чтобы проверить, что реализация классов типов выполнена верно.
    Функции, которые необходимо проверить:
    - fold, foldMap, foldr
    - (<>), sconcat, stimes
    - fmap, (<$)
    - pure, (<*>), liftA2, (*>), (<*)
-}

{-
    Примеры вызовов функций и результатов работы из ghci:
    - Foldable: 
        ghci> fold (MyRight (Sum 5)) 
        Sum {getSum = 5}
        
        ghci> fold (MyLeft "error")   
        ()
        
        ghci> fold (MyRight "hello")  
        "hello"
        
        ghci> foldMap show (MyRight 42)
        "42"
        
        ghci> foldMap show (MyLeft "err") 
        ""
        
        ghci> foldr (+) 0 (MyRight 5) 
        5
        
        ghci> foldr (+) 0 (MyLeft "err")
        0
    
    - Semigroup:
        ghci> MyRight "Hello" <> MyRight "World" 
        MyRight "HelloWorld"
        
        ghci> MyRight "Hello" <> MyLeft "error"
        MyRight "Hello"
        
        ghci> MyLeft "error1" <> MyRight "World" 
        MyLeft "error1"
        
        ghci> MyLeft "error1" <> MyLeft "error2" 
        MyLeft "error1"
        
        ghci> MyRight (Sum 3) <> MyRight (Sum 4)  
        MyRight (Sum {getSum = 7})
        
        ghci> MyLeft (Sum 1) <> MyRight (Sum 2)   
        MyLeft (Sum {getSum = 1})
        
        ghci> sconcat (MyRight "a" :| [MyRight "b", MyRight "c"]) 
        MyRight "abc"
        
        ghci> sconcat (MyRight "a" :| [])     
        MyRight "a"
        
        ghci> sconcat (MyLeft "err" :| [MyRight "b"]) 
        MyLeft "err"
        
        ghci> sconcat (MyLeft "e1" :| [MyLeft "e2"])  
        MyLeft "e1"
        
        ghci> stimes 3 (MyRight "a")   
        MyRight "aaa"
        
        ghci> stimes 3 (MyLeft "x")   
        MyLeft "x"
        
        ghci> stimes 3 (MyRight (Sum 2)) 
        MyRight (Sum {getSum = 6})

    - Functor:
        ghci> fmap (+1) (MyRight 5)  
        MyRight 6
        
        ghci> fmap (+1) (MyLeft "err") 
        MyLeft "err"
        
        ghci> (1 <$) (MyRight 5) 
        MyRight 1
        
        ghci> (1 <$) (MyLeft "err") 
        MyLeft "err"

    - Applicative:
        ghci> pure 5 :: MyEither String Int 
        MyRight 5
        
        ghci> MyRight (+1) <*> MyRight 5 
        MyRight 6
        
        ghci> MyRight (+1) <*> MyLeft "err"
        MyLeft "err"
        
        ghci> MyLeft "err" <*> MyRight 5
        MyLeft "err"
        
        ghci> MyLeft "e1" <*> MyLeft "e2"
        MyLeft "e1"
        
        ghci> liftA2 (+) (MyRight 3) (MyRight 4) 
        MyRight 7
        
        ghci> liftA2 (+) (MyLeft "e1") (MyRight 4)
        MyLeft "e1"
        
        ghci> liftA2 (+) (MyRight 3) (MyLeft "e2")
        MyLeft "e2"
        
        ghci> MyRight 3 *> MyRight 4  
        MyRight 4
        
        ghci> MyRight 3 *> MyLeft "err" 
        MyLeft "err"
        
        ghci> MyLeft "err" *> MyRight 4   
        MyLeft "err"
        
        ghci> MyRight 3 <* MyRight 4 
        MyRight 3
        
        ghci> MyRight 3 <* MyLeft "err"  
        MyLeft "err"
        
        ghci> MyLeft "err" <* MyRight 4 
        MyLeft "err"        
-}
----------------------------------------------