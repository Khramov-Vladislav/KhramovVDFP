module MyTypes.MyTree (MyTree(..), t1, t2, t3) where

----------------------------------------------
{-
    Выполнено с использованием DeepSeek-V3

    Промт:
    мне нужно создать тип MyTree, который содержит значения в узлах и листьях и сделать этот тип представителем классов типов Foldable, Functor и Applicative.
-}

data MyTree a = Leaf a | Node a (MyTree a) (MyTree a) deriving (Show, Eq)

instance Functor MyTree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

instance Foldable MyTree where
    foldMap f (Leaf x) = f x
    foldMap f (Node x l r) = f x <> foldMap f l <> foldMap f r

instance Applicative MyTree where
    pure = Leaf

    Leaf f <*> Leaf x = Leaf (f x)
    Leaf f <*> Node x l r = Node (f x) (Leaf f <*> l) (Leaf f <*> r)
    Node f fl fr <*> Leaf x = Node (f x) (fl <*> Leaf x) (fr <*> Leaf x)
    Node f fl fr <*> Node x l r = Node (f x) (fl <*> l) (fr <*> r)
----------------------------------------------
{-
    Выполнено с использованием DeepSeek-V3

    Промт:
    мне нужно создать тип MyTree, который содержит значения в узлах и листьях и сделать этот тип представителем классов типов Foldable, Functor и Applicative.
    Также необходимо написать примеры  вызовов соответствующих функций
    - fold, foldMap, foldr
    - fmap, (<$)
    - pure, (<*>), liftA2, (*>), (<*)
-}

t1 :: MyTree Integer
t1 = Leaf 1

t2 :: MyTree Integer
t2 = Node 2 (Leaf 3) (Leaf 4)

t3 :: MyTree Integer
t3 = Node 5 (Node 6 (Leaf 8) (Leaf 9)) (Node 7 (Leaf 10) (Leaf 11))

{-
    Примеры вызовов функций и результатов работы из ghci:
    - Functor:
        ghci> foldMap Sum t1 
        Sum {getSum = 1}
        
        ghci> foldMap Sum t2   
        Sum {getSum = 9}
        
        ghci> foldMap Sum t3  
        Sum {getSum = 56}
        
        ghci> foldr (-) 0 t1   
        1
        
        ghci> foldr (-) 0 t2 
        3
        
        ghci> foldr (-) 0 t3   
        6
        
        ghci> fold (fmap Sum t1)fmap (*2) t2                 
        Sum {getSum = 1}
        
        ghci> fold (fmap Sum t2)    
        Sum {getSum = 9}
        
        ghci> fold (fmap Sum t3)
        Sum {getSum = 56}

    - Foldable: 
        ghci> fmap (+10) t1  
        Leaf 11
        
        ghci> fmap (*2) t2                 
        Node 4 (Leaf 6) (Leaf 8)
        
        ghci> fmap show t3  
        Node "5" (Node "6" (Leaf "8") (Leaf "9")) (Node "7" (Leaf "10") (Leaf "11"))
        
        ghci> (100 <$) t1
        Leaf 100
        
        ghci> (100 <$) t2 
        Node 100 (Leaf 100) (Leaf 100)
        
        ghci> (100 <$) t3 
        Node 100 (Node 100 (Leaf 100) (Leaf 100)) (Node 100 (Leaf 100) (Leaf 100))
    
    - Applicative:
        ghci> pure 42 :: MyTree Int
        Leaf 42
        
        ghci> let ft2 = Node (+10) (Leaf (+20)) (Leaf (+30)) 
        ghci> ft2 <*> t2    
        Node 12 (Leaf 23) (Leaf 34)
        
        ghci> liftA2 (+) t2 t2 
        Node 4 (Leaf 6) (Leaf 8)
        
        ghci> t2 *> t2   
        Node 2 (Leaf 3) (Leaf 4)
        
        ghci> t2 <* t2 
        Node 2 (Leaf 3) (Leaf 4)
-}
----------------------------------------------
