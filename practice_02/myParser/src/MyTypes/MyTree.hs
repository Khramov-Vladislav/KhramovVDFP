import Data.Semigroup
import Data.List.NonEmpty
import Data.Monoid
import Data.Coerce
import Data.Functor
import Data.Foldable 
import Control.Applicative 

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

----------------------------------------------
