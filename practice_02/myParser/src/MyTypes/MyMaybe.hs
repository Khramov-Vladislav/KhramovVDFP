-- тип MyMaybe, аналогичный стандартному Maybe. Сделайте этот тип представителем классов типов Foldable, Semigroup, Monoid, Functor и Applicative (*)

import qualified Data.Semigroup
import qualified Data.Monoid 
import qualified Data.Functor 
import qualified Data.Foldable
import qualified Control.Applicative

data MyMaybe a = MyJust a | MyNothing deriving (Show, Eq)

instance Semigroup a => Semigroup (MyMaybe a) where
    (<>) (MyJust x) MyNothing = MyJust x
    (<>) MyNothing (MyJust x)  = MyJust x
    (<>) MyNothing MyNothing = MyNothing
    (<>) (MyJust x) (MyJust y) =  MyJust (x <> y)

instance Monoid a => Monoid (MyMaybe a) where
  mempty = MyNothing

instance Functor MyMaybe where
    fmap f MyNothing = MyNothing
    fmap f (MyJust x) = MyJust (f x)

instance Foldable MyMaybe where
    foldr _ z MyNothing = z
    foldr f z (MyJust x) = f x z

instance Applicative MyMaybe where
    pure = MyJust
    
    MyNothing <*> _ = MyNothing
    (MyJust g) <*> x = fmap g x