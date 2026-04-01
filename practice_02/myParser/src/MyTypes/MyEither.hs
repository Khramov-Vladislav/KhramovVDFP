import Data.Semigroup
import Data.List.NonEmpty
import Data.Monoid
import Data.Coerce
import Data.Functor
import Data.Foldable 
import Control.Applicative 

data MyEither a b = MyLeft a | MyRight b deriving(Show, Eq)

instance Semigroup b => Semigroup (MyEither a b) where
    (<>) (MyLeft x) (MyLeft y) = MyLeft x
    (<>) (MyLeft x) (MyRight y) = MyLeft x
    (<>) (MyRight x) (MyLeft y) = MyRight x
    (<>) (MyRight x) (MyRight y) = MyRight (x <> y)

instance Functor (MyEither e) where
    fmap :: (a -> b) -> MyEither e a -> MyEither e b
    fmap _ (MyLeft x) = MyLeft x
    fmap g (MyRight y) = MyRight (g y)

instance Foldable (MyEither a) where
    foldMap _ (MyLeft _) = mempty
    foldMap f (MyRight y) = f y

    foldr _ z (MyLeft _) = z
    foldr f z (MyRight y) = f y z
    
instance Applicative (MyEither a) where
    pure = MyRight
    
    (MyLeft x) <*> (MyLeft y) = MyLeft x
    (MyLeft x) <*> (MyRight y) = MyLeft x
    (MyRight f) <*> (MyLeft y) = MyLeft y
    (MyRight f) <*> (MyRight y) = MyRight (f y)
