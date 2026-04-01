-- тип MyMaybe, аналогичный стандартному Maybe. Сделайте этот тип представителем классов типов Foldable, Semigroup, Monoid, Functor и Applicative (*)
import qualified Data.Semigroup

data MyMaybe a = MyJust a | MyNothing deriving (Show, Eq)

instance Semigroup a => Semigroup (MyMaybe a) where
    (<>) (MyJust x) MyNothing = MyJust x
    (<>) MyNothing (MyJust x)  = MyJust x
    (<>) MyNothing MyNothing = MyNothing
    (<>) (MyJust x) (MyJust y) =  MyJust (x <> y)

instance Monoid a => Monoid (MyMaybe a) where
  mempty = MyNothing