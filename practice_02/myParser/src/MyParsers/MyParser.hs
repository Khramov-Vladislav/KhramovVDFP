module MyParsers.MyParser (runParser, plusOrMult) where

import MyTypes.MyMaybe (MyMaybe(..))
import Data.Char (isDigit, digitToInt)
import Control.Applicative (Alternative(..), some)

newtype Parser tok a = Parser {runParser :: [tok] -> MyMaybe ([tok], a) }

satisfy :: (tok -> Bool) -> Parser tok tok
satisfy p = Parser $ \xs -> case xs of
    (x:xs') | p x -> MyJust (xs', x)
    _             -> MyNothing

char :: Char -> Parser Char Char
char c = satisfy (== c)

digit :: Parser Char Int
digit = digitToInt <$> satisfy isDigit

instance Functor (Parser tok) where
    fmap g (Parser u) = Parser $ \xs -> case u xs of
        MyNothing       -> MyNothing
        MyJust (ys, x)  -> MyJust (ys, g x)

instance Applicative (Parser tok) where
    pure x = Parser $ \xs -> MyJust (xs, x)
    Parser u <*> Parser v = Parser $ \xs -> case u xs of
        MyNothing       -> MyNothing
        MyJust (ys, f)  -> case v ys of
            MyNothing       -> MyNothing
            MyJust (zs, a)  -> MyJust (zs, f a)

instance Alternative (Parser tok) where
    empty = Parser $ \_ -> MyNothing
    Parser u <|> Parser v = Parser $ \xs -> case u xs of
        MyNothing -> v xs
        res       -> res

digits :: Parser Char Int
digits = foldl (\x y -> x * 10 + y) 0 <$> some digit

finalMult :: Parser Char Int
finalMult = (*) <$> digits <* char '*' <*> digits

finalPlus :: Parser Char Int
finalPlus = (+) <$> digits <* char '+' <*> digits

plusOrMult :: Parser Char Int
plusOrMult = finalMult <|> finalPlus