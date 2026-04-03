-----------------------------------
{-
    Выполнено с использованием DeepSeek-V3

    Промт:
    мне нужно создать два модуля и в каждом из них реализовать функционал повторяющий код ниже, но с использованием библиотек Parsec и Attoparsec.
    module MyParsers.MyParser where

    import MyTypes.MyMaybe (MyMaybe(..))
    import Data.Char (isLower, isUpper, digitToInt, isDigit)

    newtype Parser tok a = Parser {runParser :: [tok] -> MyMaybe ([tok], a) }

    charA :: Parser Char Char
    charA = Parser f where
        f (x:xs) | x == 'A' = MyJust (xs, x)
        f _                 = MyNothing

    satisfy :: (tok -> Bool) -> Parser tok tok
    satisfy p = Parser f where
        f (x:xs) | p x = MyJust (xs, x)
        f _            = MyNothing

    char :: Char -> Parser Char Char
    char c = Parser f where
        f (x:xs) | x == c = MyJust (xs, x)
        f _               = MyNothing

    lower :: Parser Char Char
    lower = Parser f where
        f (x:xs) | isLower x = MyJust (xs, x)
        f _                  = MyNothing

    instance Functor (Parser tok) where
        -- fmap :: (a -> b) -> Parser tok a -> Parser tok b
        fmap g (Parser u) = Parser f where
            f xs = case u xs of
                MyNothing -> MyNothing
                MyJust (toks', x) -> MyJust (toks', g x)

    digit :: Parser Char Int
    digit = digitToInt <$> satisfy isDigit

    instance Applicative (Parser tok) where
        -- pure :: a -> Parser tok a
        pure x = Parser $ \xs -> MyJust (xs, x)
        
        -- (<*>) :: Parser tok (a -> b) -> Parser tok a -> Parser tok b
        Parser u <*> Parser v = Parser f where
            f xs = case u xs of
                MyNothing -> MyNothing
                MyJust (tok, x) -> case v tok of
                    MyNothing -> MyNothing
                    MyJust (tok', x') -> MyJust (tok', x x') 

    multiplication :: Parser Char Int
    multiplication = (*) <$> digit <* char '*' <*> digit

    instance Alternative (Parser tok) where
        -- empty :: Parser tok a
        empty = Parser $ \_ -> MyNothing 

        -- (<|>) :: Parser tok a -> Parser tok a -> Parser tok a
        Parser u <|> Parser v = Parser f where
            f xs = case u xs of
                MyNothing -> v xs
                x -> x

    lowers :: Parser Char String
    lowers = (:) <$> lower <*> lowers <|> pure ""

    digits :: Parser Char Int
    digits = fmap (foldl (\x y -> x * 10 + y) 0) $ some digit

    finalMult :: Parser Char Int
    finalMult = (*) <$> digits <* char '*' <*> digits

    finalPlus :: Parser Char Int
    finalPlus = (+) <$> digits <* char '+' <*> digits

    plusOrMult :: Parser Char Int
    plusOrMult = finalMult <|> finalPlus
-}
-----------------------------------

module MyParsers.MyAttoparsecParser (runParser, plusOrMultAttoparsec) where

import Data.Attoparsec.Text
import Data.Text (pack, unpack)
import Data.Char (isLower, isDigit, digitToInt)
import Control.Applicative ((<$>), (<*>), (*>), (<*), many, some, (<|>))
import MyTypes.MyMaybe (MyMaybe(..))

charA :: Parser Char
charA = char 'A'

mySatisfy :: (Char -> Bool) -> Parser Char
mySatisfy = satisfy

myChar :: Char -> Parser Char
myChar = char

myLower :: Parser Char
myLower = satisfy isLower

myDigit :: Parser Int
myDigit = digitToInt <$> satisfy isDigit

multiplication :: Parser Int
multiplication = (*) <$> myDigit <* char '*' <*> myDigit

lowers :: Parser String
lowers = many myLower

digits :: Parser Int
digits = foldl (\x y -> x*10 + y) 0 <$> some myDigit

finalMult :: Parser Int
finalMult = (*) <$> digits <* char '*' <*> digits

finalPlus :: Parser Int
finalPlus = (+) <$> digits <* char '+' <*> digits

plusOrMult :: Parser Int
plusOrMult = finalMult <|> finalPlus

plusOrMultAttoparsec :: Parser Int
plusOrMultAttoparsec = plusOrMult

runParser :: Parser a -> String -> MyMaybe (String, a)
runParser p input = case parse (do x <- p; rest <- takeText; return (unpack rest, x)) (pack input) of
    Fail {} -> MyNothing
    Done _ (rest, x) -> MyJust (rest, x)