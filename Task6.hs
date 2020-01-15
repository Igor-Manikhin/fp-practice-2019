module Task6 where

{-
В этом файле приведён код, написанный (совместными усилиями) на лекции

Модифицируйте представленный тут парсер таким образом, чтобы он поддерживал унарные операции 
(отрицание и факториал), а также числа с плавающей точкой
-}

import Text.Parsec hiding(digit)
import Data.Functor



type Parser a = Parsec String () a

data Number = IntVal   Integer
            | FloatVal Double
            deriving(Show)


eval :: Parser a -> String -> Either ParseError a
eval p s = parse p "" s

digit :: Parser Char
digit = oneOf ['0'..'9']

number :: Parser Number
number = try float <|> int
    where
        int = IntVal <$> read <$> (many1 digit)
        float = do
            n1 <- many1 digit
            char '.'
            n2 <- many1 digit
            FloatVal <$> read <$> (return (n1 ++ "." ++ n2))

applyMany :: a -> [a -> a] -> a
applyMany x [] = x
applyMany x (h:t) = applyMany (h x) t

(|+|) :: Number -> Number -> Number
(|+|) (IntVal x) (IntVal y) = IntVal (x + y)
(|+|) (IntVal x) (FloatVal y) = FloatVal ((fromIntegral x) + y)
(|+|) (FloatVal x) (IntVal y) = FloatVal (x + (fromIntegral y))
(|+|) (FloatVal x) (FloatVal y) = FloatVal (x + y)

(|-|) :: Number -> Number -> Number
(|-|) (IntVal x) (IntVal y) = IntVal (x - y)
(|-|) (IntVal x) (FloatVal y) = FloatVal ((fromIntegral x) - y)
(|-|) (FloatVal x) (IntVal y) = FloatVal (x - (fromIntegral y))
(|-|) (FloatVal x) (FloatVal y) = FloatVal (x - y)

(|/|) :: Number -> Number -> Number
(|/|) (IntVal x) (IntVal y) = IntVal (x `div` y)
(|/|) (IntVal x) (FloatVal y) = FloatVal ((fromIntegral x) / y)
(|/|) (FloatVal x) (IntVal y) = FloatVal (x / (fromIntegral y))
(|/|) (FloatVal x) (FloatVal y) = FloatVal (x / y)

(|*|) :: Number -> Number -> Number
(|*|) (IntVal x) (IntVal y) = IntVal (x * y)
(|*|) (IntVal x) (FloatVal y) = FloatVal ((fromIntegral x) * y)
(|*|) (FloatVal x) (IntVal y) = FloatVal (x * (fromIntegral y))
(|*|) (FloatVal x) (FloatVal y) = FloatVal (x * y)

div_ :: Parser (Number -> Number -> Number)
div_= do
    char '/'
    return (|/|)
    
star :: Parser (Number -> Number -> Number)
star = do
    char '*'    
    return (|*|)    
    
plus :: Parser (Number -> Number -> Number)
plus = do
    char '+'    
    return (|+|)
    
minus :: Parser (Number -> Number -> Number)
minus = do
    char '-'    
    return (|-|)    
    
multiplication :: Parser Number
multiplication = do
    spaces
    lhv <- atom
    spaces
    t <- many tail
    return $ applyMany lhv t
    where tail =
                do
                    f <- star <|> div_
                    spaces
                    rhv <- atom
                    spaces
                    return (`f` rhv)
    
    
addition :: Parser Number
addition = do
    spaces
    lhv <- multiplication
    spaces
    t <- many tail
    return $ applyMany lhv t
    where tail =
                do
                    f <- plus <|> minus
                    spaces
                    rhv <- multiplication
                    spaces
                    return (`f` rhv)
                    
atom :: Parser Number
atom = number <|> do
    char '('
    res <- addition
    char ')'
    return res

-- factorial    
fact :: Parser Number
fact = do
    spaces
    lhv <- atom 
    char '!'
    return $ fact' lhv  
    where
        fact' (IntVal x) | x >= 0    = IntVal $ fact'' x
                         | otherwise = FloatVal (0.0/0.0)
        fact' _                      = FloatVal (0.0/0.0)

        fact'' 0 = 1
        fact'' 1 = 1
        fact'' n = n * (fact'' (n - 1))

-- negation

neg :: Parser Number
neg = do
    spaces
    char '~'
    rhv <- atom
    spaces
    return (neg' rhv)
    where
        neg' (IntVal x)   = IntVal (negate x)
        neg' (FloatVal x) = FloatVal (negate x)