-- | Parsing with parser combinators

{-
Комбинаторы парсеров

Грамматики

Например, регулярные выражения:
([0-9]{4})-([0-9]{2})-([0-9]{2})
2024-11-23
->
2024
11
23

Или что-то более сложное:
(fun arg1 (f2 arg2 arg3) arg4)
->
fun
   + arg1
   + f2
      + arg2
      + arg3
   + arg4

date ::= year '-' month '-' day
year ::= digit digit digit digit
month ::= digit digit
day ::= digit digit
digit ::= '0' | '1' | ... | '9'

expr ::= '(' exprs ')'
       | atom
exprs ::= expr ' ' exprs
        | ''
atom ::= [a-zA-Z]*

data Parser a = Parser { runParser :: String -> Either String (String, a) }
            ^                           ^                ^       ^     ^
      тип результата                    |                |       |     |
                                 входная строка          |       |     |
                                                       ошибка    |     |
                                                              остаток  |
                                                                    результат

number :: Parser Int -- парсит целые числа (Int)

если вызвать на строке
"1234abcdef"
внутри будет примерно такое
Right ("abcdef", 1234)

если на строке
"abcdef"
внутри примерно
Left "Expected digit, found a"

Этот тип - функтор, потому что можно применить внутрь функцию:
fmap :: (a -> b) -> f a -> f b

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> case runParser p s of
    Left err -> Left err
    Right (rest, a) -> Right (rest, f a)

Например, можно так:
fmap (+1) number

на строке "1234abcdef" вернётся 1235

Этот тип - аппликативный функтор, потому что реализуется pure и (<*>):

instance Applicative Parser where
  pure a = Parser $ \s -> Right (s, a)
  Parser pf <*> Parser pa = Parser $ \s -> case runParser pf s of
    Left e1 -> Left e1
    Right (rest, f) -> case runParser pa rest of
      Left e2 -> Left e2
      Right (rest2, a) -> Right (rest2, f a)

instance Alternative Parser where
  Parser pa <|> Parser pb = Parser $ \s -> case runParser pa s of
    Left _e1 -> runParser pb s
    Right (rest, a) -> Right (rest, a)
-}

module D4 where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

data SExpr = Atom String
           | Call [SExpr]
           deriving (Eq,Show,Read)

expr :: Parser SExpr
expr = Call <$> between (char '(') (char ')') exprs
  <|> Atom <$> atom

exprs :: Parser [SExpr]
exprs = expr `sepBy1` space1

atom :: Parser String
atom = some alphaNumChar


-- (abc (def ghi jkl) mno qpr)
-- (abc . (def . ghi . jkl . ()) . mno . qpr . ())
-- unsugar :: SExpr -> STree
-- data STree = Nil | Atom String | STree SExpr STree
