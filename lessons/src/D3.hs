module D3 where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

-- Монады: побочные эффекты

-- IO - ввод-вывод
-- [] - недетерминированные вычисления (несколько результатов)
-- Maybe - опциональные результаты
-- Either - возможность ошибки
-- (r -> ) - окружение (Reader)
-- (w,) - дополнительный результат (Writer)

-- Вычисление, которое дополнительно пишет ещё один результат
newtype MyWriter w a = MyWriter { runMyWriter :: (w, a) } deriving (Show, Read)

instance Functor (MyWriter w) where
  fmap f (MyWriter (w, a)) = MyWriter (w, f a)

-- Monoid - у w есть значение "нуль" (mempty), и можно комбинировать два w при помощи mappend ((<>))
instance Monoid w => Applicative (MyWriter w) where
  pure a = MyWriter (mempty, a)
  MyWriter (w1, f) <*> MyWriter (w2, a) = MyWriter (w1 <> w2, f a)

instance Monoid w => Monad (MyWriter w) where
  MyWriter (w1, a) >>= k = let
    MyWriter (w2, b) = k a
    in MyWriter (w1 <> w2, b)

-- Например, для логирования:
f :: Int -> Int -> MyWriter [String] Int
f x y = do
  myTell ["f started with x=" ++ show x ++ " y=" ++ show y]
  let r = x + y
  info $ "result is " ++ show r
  pure r

-- Функция tell записывает доп.результат:
myTell :: w -> MyWriter w ()
myTell w = MyWriter (w, ())

-- Для удобства можно написать функцию
info :: String -> MyWriter [String] ()
info = myTell . (\x -> [x])

-------------- дальше используем готовое библиотечное -----------------------


f2 :: ReaderT Int (WriterT [String] IO) ()
f2 = do
  r <- ask
  s <- lift $ lift getLine
  lift $ tell ["String 1: " ++ s]
  t <- lift $ lift getLine
  lift $ tell ["String 2: " ++ t]
  lift $ lift $ print $ r * (length s + length t)
