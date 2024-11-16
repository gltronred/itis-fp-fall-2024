
{-

Типизация:

             статическая  динамическая
строгая        Haskell
нестрогая

-}

{-

Tooling:

ghcup - для установки: https://www.haskell.org/ghcup/
ghc - компилятор
cabal - сборка пакетов

ghci - REPL (read-eval-print loop)

* GHCup

ghcup tui - terminal интерфейс
ghcup list
ghcup install
ghcup ...

* GHC

ghc File.hs
ghc --make File.hs

* GHCi

ghci File.hs

:q, :quit - выйти из REPL
:l, :load File - загрузить файл File
:r, :reload - перезагружает все файлы
:m + Module - загружает модуль
:m - Module - выгружает модуль

-}

-- module Main where
import Types

f x = x*x

functionName arg1 arg2 = arg1 + arg2
-- название
--           аргументы
--                       тело функции

f' x = 2*x

-- стандартная арифметика
-- скобки, +, *
-- div, mod

h x y = f' (x+y)
-- h 3 4
-- f' (3+4)
-- 2*(3+4)
-- ==> 2*7 ==> 14

h2 x y = functionName (functionName x y) (f x)
-- h2 3 4
-- functionName (functionName 3 4) (f 3)
-- functionName 3 4 + f 3
-- (3+4) + 3*3
-- ==> 16

-- (+), (*)
f'' x = (*) 2 x

f''' = 13 `mod` 3   -- mod 13 3
f'''' = 13 `functionName` 4 -- functionName 13 4

----------------------------------------------------------

data Switch = On | Off
--  название
--            определение типа

-- data Bool = False | True
-- data SmallInt = -32768 | -32767 | ... | 32767 -- на самом деле, определено не так

data Coord = Coord { coordX :: Int, coordY :: Int }
--        конструктор

data Colour
  = Red
  | Green
  | Blue
  | CustomColour Int Int Int

data ColouredPoint = ColouredPoint Coord Colour

-- case ... of
--   v1 -> ...
--   v2 -> ...
switchToBool1 x = case x of
  On -> True
  Off -> False

switchToBool On = True
switchToBool _ = False

xCoordinate (Coord x y) = x


colouredFun (ColouredPoint (Coord x y) Red) = "red"
colouredFun (ColouredPoint (Coord x y) Green) = "green"
colouredFun (ColouredPoint (Coord x y) _) = "blue or custom"


equalPoints (Coord x1 y1) (Coord x2 y2) = x1 == x2 && y1 == y2

twiceXCoord Coord{ coordX=x } = 2*x


data Vector2D a = Vector2D a a

v1 :: Vector2D Int
v1 = Vector2D 3 4

-- Error!
-- v2 = Vector2D 3 "adf"

-- Стандартные типы

{-
data Maybe a = Nothing | Just a
data Either a b = Left a | Right b
data (,) a b = (,) a b
-- v3 = (3,4)
-}

fromDefault :: a -> (Maybe a -> a)
fromDefault def Nothing = def
fromDefault def (Just a) = a


-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- (.) f g x = f (g x)

-- ($) :: (a -> b) -> a -> b
-- ($) f x = f x

-- Example:
-- λ> f' . f $ 3+4
-- 98
-- λ> f $ 3+4
-- 49
-- λ> f (3+4)
-- 49


fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

-- fact 3
-- [n |-> 3] 3 * fact (3-1)  ==>  3 * fact 2
-- [n |-> 2] 3 * (2 * fact (2-1))  ==>  3 * (2 * fact 1)
-- [n |-> 1] 3 * (2 * (1 * fact (1-1)))  ==>  3 * (2 * (1 * fact 0))
-- ==> 3 * (2 * (1 * 1)) ==> 6


data BinaryTree
  = EmptyTree
  | NonEmptyTree BinaryTree Int BinaryTree
  deriving (Show,Read)


sumTree :: BinaryTree -> Int
sumTree EmptyTree = 0
sumTree (NonEmptyTree left root right) = sumTree left + root + sumTree right

bt1 = NonEmptyTree
  (NonEmptyTree EmptyTree 5 EmptyTree)
  6
  EmptyTree


-- дерево с значениями типа node в промежуточных узлах
-- и значениями типа leaf в листьях
data Tree node leaf
  = Leaf leaf
  | Node (Tree node leaf) node (Tree node leaf)
  deriving (Show,Read)

-- операция
data Op
  = Add -- сложение
  | Sub -- вычитание
  | Mul -- умножение
  | Div -- деление (нацело)

-- вычисление значения выражения, заданного деревом
eval :: Tree Op Int -> Int
eval = error "Implement me!"

-- количество листьев в дереве
countLeaf :: Tree a b -> Int
countLeaf = error "Implement me!"


-- (3+4)-(9/8)
t1 = Node
  (Node (Leaf 3) Add (Leaf 4))
  Sub
  (Node (Leaf 9) Div (Leaf 8))
-- eval t1 == 6
-- countLeaf t1 == 4


main = putStrLn "Hi!"
