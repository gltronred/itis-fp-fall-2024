module D2 
  ( BinaryTree(..)
  , sumTree
  ) where

data BinaryTree
  = EmptyTree
  | NonEmptyTree BinaryTree Int BinaryTree
  deriving (Show,Read)

sumTree :: BinaryTree -> Int
sumTree EmptyTree = 0
sumTree (NonEmptyTree left root right) = sumTree left + root + sumTree right


