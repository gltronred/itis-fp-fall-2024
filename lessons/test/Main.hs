module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import D2 (BinaryTree(..), sumTree)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ arithTests, listTests, treeTests ]

arithTests :: TestTree
arithTests = testGroup "Arith"
  [ testCase "2+2=4" $
      2+2 @?= (4 :: Int)
  , testCase "7 is odd" $
      assertBool "Oops, 7 is not odd" (odd (7 :: Int))
  ]

listTests :: TestTree
listTests = testGroup "List"
  [ testProperty "len (drop k xs) < len xs" $ property $ do
      xs <- forAll $ Gen.list (Range.linear 1 100) Gen.alpha
      k <- forAll $ Gen.int (Range.linear 1 10)
      let n' = length $ drop k xs
          n  = length xs
      diff n' (<) n
  -- -- this test is wrong:
  -- , testProperty "len xs < 20" $ property $ do
  --     xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
  --     diff (length xs) (<) 20
  ]

treeTests :: TestTree
treeTests = testGroup "Tree" 
  [ testCase "sum bt1 == 11" $ sumTree bt1 @?= 11
  -- -- this test is wrong:
  -- , testProperty "sumTree t < 10" $ property $ do
  --     t <- forAll $ genTree $ Range.linear 1 10
  --     diff (sumTree t) (<) 10
  ]

bt1 :: BinaryTree
bt1 = NonEmptyTree
  (NonEmptyTree EmptyTree 5 EmptyTree)
  6
  EmptyTree

genTree :: MonadGen m => Range.Range Int -> m BinaryTree
genTree range = Gen.recursive Gen.choice
  [ pure EmptyTree ]
  [ Gen.subtermM2 (genTree range) (genTree range)
    (\l r -> NonEmptyTree <$> pure l <*> Gen.int range <*> pure r) 
    -- (\l r -> do
    --     x <- Gen.int (Range.linear 0 2)
    --     pure $ NonEmptyTree l x r) 
  ]

{-
 - проверить свойство, что сумма элементов дерева (с каким-то диапазоном для листьев?) больше, чем глубина дерева
 -}
