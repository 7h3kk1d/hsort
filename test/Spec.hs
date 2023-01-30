{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
import Data.Functor.Identity (Identity (..))
import Lib
import Test.Hspec
import Test.QuickCheck
import Data.List (sort)

main :: IO ()
main = hspec $ do
  describe "sortM" $ do
    it "sorts an empty list" $ do
      Lib.mergeSortM ((Identity .) . compare) ([] :: [Int]) `shouldBe` Identity []
    it "sorts a single element list" $ do
      Lib.mergeSortM ((Identity .) . compare) [1 :: Int] `shouldBe` Identity [1]
    it "sorts does not modify an already sorted list" $ do
      Lib.mergeSortM ((Identity .) . compare) ([1, 2, 3, 4, 5] :: [Int]) `shouldBe` Identity [1, 2, 3, 4, 5]
    it "sorts an out of sort list" $ do
      Lib.mergeSortM ((Identity .) . compare) ([5, 2, 3, 4, 1] :: [Int]) `shouldBe` Identity [1, 2, 3, 4, 5]
    it "always sorts the list equivalent to builtin sort" $ do
      property $ \x -> runIdentity (Lib.mergeSortM ((Identity .) . compare) (x :: [Int])) == sort x
    it "sorts a large list" $ do
      let sorted = Lib.mergeSortM ((Identity .) . compare) [1..10000000 :: Int]
      length (runIdentity sorted) `shouldBe` 10000000