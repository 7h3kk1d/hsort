module Lib (mergeSortM) where

mergeSortM :: Monad m => (a -> a -> m Ordering) -> [a] -> m [a]
mergeSortM _ [] = pure []
mergeSortM _ [a] = pure [a]
mergeSortM cmp as = do
  let (firstHalf, secondHalf) = splitAt (length as `div` 2) as
  firstHalfSorted <- mergeSortM cmp firstHalf
  secondHalfSorted <- mergeSortM cmp secondHalf
  mergeM cmp firstHalfSorted secondHalfSorted

mergeM :: Monad m => (a -> a -> m Ordering) -> [a] -> [a] -> m [a]
mergeM _ as [] = pure as
mergeM _ [] bs = pure bs
mergeM cmp (a : as) (b : bs) = do
  order <- cmp a b
  case order of
    LT -> fmap (a :) (mergeM cmp as (b : bs))
    _ -> fmap (b :) (mergeM cmp (a : as) bs)