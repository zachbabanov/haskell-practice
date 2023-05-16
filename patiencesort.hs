import Data.List (foldl')

patienceSort :: Ord a => [a] -> [a]
patienceSort = foldl' merge [] . map pure
  where
    merge [] ys = ys
    merge xs [] = xs
    merge xs@(x:xs') ys@(y:ys')
      | x < y     = x : merge xs' ys
      | otherwise = y : merge xs ys'