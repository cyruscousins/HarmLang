module HarmLang.Utility where

import Data.List

sortedUnique :: (Ord a, Eq a) => [a] -> [a]
sortedUnique = removeAdjacentDups . sort

-- http://stackoverflow.com/questions/8227218/removing-repeated-elements-from-a-list-in-haskell
removeAdjacentDups :: (Ord a, Eq a) => [a] -> [a]
removeAdjacentDups xs = remove xs
  where
    remove []  = []
    remove [x] = [x]
    remove (x1:x2:xs)
      | x1 == x2  = remove (x1:xs)
      | otherwise = x1 : remove (x2:xs)

-- http://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
separateBy :: Eq a => a -> [a] -> [[a]]
separateBy chr = unfoldr sep where
  sep [] = Nothing
  sep l  = Just . fmap (drop 1) . break (==chr) $ l

