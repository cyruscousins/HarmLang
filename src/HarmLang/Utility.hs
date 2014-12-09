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

-- A Cyrus Cousins original.
allRotations :: [a] -> [[a]]
allRotations l = map (\i -> take (length l) ((drop i) (cycle l))) [0..((-) (length l) 1)]

-- https://www.haskell.org/pipermail/haskell-cafe/2003-June/004484.html
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = xss ++ map (x:) xss
                  where xss = powerset xs

--map with indices
mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = let
    mapIndH f [] _ = []
    mapIndH f (a:as) i = (f a i):(mapIndH f as ((+) i 1))
  in
    mapIndH f l 0

--http://stackoverflow.com/questions/9270478/efficiently-find-indices-of-maxima-of-a-list
indexOfMaximum :: (Ord n, Num n) => [n] -> Int
indexOfMaximum list =
   let indexOfMaximum' :: (Ord n, Num n) => [n] -> Int -> n -> Int -> Int
       indexOfMaximum' list' currIndex highestVal highestIndex
          | null list'                = highestIndex
          | (head list') > highestVal = 
               indexOfMaximum' (tail list') (1 + currIndex) (head list') currIndex
          | otherwise                 = 
               indexOfMaximum' (tail list') (1 + currIndex) highestVal highestIndex
   in indexOfMaximum' list 0 0 0

indexOfMinimum :: (Ord n, Num n) => [n] -> Int
indexOfMinimum list = indexOfMaximum (map (\a -> 0 - a) list)
