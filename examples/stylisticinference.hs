--module Examples.StylisticInference where

import HarmLang.Types
import HarmLang.InitialBasis

import HarmLang.ChordProgressionDatabase
import HarmLang.HarmonyDistributionModel
import HarmLang.Priors

import Data.List
import Data.Maybe

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = let
    mapIndH f [] _ = []
    mapIndH f (a:as) i = (f a i):(mapIndH f as ((+) i 1))
  in
    mapIndH f l 0

-- groups progressions in a CPD by artist, denoting the artist with a string
getByArtist :: ChordProgressionDatabase -> [(String, [TimedChordProgression])]
getByArtist cpd = (getProgressionsCategorizedByCriterion cpd "Artist") 

-- extracts from a list of categories the n with the most progressions
getTopCategories :: Int -> [(String, [TimedChordProgression])] -> [(String, [TimedChordProgression])]
getTopCategories n = (take n) . reverse . sortGroupsBySize


artists :: [String]
--artists = ["Antonio-Carlos Jobim", "Duke Ellington", "Cole Porter", "Richard Rodgers", "George Gershwin", "Jerome Kern"]
artists = ["Antonio-Carlos Jobim", "Duke Ellington", "George Gershwin", "Jerome Kern"]
getTestArtists :: ChordProgressionDatabase -> [(String, [TimedChordProgression])]
getTestArtists db = filter (\ (name, cps) -> (elem name artists)) (getByArtist db)

splitTrainingTest :: Int -> [[ChordProgression]] -> ([(ChordProgression, Int)], [[ChordProgression]])
splitTrainingTest tSize db = (concat $ mapInd (\ l index -> map (\ q -> (q, index)) (take tSize l)) db, map (drop tSize) db)

-- build harmony distribution models, and some cool priors to boot.
makeHdms :: [[ChordProgression]] -> [[ChordProgression]] -> [HarmonyDistributionModel]
makeHdms allData hdmData =
  let
    k = 3
    priorPrior = chordLimitedLaplacianPriorFromDb $ concat allData
    prior = hdmPrior $ buildHarmonyDistributionModelWithPrior k priorPrior 1.0 (concat hdmData)
  in
    map (\thisHdmData -> buildHarmonyDistributionModelWithPrior k prior 1.0 thisHdmData) hdmData


-- gives the name of each category followed by a colon followed by number of
-- progressions in that category. each entry separated by newline.
summary :: [(String, [TimedChordProgression])] -> String
summary ([]) = ""
summary ((s,l):rest) =  s ++ ": " ++ (show $ length l) ++ "\n" ++ (summary rest)
--summary (item:more) = (fst item) ++ ": " ++ (show $ (length . snd) item) ++ "\n" ++ (summary more)

probsToStr :: [Double] -> String
probsToStr [] = ""
probsToStr (a:[]) = show a
probsToStr (a:b) = (show a) ++ ", " ++ (probsToStr b)



main :: IO ()
main = do
  putStrLn "Please enter the path to the database."
  path <- getLine
  cpd <- loadChordProgressionDatabase (if path == "" then "./res/progressions.txt" else path)
  --putStrLn $ "DB:\n" ++ (show cpd)

  --Has type [(String, [TimedChordProgression])]
  --let topClasses = (getTopCategories 5) (getByArtist cpd)
  let topClasses = (getTestArtists cpd)
  putStrLn $ "Top Classes:\n" ++ (summary topClasses)
  --let hdms = map (\ (name, progs) -> buildHarmonyDistributionModel 2 (map toUntimedProgression progs)) topClasses

  --Has type ([(ChordProgression, Int)], [[ChordProgression]])
  let (test, training) = splitTrainingTest 3 (map ((map toUntimedProgression) . snd) topClasses)
  let hdms = makeHdms (map ((map toUntimedProgression) . snd) (getByArtist cpd)) training
  
  putStrLn $ concat (map (\ (prog, classIndex) -> "Class " ++ (show classIndex) ++ ", " ++ ("rank " ++ (show $ getRank (inferStyle hdms prog) classIndex)) ++ ", " ++ (show $ inferStyle hdms prog) ++ "\n") test )
  --putStrLn $ "Prob 1: " ++ (show $ inferStyle [hdms !! 0] (toUntimedProgression $ (head . snd . head) topClasses))
  --putStrLn $ "Prob 1: " ++ (show $ inferStyle [hdms !! 1] (toUntimedProgression $ (head . snd . head) topClasses))
  --putStrLn $ "Probs of first prog: " ++ (show $ inferStyle hdms (toUntimedProgression $ (head . snd . head) topClasses))
  --putStrLn $ "Probs of second prog: " ++ (show $ inferStyle hdms (toUntimedProgression $ (head . snd . (flip (!!)) 1) topClasses))
  --putStrLn $ "Probs of third prog: " ++ (show $ inferStyle hdms (toUntimedProgression $ (head . snd . (flip (!!)) 2) topClasses))
  --putStrLn $ "Probs of fourth prog: " ++ (show $ inferStyle hdms (toUntimedProgression $ (head . snd . (flip (!!)) 3) topClasses))
  

  --TODO probProgGivenModel does not always work.  What to do for when something isn't found???
  --TODO cheating, don't use training data!


--TODO robustness testing.

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


getRank :: (Ord n, Num n) => [n] -> Int -> Int
getRank l i = fromJust $ Data.List.elemIndex (l !! i) (reverse $ Data.List.sort l)

