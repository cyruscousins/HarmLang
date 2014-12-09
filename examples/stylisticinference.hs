--module Examples.StylisticInference where

import HarmLang.Types
import HarmLang.InitialBasis

import HarmLang.ChordProgressionDatabase
import HarmLang.HarmonyDistributionModel
import HarmLang.Priors

import Data.List
import Data.Maybe

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
    priorPrior = chordLimitedLaplacianPriorFromDb $ concat allData -- all trans between chords in db are equally likely
    prior = hdmPrior $ buildHarmonyDistributionModelWithPrior k priorPrior 1.0 (concat hdmData) -- HDM of all data in db
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

--TODO robustness testing.

getRank :: (Ord n, Num n) => [n] -> Int -> Int
getRank l i = fromJust $ Data.List.elemIndex (l !! i) (reverse $ Data.List.sort l)

