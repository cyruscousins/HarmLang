module Examples.StylisticInference where

import HarmLang.Types
import HarmLang.InitialBasis

import HarmLang.ChordProgressionDatabase
import HarmLang.HarmonyDistributionModel
import HarmLang.Priors

-- groups progressions in a CPD by artist, denoting the artist with a string
getByArtist :: ChordProgressionDatabase -> [(String, [TimedChordProgression])]
getByArtist cpd = (getProgressionsCategorizedByCriterion cpd "Artist") 

-- extracts from a list of categories the n with the most progressions
getTopCategories :: Int -> [(String, [TimedChordProgression])] -> [(String, [TimedChordProgression])]
getTopCategories n = (take n) . reverse . sortGroupsBySize

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

makeHdms :: [[ChordProgression]] -> [HarmonyDistributionModel]
makeHdms hdmData =
  let
    k = 3
    priorPrior = chordLimitedLaplacianPriorFromDb $ concat hdmData
    prior = hdmPrior $ buildHarmonyDistributionModelWithPrior k priorPrior 1.0 (concat hdmData)
  in
    map (\thisHdmData -> buildHarmonyDistributionModelWithPrior k priorPrior 1.0 thisHdmData) hdmData



main :: IO ()
main = do
  cpd <- loadChordProgressionDatabase "./res/progressions.txt"
  let topClasses = (getTopCategories 4) (getByArtist cpd)
  putStrLn $ "Top Classes: " ++ (summary topClasses)
  --let hdms = map (\ (name, progs) -> buildHarmonyDistributionModel 2 (map toUntimedProgression progs)) topClasses
  let hdms = makeHdms (map (\ (name, progs) -> (map toUntimedProgression progs)) topClasses)
  --putStrLn $ "Prob 1: " ++ (show $ inferStyle [hdms !! 0] (toUntimedProgression $ (head . snd . head) topClasses))
  --putStrLn $ "Prob 1: " ++ (show $ inferStyle [hdms !! 1] (toUntimedProgression $ (head . snd . head) topClasses))
  putStrLn $ "Probs of first prog: " ++ (show $ inferStyle hdms (toUntimedProgression $ (head . snd . head) topClasses))
  putStrLn $ "Probs of second prog: " ++ (show $ inferStyle hdms (toUntimedProgression $ (head . snd . (flip (!!)) 1) topClasses))
  putStrLn $ "Probs of third prog: " ++ (show $ inferStyle hdms (toUntimedProgression $ (head . snd . (flip (!!)) 2) topClasses))
  putStrLn $ "Probs of fourth prog: " ++ (show $ inferStyle hdms (toUntimedProgression $ (head . snd . (flip (!!)) 3) topClasses))
  
  --TODO probProgGivenModel does not always work.  What to do for when something isn't found???
  --TODO cheating, don't use training data!



