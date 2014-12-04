
import HarmLang.Types
import HarmLang.ChordProgressionDatabase
import HarmLang.HarmonyDistributionModel
import HarmLang.InitialBasis

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





main :: IO ()
main = do
  cpd <- loadChordProgressionDatabase "../res/progressions.txt"
  let topClasses = (getTopCategories 4) (getByArtist cpd)
  putStrLn $ "Top Classes: " ++ (summary topClasses)
  let hdms = map (\ (name, progs) -> buildHarmonyDistributionModel 3 (map toUntimedProgression progs)) topClasses
  putStrLn $ "Probs of first prog: " ++ (show $ inferStyle hdms (toUntimedProgression $ (head . snd . head) topClasses))
  
  --TODO probProgGivenModel does not always work.  What to do for when something isn't found???
  --TODO cheating, don't use training data!



