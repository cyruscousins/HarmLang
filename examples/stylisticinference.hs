
import HarmLang.Types
import HarmLang.ChordProgressionDatabase
import HarmLang.HarmonyDistributionModel
import HarmLang.InitialBasis

getByArtist :: ChordProgressionDatabase -> [(String, [TimedChordProgression])]
getByArtist cpd = (getProgressionsCategorizedByCriterion cpd "Artist") 

getTopCategories :: Int -> [(String, [TimedChordProgression])] -> [(String, [TimedChordProgression])]
getTopCategories count = (take count) . reverse . sortGroupsBySize

summary :: [(String, [TimedChordProgression])] -> String
summary ([]) = ""
summary (item:more) = (fst item) ++ ": " ++ (show $ (length . snd) item) ++ "\n" ++ (summary more)

probsToStr :: [Double] -> String
probsToStr (a:[]) = show a
probsToStr (a:b) = (show a) ++ ", " ++ (probsToStr b)





main :: IO ()
main = do
  cpd <- loadChordProgressionDatabase "../res/progressions.txt"
  let topClasses = (getTopCategories 4) (getByArtist cpd)
  putStrLn $ "Top Classes: " ++ (summary topClasses)
  let hdms = map (\ (name, progs) -> buildHarmonyDistributionModel 3 (map toUntimedProgression progs)) topClasses
  putStrLn $ "Probs of first prog: " ++ (probsToStr $ inferStyle hdms (toUntimedProgression $ (head . snd . head) topClasses))
  
  --TODO cheating, don't use training data!

