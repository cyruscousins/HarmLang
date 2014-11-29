
module HarmLang.ChordProgressionDatabase
where

--import qualified Data.Text
import Data.List
import Data.Maybe
import qualified Data.Map

import HarmLang.Types
import HarmLang.Interpreter
import HarmLang.Utility

--TODO This should be generic, so they could be used with TimedChordProgression and TimedTimedChordProgression.

data ChordProgressionDatabase = ChordProgressionDatabase [((Data.Map.Map String String), TimedChordProgression)] deriving Show

--splitByCriterion :: ChordProgressionDatabase -> String -> [(String, [TimedChordProgression])

getProgressionsInCategoryByCriterion :: ChordProgressionDatabase -> String -> String -> [TimedChordProgression] 
getProgressionsInCategoryByCriterion (ChordProgressionDatabase list) criterion category = map snd (filter (\ (map, _) -> ((==) ((flip Data.Map.lookup) map criterion) (Just category))) list)

getCategoriesInCriterion :: ChordProgressionDatabase -> String -> [String]
--getCategoriesInCriterion (ChordProgressionDatabase list) criterion = Data.Map.mapMaybe ((flip Data.Map.lookup) criterion $ fst) list
getCategoriesInCriterion (ChordProgressionDatabase list) criterion = Data.Maybe.mapMaybe (\ cpMap -> Data.Map.lookup criterion cpMap) (map fst list)

getProgressionsCategorizedByCriterion :: ChordProgressionDatabase -> String -> [(String, [TimedChordProgression])]
getProgressionsCategorizedByCriterion cpd criterion = map (\catName -> (catName, getProgressionsInCategoryByCriterion cpd criterion catName)) $ getCategoriesInCriterion cpd criterion

compareBy :: (Ord b) => (a -> b) -> (a -> a -> Ordering)
compareBy f = (\ a1 a2 -> compare (f a1) (f a2))

sortGroupsByName :: [(String, [TimedChordProgression])] -> [(String, [TimedChordProgression])]
sortGroupsByName = sortBy (compareBy fst)

sortGroupsBySize :: [(String, [TimedChordProgression])] -> [(String, [TimedChordProgression])]
sortGroupsBySize = sortBy (compareBy (length . snd))

cpdEntryFromString :: String -> ((Data.Map.Map String String), TimedChordProgression) 
cpdEntryFromString str = let
  greatSplits = separateBy ';' str
  minorSplits = map (separateBy ':') greatSplits
  progression = interpretTimedChordProgression $ last greatSplits 
  allButLastMinorSplit = (take ((-) (length minorSplits) 1) minorSplits)
  progMap = Data.Map.fromList (map (\ l -> (l !! 0, l !! 1)) allButLastMinorSplit)
  in (progMap, progression)

cpdDbFromStrings :: [String] -> ChordProgressionDatabase
cpdDbFromStrings strings = ChordProgressionDatabase (map cpdEntryFromString (filter (\ str -> (not $ null str) && ((/=) '#' (head str))) strings))

loadChordProgressionDatabase :: String -> IO ChordProgressionDatabase
loadChordProgressionDatabase fileName =
  do
    wholeFile <- readFile fileName 
    return $ cpdDbFromStrings (lines wholeFile)

testCPDLoader = do
  db <- loadChordProgressionDatabase "res/progressions.txt"
  putStrLn . show $ db


