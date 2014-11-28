
module ChordProgressionDatabase
where

import Data.List
import Data.Maybe
import qualified Data.Map
--import qualified Data.Maybe

import HarmLang.Types



data ChordProgressionDatabase = ChordProgressionDatabase [((Data.Map.Map String String), ChordProgression)]

--splitByCriterion :: ChordProgressionDatabase -> String -> [(String, [ChordProgression])

getProgressionsInCategoryByCriterion :: ChordProgressionDatabase -> String -> String -> [ChordProgression] 
getProgressionsInCategoryByCriterion (ChordProgressionDatabase list) criterion category = map snd (filter (\ (map, _) -> ((==) ((flip Data.Map.lookup) map criterion) (Just category))) list)

getCategoriesInCriterion :: ChordProgressionDatabase -> String -> [String]
--getCategoriesInCriterion (ChordProgressionDatabase list) criterion = Data.Map.mapMaybe ((flip Data.Map.lookup) criterion $ fst) list
getCategoriesInCriterion (ChordProgressionDatabase list) criterion = Data.Maybe.mapMaybe (\ cpMap -> Data.Map.lookup criterion cpMap) (map fst list)

getProgressionsCategorizedByCriterion :: ChordProgressionDatabase -> String -> [(String, [ChordProgression])]
getProgressionsCategorizedByCriterion cpd criterion = map (\catName -> (catName, getProgressionsInCategoryByCriterion cpd criterion catName)) $ getCategoriesInCriterion cpd criterion

compareBy :: (Ord b) => (a -> b) -> (a -> a -> Ordering)
compareBy f = (\ a1 a2 -> compare (f a1) (f a2))

sortGroupsByName :: [(String, [ChordProgression])] -> [(String, [ChordProgression])]
sortGroupsByName = sortBy (compareBy fst)

sortGroupsBySize :: [(String, [ChordProgression])] -> [(String, [ChordProgression])]
sortGroupsBySize = sortBy (compareBy (length . snd))

--loadChordProgressionDatabase :: String -> IO ChordProgressionDatabase
