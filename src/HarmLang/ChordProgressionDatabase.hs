
module ChordProgressionDatabase
where

import qualified Data.Map

import HarmLang.Types



data ChordProgressionDatabase = ChordProgressionDatabase [((Data.Map.Map String String), ChordProgression)]

--splitByCriterion :: ChordProgressionDatabase -> String -> [(String, [ChordProgression])

getProgressionsInCategoryByCriterion :: ChordProgressionDatabase -> String -> String -> [ChordProgression] 
extractByCriterion (ChordProgressionDatabase list) criterion category = map snd (filter (\ (map, _) -> ((==) (Data.Map.lookup map criterion) (Just category))) list)

getCategoriesInCriterion :: ChordProgressionDatabase -> String -> [String]

getProgressionsCategorizedByCriterion :: ChordProgressionDatabase -> String -> [(String, [ChordProgression])]

sortGroupsByName :: [(String, [ChordProgression])] -> [(String, [ChordProgression])]

sortGroupsBySize :: [(String, [ChordProgression])] -> [(String, [ChordProgression])]


