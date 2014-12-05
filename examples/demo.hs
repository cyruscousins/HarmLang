module Examples.StylisticInference where

import HarmLang.Types
import HarmLang.InitialBasis

import HarmLang.ChordProgressionDatabase
import HarmLang.HarmonyDistributionModel
import HarmLang.Priors
import HarmLang.Interpreter
import HarmLang.Probability

import Data.List
import Data.Maybe

-- groups progressions in a CPD by artist, denoting the artist with a string
buildHDM :: ChordProgressionDatabase -> HarmonyDistributionModel
buildHDM cpd = buildHarmonyDistributionModel 4 $ map toUntimedProgression $ concat $ map snd (getProgressionsCategorizedByCriterion cpd "Artist")


main :: IO ()
main = do
  cpd <- loadChordProgressionDatabase "./res/progressions.txt"
  let hdm = buildHDM cpd
  putStrLn "Please enter a chord progression of length 4"
  chordStr <- getLine
  let cp = interpretChordProgression chordStr
  putStrLn $ show (take 10 $ likelylist (distAfter hdm cp))
