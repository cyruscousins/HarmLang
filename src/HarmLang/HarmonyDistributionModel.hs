
module HarmLang.HarmonyDistributionModel
where

import HarmLang.Types

import Data.Map

--DIST:

type (Dist Chord) ChordDistribution

type Double Probability



--HarmonyDistributionModel



--A few helper functions:


sliceKmers :: Int -> ChordProgression -> [ChordProgression]
sliceKmers i h:hs = if length h:hs < i then [] else (take i h:hs) : (sliceKmers i hs)

k :: Int
k = 3

data HarmonyDistributionModel = HarmonyDistributionModel Integer (Data.Map ChordProgression ChordDistribution) --Double (ChordProgression -> Double) (Map ChordProgression Chord)
--type HarmDistModel HarmonyDistributionModel

type HarmonyDistributionModel HDM

buildHarmonyDistributionModel :: [ChordProgression] -> Integer -> HarmonyDistributionModel
buildHarmonyDistributionModel cpArr k =
  let listVals = foldr (++) [] (map (\ cp -> (map (\ kmer -> (take k kmer, last kmer) (sliceKmers (k + 1))  ))) cpArr) --TODO this is slow.
      listValLists = map (\ (key, val) -> (key, [val])) listVals
      mapAll = Data.Map.fromListWith (++) listValLists
      --mapAllSorted = map List.sort mapAll
      mapAllDist = map equally mapAll
  in mapAllDist
    

hdmChoose :: Double -> HDM -> HDM -> HDM
hdmChoose = error "No hdm choose."

--TODO Switch to pattern matching
hdmAskK :: HDM -> Integer
hdmAskK (k _) = k

-- chord progression must be of length k
distAfter :: HDM -> ChordProgression -> Dist Chord
distAfter hdm cp = if length cp /= k then error "bad cp length" else
  case (findKey cp hdm) of
    Nothing -> error "did not find case." --TODO even distribution?
    Just d -> d
                   

--Pseudoprobabilities with wildcard.  Or reserve some space for wildcard.ner
--Prior on the HDM

terminalKmer :: ChordProgression -> ChordProgression
terminalKmer cp = reverse $ take k $ reverse cp -- :'( TODO no. bad.

nextHarm :: HDM -> ChordProgression -> Chord
nextHarm hdm cp = (terminalKmer k cp) --TODO need sampling!

extend :: HDM -> ChordProgression -> ChordProgression
extend hdm cp = cp ++ [nextHarm hdm cp]

generate :: HDM -> ChordProgression -> Integer -> ChordProgression
generate _ cp 0 = cp
generate hdm cp lenToGen = generate hdm (extend cp) (lenToGen - 1)









--Calculates P(progression | HarmonyDistributionModel), or probability of generating a progression from a generative model.
probProgGivenModel :: HarmonyDistributionModel -> [Harmony] -> Probability
probProgGivenModel hdm@(HarmonyDistributionModel k [[Harmony]]) prog = product (map (sliceKmers k prog) (\ kmer -> probv (hDist hdm (take (k-1) kmer)) (last kmer) ) )

--(getK hdm)

inferStyle :: [HarmonyDistributionModel] -> [Harmony] -> [Probability]
--Fixed point style.
--inferStyle models prog = map models ((flip probProgGivenModel) prog)
inferStyle models prog = map models (\ model -> probProgGivenModel model prog)
