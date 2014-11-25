
module HarmLang.HarmonyDistributionModel
where

import HarmLang.Types

import HarmLang.Probability

import qualified Data.Map 

--DIST:

type ChordDistribution = (Dist Chord)

type Probability = Double

--In a HDM, chords are ordered (to use the map)

instance Ord Chord where
  (<=) (Other a) (Other b) = a <= b
  (<=) (Other a) _ = True
  (<=) _ (Other b) = False
  (<=) h1@(Harmony pc1 ints1) h2@(Harmony pc2 ints2) = (harmonyToInt h1) <= (harmonyToInt h2)

intervalToInt :: Interval -> Int
intervalToInt (Interval a) = a

harmonyToInt :: Chord -> Int
harmonyToInt (Harmony (PitchClass pc) intervals) = 2 ^ 12 * (mod pc 12) + (sum (map (\ interval -> 2 ^ (intervalToInt interval)) intervals))


--HarmonyDistributionModel



--A few helper functions:


sliceKmers :: Int -> ChordProgression -> [ChordProgression]
sliceKmers i hprog@(h:hs) = if length hprog < i then [] else (take i hprog) : (sliceKmers i hs)

splitLast :: ChordProgression -> (ChordProgression, Chord)
splitLast (a:[]) = ([], a)
splitLast (a:b) = let (rest, last) = splitLast b in (a:rest, last)
splitLast _ = error "splitLast on empty list"

sliceKmersWithLastSplit :: Int -> ChordProgression ->  [(ChordProgression, Chord)]
sliceKmersWithLastSplit i cp = map splitLast $ sliceKmers (i + 1) cp

k :: Int
k = 3

data HarmonyDistributionModel = HarmonyDistributionModel Int (Data.Map.Map ChordProgression ChordDistribution) --Double (ChordProgression -> Double) (Map ChordProgression Chord)
--type HarmDistModel HarmonyDistributionModel

type HDM = HarmonyDistributionModel

buildHarmonyDistributionModel :: [ChordProgression] -> Int -> HarmonyDistributionModel
buildHarmonyDistributionModel cpArr kVal =
  let listVals = concat (map (sliceKmersWithLastSplit kVal) cpArr)
  --let listVals = foldr (++) [] (map (\ cp -> (map (\ kmer -> (take kVal kmer, last kmer) (sliceKmers (kVal + 1))  ))) cpArr) --TODO this is slow.
      listValLists = map (\ (key, val) -> (key, [val])) listVals
      mapAll = Data.Map.fromListWith (++) listValLists
      --mapAllSorted = map List.sort mapAll
      mapAllDist = Data.Map.map equally mapAll
  in HarmonyDistributionModel kVal mapAllDist
    

hdmChoose :: Double -> HDM -> HDM -> HDM
hdmChoose = error "No hdm choose."

--TODO Switch to pattern matching
hdmAskK :: HDM -> Int
hdmAskK (HarmonyDistributionModel kVal _) = kVal

-- chord progression must be of length k
--TODO no error, return a Maybe.
distAfter :: HDM -> ChordProgression -> Dist Chord
distAfter (HarmonyDistributionModel thisK hdmMap) cp =
  if length cp /= thisK
  then error "bad cp length"
  else let mapVal = Data.Map.lookup cp hdmMap 
       in case (mapVal) of
         Nothing -> error "did not find case." --TODO even distribution?
         Just d -> d
                   

--Pseudoprobabilities with wildcard.  Or reserve some space for wildcard.ner
--Prior on the HDM

terminalKmer :: Int -> ChordProgression -> ChordProgression
terminalKmer thisK cp = reverse $ take thisK $ reverse cp -- :'( TODO is this slow?

nextHarm :: HDM -> ChordProgression -> Chord
nextHarm hdm cp = maxlikelihood $ distAfter hdm (terminalKmer k cp) --TODO need sampling!

extend :: HDM -> ChordProgression -> ChordProgression
extend hdm cp = cp ++ [nextHarm hdm cp]

generate :: HDM -> ChordProgression -> Int -> ChordProgression
generate _ cp 0 = cp
generate hdm cp lenToGen = generate hdm (extend hdm cp) (lenToGen - 1)



--Calculates P(progression | HarmonyDistributionModel), or probability of generating a progression from a generative model.
probProgGivenModel :: HarmonyDistributionModel -> ChordProgression -> Probability
probProgGivenModel hdm@(HarmonyDistributionModel thisK _) prog = product (map (\ (kmer, nextVal) -> probv (distAfter hdm kmer) nextVal )  (sliceKmersWithLastSplit thisK prog) )

--(getK hdm)

inferStyle :: [HarmonyDistributionModel] -> [Chord] -> [Probability]
--Fixed point style.
--inferStyle models prog = map models ((flip probProgGivenModel) prog)
inferStyle models prog = map (\ model -> probProgGivenModel model prog) models

