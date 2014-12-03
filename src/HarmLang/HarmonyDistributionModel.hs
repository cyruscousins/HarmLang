
module HarmLang.HarmonyDistributionModel
where

import HarmLang.Types
import HarmLang.InitialBasis
import HarmLang.Priors

import HarmLang.Probability

import qualified Data.Map 

--DIST:

type Probability = Double

--In a HDM, chords are ordered (to use the map)

instance Ord Chord where
  (<=) (Other a) (Other b) = a <= b
  (<=) (Other a) _ = True
  (<=) _ (Other b) = False
  (<=) h1@(Harmony pc1 ints1) h2@(Harmony pc2 ints2) = (fromEnum h1) <= (fromEnum h2)

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

data HarmonyDistributionModel = HDMTC Int Prior (Data.Map.Map ChordProgression ChordDistribution)

--type HarmDistModel HarmonyDistributionModel

type HDM = HarmonyDistributionModel

--2 helper functions

cpFirstInterval :: ChordProgression -> Interval
cpFirstInterval cp@((Harmony (PitchClass p) _):_) = Interval p
cpFirstIntreval _ = Interval 0

{-
transposeToA :: ChordProgression -> ChordProgression
transposeToA cp = transpose cp (inverse (cpFirstInterval cp))
-}

transposeToA :: (ChordProgression, Chord) -> (ChordProgression, Chord)
transposeToA (cp, chord) = (transpose cp (inverse (cpFirstInterval cp)), transpose chord (inverse (cpFirstInterval cp)))

dplus :: Double -> Double -> Double
dplus = (+)

ddiv :: Double -> Double -> Double
ddiv = (/)

--Function used to build a harmony distribution model
buildHarmonyDistributionModelWithPrior :: Int -> Prior -> Double -> [ChordProgression] -> HarmonyDistributionModel
buildHarmonyDistributionModelWithPrior kVal prior priorWeight cpArr =
  let listVals = concat (map (sliceKmersWithLastSplit kVal) cpArr)
      --Greatest Hack of all time.  Transpose everything to starting on an A.
      listValsTransposed = map transposeToA listVals --This is where Key Agnosticism occurs
      listValLists = map (\ (key, val) -> (key, [val])) listValsTransposed
      mapAll = Data.Map.fromListWith (++) listValLists
      --mapAllSorted = map List.sort mapAll
      mapAllDist = if priorWeight == 0
                   then Data.Map.map equally mapAll
                   else Data.Map.mapWithKey (\ before afters -> choose (ddiv priorWeight (dplus priorWeight (fromIntegral $ length afters))) (prior before) (equally afters)) mapAll
  in HDMTC kVal prior mapAllDist

buildHarmonyDistribution :: Int -> [ChordProgression] -> HarmonyDistributionModel
buildHarmonyDistribution kVal = buildHarmonyDistributionModelWithPrior kVal laplacianPrior 0

hdmChoose :: Double -> HDM -> HDM -> HDM
hdmChoose = error "No hdm choose."

--TODO Switch to pattern matching
hdmAskK :: HDM -> Int
hdmAskK (HDMTC kVal _ _) = kVal

-- chord progression must be of length k
--TODO no error, return a Maybe.
distAfter :: HDM -> ChordProgression -> Dist Chord
distAfter (HDMTC thisK prior hdmMap) cp =
  if length cp /= thisK
  then error "bad cp length"
  else let mapVal = Data.Map.lookup cp hdmMap 
       in case (mapVal) of
         Nothing -> (prior cp) -- error "did not find case." --Use prior when nothing is available.
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
probProgGivenModel hdm@(HDMTC thisK _ _) prog = product (map (\ (kmer, nextVal) -> probv (distAfter hdm kmer) nextVal )  (sliceKmersWithLastSplit thisK prog) )

--(getK hdm)

inferStyle :: [HarmonyDistributionModel] -> ChordProgression -> [Probability]
--Fixed point style.
--inferStyle models prog = map models ((flip probProgGivenModel) prog)
inferStyle models prog = map (\ model -> probProgGivenModel model prog) models

