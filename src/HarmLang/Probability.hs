
module HarmLang.Probability
where

import HarmLang.Types

import Data.Map

--DIST:

type Dist Chord ChordDistribution




--HarmonyDistributionModel

k :: Int
k = 3

data HarmonyDistributionModel = HarmonyDistributionModel Integer (Map ChordProgression Chord) --Double (ChordProgression -> Double) (Map ChordProgression Chord)
--type HarmDistModel HarmonyDistributionModel

type HarmonyDistributionModel HDM

buildHarmonyDistributionModel :: [ChordProgression] Integer -> HarmonyDistributionModel

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
