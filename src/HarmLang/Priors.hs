
module HarmLang.Priors where

import Data.List
import Data.Maybe


import HarmLang.Types
import HarmLang.InitialBasis
import HarmLang.Utility


import HarmLang.Probability

type Prior = (ChordProgression -> ChordDistribution)

type ChordDistribution = Dist Chord

--LAPLACIAN PRIOR

allChordsDist :: ChordDistribution
allChordsDist = equally allChords

laplacianPrior :: Prior
laplacianPrior _ = allChordsDist

--TODO chooseNoDups

--NOTE SIMILARITY BASED PRIOR
noteSimilarityPrior :: Prior
noteSimilarityPrior cp =
  let
    notesInChord = getNotesFromChordProgression cp
    notesNotInChord = (\\) allPitchClasses notesInChord
    noNews = concat $ map chordInversions (map (\pcs -> toChord (head pcs) (tail pcs)) (powerset notesInChord))
    --TODO 1new? general?
  in equally noNews

--Laplacian with limited prior
allChordsTypeLimited :: [ChordType] -> [Chord]
allChordsTypeLimited chords = [(Harmony root intervalSets) | root <-allPitchClasses, intervalSets <- chords]

chordLimitedLaplacianPrior :: [ChordType] -> Prior
chordLimitedLaplacianPrior chords _ = equally (allChordsTypeLimited chords)

--smartPrior :: ChordType -> Prior
--smartPrior chords = weightedly (map (\ chord ->  


--Prior based on a set of chords
chordToChordType :: Chord -> Maybe ChordType
chordToChordType (Harmony pc ty) = Just ty
chordToChordType _ = Nothing

allChordsUsed :: [ChordProgression] -> [ChordType]
allChordsUsed cpDb = Data.List.nub $ (Data.Maybe.mapMaybe chordToChordType (concat cpDb))

chordLimitedLaplacianPriorFromDb :: [ChordProgression] -> Prior
chordLimitedLaplacianPriorFromDb db = chordLimitedLaplacianPrior (allChordsUsed db)


--Choose applied to the prior type
priorChoose :: Double -> Prior -> Prior -> Prior
priorChoose weight p0 p1 cp = choose weight (p0 cp) (p1 cp)

