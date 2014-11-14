module HarmLang.Expression
where

import HarmLang.Types
-- import Data.Typeable
-- import Data.Data


data HLExp = ExpPitchClass PitchClass 
           | ExpInterval Interval
           | ExpPitch Pitch
           | ExpTimedChord TimedChord
           | ExpNote Note
           | ExpChord Chord
           | ExpPitchProgression [Pitch]
           | ExpChordProgression [Chord]
           | ExpTimedChordProgression [TimedChord]
           | ExpNoteProgression [Note]
    deriving(Show)

