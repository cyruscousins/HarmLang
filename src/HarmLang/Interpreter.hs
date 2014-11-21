module HarmLang.Interpreter where

import HarmLang.Parser
import HarmLang.Types
import HarmLang.InitialBasis

import Text.ParserCombinators.Parsec

--In addition to compile time code, here functionality to interpret strings at runtime (with identical parse semantics) is provided.

--These are typed functions that cause runtime errors on bad input

--Each function is essentially the same: just boring boilerplate allowing the use of the parser at runtime.

interpretPitchClass :: String -> PitchClass
interpretPitchClass = 
  let checkResult (Right res) = res
      checkResult _ = error "Invalid PitchClass."
  in 
    checkResult . (parse parsePitchClass "")

interpretPitch :: String -> Pitch
interpretPitch = 
  let checkResult (Right harmony) = harmony
      checkResult _ = error "Invalid Pitch."
  in 
    checkResult . (parse parsePitch "")

interpretChord :: String -> Chord
interpretChord = 
  let checkResult (Right harmony) = harmony
      checkResult _ = error "Invalid Chord."
  in 
    checkResult . (parse parseChord "")

interpretTimedChord :: String -> TimedChord
interpretTimedChord =
  let checkResult (Right harmony) = harmony
      checkResult _ = error "Invalid Timed Chord."
  in 
    checkResult . (parse parseTimedChord "")

interpretNote :: String -> Note
interpretNote = 
  let checkResult (Right res) = res
      checkResult _ = error "Invalid Note."
  in 
    checkResult . (parse parseNote "")



interpretPitchProgression :: String -> [Pitch]
interpretPitchProgression = 
  let checkResult (Right progression) = progression
      checkResult _ = error "Invalid Pitch Progression."
  in 
    checkResult . (parse parsePitchProgression "")

interpretChordProgression :: String -> [Chord]
interpretChordProgression = 
  let checkResult (Right progression) = progression
      checkResult _ = error "Invalid Chord Progression."
  in 
    checkResult . (parse parseChordProgression "")

interpretTimedChordProgression :: String -> [TimedChord]
interpretTimedChordProgression = 
  let checkResult (Right timedChordProgression) = timedChordProgression
      checkResult _ = error "Invalid Timed Chord Progression."
  in 
    checkResult . (parse parseTimedChordProgression "")

interpretNoteProgression :: String -> [Note]
interpretNoteProgression = 
  let checkResult (Right progression) = progression
      checkResult _ = error "Invalid Note Progression."
  in 
    checkResult . (parse parseNoteProgression "")

