{-# LANGUAGE  QuasiQuotes #-}

import HarmLang.Interpreter
import HarmLang.Types
import HarmLang.InitialBasis
import HarmLang.QuasiQuoter
import HarmLang.IO

progression = [hl|[CM C7 F7 C7 G7 F7 G#7]|]

arpeggiate :: ChordProgression -> NoteProgression
arpeggiate [] = []
arpeggiate ((Harmony root intervals):rest) = let 
    rootnote = Note (Pitch root (Octave 3)) (Time 1 8)
    others = map (\i -> transpose rootnote i) intervals
    in
    (rootnote:others) ++ arpeggiate rest




main :: IO ()
main = 
  do
    putStrLn "Welcome to the Blues Buddy!"
    putStrLn "Original 12 bar blues in C."
    putStrLn . show $ progression
    putStrLn "Please enter the key to which you wish to transpose."

    newKey <- fmap interpretPitchClass getLine
    putStrLn $ "Transposed 12 bar blues, to " ++ (show newKey) ++ " and output to blues.mid"

    outputToMidi (arpeggiate $ transpose progression (intervalAB [hl|'C'|] newKey)) "arpeggio.mid"
    outputToMidi (transpose progression (intervalAB [hl|'C'|] newKey)) "blues.mid"
    putStrLn $ "Arpegiatted transposed blues to arpeggio.mid"
