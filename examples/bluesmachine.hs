{-# LANGUAGE  QuasiQuotes #-}

import HarmLang.Interpreter
import HarmLang.Types
import HarmLang.InitialBasis
import HarmLang.QuasiQuoter
import HarmLang.IO

progression = [hl|[CM C7 F7 C7 G7 F7 G#7]|]

main :: IO ()
main = 
  do
    putStrLn "Welcome to the Blues Buddy!"
    putStrLn "Original 12 bar blues in C."
    putStrLn . show $ progression
    putStrLn "Please enter the key to which you wish to transpose."

    newKey <- fmap interpretPitchClass getLine

    let newchords = transpose progression (intervalAB [hl|'C'|] newKey)
    outputToMidi newchords "blues.mid"

    let timedchords = map (\c -> (TimedChord c (Time 8 8))) progression
    let arpeggios = arpeggiate $ transpose timedchords (intervalAB [hl|'C'|] newKey)
    outputToMidi arpeggios "arpeggio.mid"

    writeMidi [makeTrack arpeggios, makeTrack timedchords] "jazz.mid"

    putStrLn $ "Transposed 12 bar blues, to " ++ (show newKey) ++ " and output to blues.mid"
    putStrLn $ "Arpegiatted transposed blues to arpeggio.mid"
    putStrLn $ "Together in jazz.mid"
