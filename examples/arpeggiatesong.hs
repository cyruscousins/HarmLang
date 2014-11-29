{-# LANGUAGE  QuasiQuotes #-}

import HarmLang.Interpreter
import HarmLang.Types
import HarmLang.InitialBasis
import HarmLang.QuasiQuoter
import HarmLang.IO
import HarmLang.ChordProgressionDatabase

main :: IO ()
main = 
  do
    db <- loadChordProgressionDatabase "res/progressions.txt"
    putStrLn "Enter the song."
    song_name <- getLine
    putStrLn "Enter the progression name."
    prog_name <- getLine
    let (timedchords:_) = getProgressionsInCategoryByCriterion db prog_name song_name
    let arpeggios = arpeggiate timedchords
    let midi_name = song_name ++ ".mid"
    writeMidi [makeTrack arpeggios, makeTrack timedchords] midi_name
    putStrLn ("Outputted in " ++ midi_name)
