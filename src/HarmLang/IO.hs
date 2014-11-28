{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HarmLang.IO where

import HarmLang.Types
import Codec.Midi

-- http://stackoverflow.com/questions/26155872/creation-of-midi-file-in-haskell

class MIDIable a where
    outputToMIDI :: a -> String -> IO ()

instance MIDIable ChordProgression where
    outputToMIDI prog file = outputToMIDI (map (\c -> TimedChord c $ Time 1 4) prog) file

instance MIDIable NoteProgression where
    outputToMIDI prog file = let
        convertToEvents [] _ = [(0,  TrackEnd)]
        convertToEvents ((Note (Pitch (PitchClass p) o) (Time n d)):rest) start = let
            num = 24 + (12 * o) + p
            end = start + (24 * n) `div` d
            in
            (start,  NoteOn 0 num 80):(end, NoteOn 0 num 0):(convertToEvents rest end)
        in 
        writeMIDI (convertToEvents prog 0) file

instance MIDIable TimedChordProgression where
    outputToMIDI prog file = let
        convertToEvents [] _ = [(0,  TrackEnd)]
        convertToEvents ((TimedChord (Harmony (PitchClass p) intervals) (Time n d)):rest) start = let
            root = 72 + p
            end = start + (24 * n) `div` d
            others = map (\(Interval i) -> root + i) intervals
            noteson  = map (\note -> (start, NoteOn 0 note 80)) (root:others)
            notesoff = map (\note -> (end, NoteOn 0 note 0)) (root:others)
            in
            noteson ++ notesoff ++ (convertToEvents rest end)
        in 
        writeMIDI (convertToEvents prog 0) file


writeMIDI :: Track Ticks -> FilePath -> IO ()
writeMIDI track file = 
    exportFile file myMidi
    where
    myMidi = Midi { fileType = SingleTrack, 
                timeDiv  = TicksPerBeat 24, 
                tracks   = [track] }