{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HarmLang.IO where

import HarmLang.Types
import Codec.Midi

tempo = 500000
granularity = 24
notelength = granularity * 2

-- http://stackoverflow.com/questions/26155872/creation-of-midi-file-in-haskell

class MIDIable a where
    outputToMidi :: a -> String -> IO ()

instance MIDIable ChordProgression where
    outputToMidi prog file = outputToMidi (map (\c -> TimedChord c $ Time 1 4) prog) file

instance MIDIable NoteProgression where
    outputToMidi prog file = let
        convertToEvents [] = [(0,  TrackEnd)]
        convertToEvents ((Note (Pitch (PitchClass p) (Octave o)) (Time n d)):rest) = let
            num = 24 + (12 * o) + p
            start = 0
            end = ((notelength * 2 * n) `div` d)
            in
            (start,  NoteOn 0 num 80):(end, NoteOn 0 num 0):(convertToEvents rest)
        in 
        writeMIDI (convertToEvents prog) file

instance MIDIable TimedChordProgression where
    outputToMidi prog file = let
        convertToEvents [] = [(0,  TrackEnd)]
        convertToEvents ((TimedChord (Harmony (PitchClass p) intervals) (Time n d)):rest) = let
            root = 72 + p
            start = 0
            end = start + ((notelength * n) `div` d)
            others = map (\(Interval i) -> root + i) intervals
            noteson  = map (\note -> (start, NoteOn 0 note 80)) (root:others)
            notesoff = map (\note -> (end, NoteOn 0 note 0)) (root:others)
            in
            noteson ++ notesoff ++ (convertToEvents rest)
        in 
        writeMIDI (convertToEvents prog) file


writeMIDI :: Track Ticks -> FilePath -> IO ()
writeMIDI track file = 
    exportFile file midi
    where
    midi = Midi { fileType = SingleTrack, 
                timeDiv  = TicksPerBeat granularity, 
                tracks   = [(0, TempoChange tempo):track]}