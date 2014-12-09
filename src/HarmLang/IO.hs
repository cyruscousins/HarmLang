{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HarmLang.IO where

import HarmLang.InitialBasis
import HarmLang.Types
import Codec.Midi

tempo = 500000
granularity = 24
notelength = granularity * 2
basenote = 0

-- http://stackoverflow.com/questions/26155872/creation-of-midi-file-in-haskell

class MIDIable a where
    makeTrack :: a -> Track Ticks
    outputToMidi :: a -> String -> IO ()

instance MIDIable ChordProgression where
    makeTrack prog = makeTrack (toTimedProgression (Time 1 4) prog)

    outputToMidi prog file = writeMidi [(makeTrack prog)] file

instance MIDIable NoteProgression where
    makeTrack [] = [(0,  TrackEnd)]
    makeTrack ((Note (Pitch (PitchClass p) (Octave o)) (Time n d)):rest) = (0, TempoChange tempo):(let
        num = basenote + (12 * o) + p
        start = 0
        end = ((notelength * n) `div` d)
        in
        (start,  NoteOn 0 num 80):(end, NoteOff 0 num 0):(makeTrack rest))

    outputToMidi prog file = writeMidi [(makeTrack prog)] file

instance MIDIable TimedChordProgression where
    makeTrack [] = [(0,  TrackEnd)]
    makeTrack ((TimedChord (Harmony (PitchClass p) intervals) (Time n d)):rest) = (0, TempoChange tempo):(let
        root = basenote + (4 * 12) + p
        start = 0
        end = start + ((notelength * n) `div` d)
        others = map (\(Interval i) -> root + i) intervals
        noteson  = map (\note -> (start, NoteOn 0 note 80)) (root:others)
        notesoff = (end, NoteOff 0 root 0):(map (\note -> (0, NoteOff 0 note 0)) others)
        in
        noteson ++ notesoff ++ (makeTrack rest))

    outputToMidi prog file = writeMidi [(makeTrack prog)] file

    --TODO this creates a tempo change for every chord.  Why?
    --TODO breaks on Other chords.

writeMidi :: [Track Ticks] -> FilePath -> IO ()
writeMidi tracks file = 
    exportFile file midi
    where
    midi = Midi { fileType = SingleTrack, 
                timeDiv  = TicksPerBeat granularity, 
                tracks   = tracks}
