module HarmLang.EuterpeaIntegration where

import Euterpea

-- see http://goo.gl/hxwFv0
class MIDIable a where
    outputMIDI :: String -> a -> IO ()

instance MIDIable ChordProgression where
    outputMIDI file prog = outputMIDI $ map (\c -> TimedChord c $ Time 1 4) prog

-- instance MIDIable NoteProgression where
--     outputMIDI file prog = 

-- instance MIDIable TimedChordProgression where
--     outputMIDI file prog = 


pitchToMIDIMessages :: Pitch -> (MidiMessage, MidiMessage)

timedPitchesToMIDIEvents :: Time -> Time -> [Pitch] -> [MidiEvent]
timedPitchesToMIDIEvents start duration pitches = 
    concat (map (\p -> let
        (m0, m1) = pitchToMIDIMessages p
        in 
        ))



writeMIDI :: String -> [(Time, [Pitch])] -> IO ()
writeMIDI file list = 
    MIDIMessages = 
