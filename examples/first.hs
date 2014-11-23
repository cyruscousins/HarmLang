{-# LANGUAGE  QuasiQuotes #-}

module Examples.First where

import HarmLang.Interpreter
import HarmLang.Types
import HarmLang.InitialBasis
import HarmLang.QuasiQuoter
import HarmLang.Expression

import Test.HUnit hiding (test)


test = runTestTT tests
tests = TestList [ 
                   TestLabel "Note test" testNote,
                   TestLabel "TimedChord test" testTimedChord,
                   TestLabel "TimedChord Progression test" testTimedChordProgression,
                   TestLabel "Chord test" testChord,
                   TestLabel "Transpose Chord test" testTransposeChord,
                   TestLabel "Quasi quoting: Pitch Class" testQuasiQuotingPitchClass,
                   TestLabel "Quasi quoting: Pitch" testQuasiQuotingPitch,
                   TestLabel "Quasi quoting: Timed Chord" testQuasiQuotingTimedChord,
                   TestLabel "Quasi quoting: Note" testQuasiQuotingNote,
                   TestLabel "Quasi quoting: Chord" testQuasiQuotingChord,
                   TestLabel "Quasi quoting: Note Progression" testQuasiQuotingNoteProgression,
                   TestLabel "Quasi quoting: Timed Chord Progression" testQuasiQuotingTimedChordProgression,
                   TestLabel "Quasi quoting: Interval" testQuasiQuotingInterval]

-- parse tests
testNote = let
    got = interpretNote "A@5:4"
    should = Note (Pitch (PitchClass 0) 5) (Time 4 4)
    in 
    TestCase $ assertEqual "Note test" should got 

testChord = let
    got = interpretChord "C#[G# B]"
    should = Harmony (PitchClass 4) [Interval 7,Interval 10]
    in 
    TestCase $ assertEqual "Chord test" should got 

testTimedChord = let
    got = interpretTimedChord("Am7:4/3")
    should = TimedChord (Harmony (PitchClass 0) [Interval 3,Interval 7,Interval 10]) (Time 4 3)
    in 
    TestCase $ assertEqual "TimedChord test" should got 

testTimedChordProgression = let
    got = interpretTimedChordProgression "[FMa7:4 F#o7:4/2]"
    should = [TimedChord (Harmony (PitchClass 8) [Interval 4,Interval 7,Interval 11]) (Time 4 4),TimedChord (Harmony (PitchClass 9) [Interval 3,Interval 6,Interval 9]) (Time 4 2)]
    in 
    TestCase $ assertEqual "TimedChord Progression test" should got 

-- transposeChord
testTransposeChord = let
    got = transpose (interpretChord "C#[G# B]") (Interval 7) 
    should = Harmony (PitchClass 11) [Interval 7,Interval 10]
    in 
    TestCase $ assertEqual "Chord test" should got 


-- Quasiquoting on pitchclasses
testQuasiQuotingPitchClass = let
    got = [[hl|'A'|], [hl|'Ab'|]] 
    should = [PitchClass 0, PitchClass 11]
    in 
    TestCase $ assertEqual "Quasi quoting PitchClass test" should got 

-- Quasiquoting on pitches
testQuasiQuotingPitch = let
    got = [[hl|'A@5'|], [hl|'C#@6'|]] 
    should = [Pitch (PitchClass 0) 5 , Pitch (PitchClass 4) 6]
    in 
    TestCase $ assertEqual "Quasi quoting Pitch test" should got 

-- Quasiquoting on timed chords
testQuasiQuotingTimedChord = let
    got = [[hl|'AM:4'|], [hl|'C#m7b5:7'|]] 
    should = [TimedChord (Harmony (PitchClass 0) [(Interval 4), (Interval 7)]) (Time 4 4), TimedChord (Harmony (PitchClass 4) [(Interval 3), (Interval 6), (Interval 10)]) (Time 7 4)]
    in 
    TestCase $ assertEqual "Quasi quoting TimedChord test" should got 

-- Quasiquoting on notes
testQuasiQuotingNote = let
    got = [[hl|'B@5:3'|], [hl|'D@6:3/2'|]] 
    should = [Note (Pitch (PitchClass 2) 5) (Time 3 4), Note (Pitch (PitchClass 5) 6) (Time 3 2)]
    in 
    TestCase $ assertEqual "Quasi quoting TimedChord test" should got 

-- Quasiquoting on chords
testQuasiQuotingChord = let
    got = [[hl|'AM'|], [hl|'C#m7b5'|]] 
    should = [Harmony (PitchClass 0) [(Interval 4), (Interval 7)], Harmony (PitchClass 4) [(Interval 3), (Interval 6), (Interval 10)]]
    in 
    TestCase $ assertEqual "Quasi quoting Chord test" should got 

-- 
testQuasiQuotingNoteProgression = let
    got = [hl|[B@5:3 D@6:3/2]|] 
    should = [Note (Pitch (PitchClass 2) 5) (Time 3 4), Note (Pitch (PitchClass 5) 6) (Time 3 2)]
    in 
    TestCase $ assertEqual "Quasi quoting NoteProgression test" should got 

--
testQuasiQuotingTimedChordProgression = let
    got = [hl|[FMa7:4 F#o7:4/2]|]
    should = [TimedChord (Harmony (PitchClass 8) [Interval 4,Interval 7,Interval 11]) (Time 4 4),TimedChord (Harmony (PitchClass 9) [Interval 3,Interval 6,Interval 9]) (Time 4 2)]
    in 
    TestCase $ assertEqual "Quasi quoting TimedChord Progression test" should got 

-- Quasiquoting on intervals
testQuasiQuotingInterval = let
    got = [[hl|'4'|], [hl|'8'|]]
    should = [Interval 4, Interval 8]
    in 
    TestCase $ assertEqual "Quasi quoting Interval test" should got 



--TODO:
--Do these:
{-
                   TestLabel "Quasi quoting: Timed Chord Progression" testQuasiQuotingTimedChordProgression,
                   TestLabel "Quasi quoting: Interval" testQuasiQuotingInterval]
-}


