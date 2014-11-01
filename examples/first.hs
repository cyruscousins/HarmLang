module Examples.First where

import HarmLang.Interpreter
import HarmLang.Types
import HarmLang.InitialBasis
import Test.HUnit hiding (test)


test = runTestTT tests
tests = TestList [ TestLabel "Note test" testNote,
                   TestLabel "TimedChord test" testTimedChord,
                   TestLabel "TimedChord Progression test" testTimedChordProgression,
                   TestLabel "Chord test" testChord,
                   TestLabel "Transpose Chord test" testTransposeChord]

-- parse tests
testNote = let
    got = interpretNote "A5:4"
    should = Note (Pitch (PitchClass 0) 5) (Time 4 4)
    in 
    TestCase $ assertEqual "Note test" should got 

testChord = let
    got = interpretChord "C#:n G#,B"
    should = Harmony (PitchClass 4) [Interval 7,Interval 10]
    in 
    TestCase $ assertEqual "Chord test" should got 

testTimedChord = let
    got = interpretTimedChord("Am7:4/3")
    should = TimedChord (Harmony (PitchClass 0) [Interval 3,Interval 7,Interval 10]) (Time 4 3)
    in 
    TestCase $ assertEqual "TimedChord test" should got 

testTimedChordProgression = let
    got = interpretTimedChordProgression "FMa7:4 F#o7:4/2"
    should = [TimedChord (Harmony (PitchClass 8) [Interval 4,Interval 7,Interval 11]) (Time 4 4),TimedChord (Harmony (PitchClass 9) [Interval 3,Interval 6,Interval 9]) (Time 4 2)]
    in 
    TestCase $ assertEqual "TimedChord Progression test" should got 

-- transposeChord
testTransposeChord = let
    got = transpose (interpretChord "C#:n G#,B") (Interval 7) 
    should = Harmony (PitchClass 11) [Interval 7,Interval 10]
    in 
    TestCase $ assertEqual "Chord test" should got 

-- many initial basis functions as nonfunctional as of yet, but the focus of
-- this submission is the parser. initial basis will be completed later.
