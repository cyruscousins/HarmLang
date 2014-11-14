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
                   -- TestLabel "Note test" testNote,
                   -- TestLabel "TimedChord test" testTimedChord,
                   -- TestLabel "TimedChord Progression test" testTimedChordProgression,
                   -- TestLabel "Chord test" testChord,
                   -- TestLabel "Transpose Chord test" testTransposeChord,
                   TestLabel "Basic quasi quoting" testQuasiQuoting]

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

-- transposeChord
testQuasiQuoting = let
    got = [hl|'A'|] 
    should = ExpPitchClass(PitchClass 0)
    in 
    TestCase $ assertEqual "Quasi quoting test" should got 