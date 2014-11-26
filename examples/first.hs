{-# LANGUAGE  QuasiQuotes #-}

module Examples.First where

import HarmLang.Interpreter
import HarmLang.Types
import HarmLang.InitialBasis
import HarmLang.QuasiQuoter
import HarmLang.Expression

import HarmLang.Probability
import HarmLang.HarmonyDistributionModel

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
                   TestLabel "Quasi quoting: Interval" testQuasiQuotingInterval,

                   TestLabel "HarmonyDistributionModel Tests" testHarmonyDistributionModel,
                   TestLabel "Test Inference" testHarmonyDistributionModel]

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



testHarmonyDistributionModel = let
    k = 2
    hdm = buildHarmonyDistributionModel k [[hl|[Dm GM CM Dm GM CM]|], [hl|[Dm GM FM]|], [hl|[Dm GM E7]|]]
    got = [probv (distAfter hdm [hl|[Dm GM]|]) [hl|'CM'|], probv (distAfter hdm [hl|[Dm GM]|]) [hl|'FM'|], probv (distAfter hdm [hl|[Dm GM]|]) [hl|'CM7'|]]
    should = [0.5, 0.25, 0]
    in
    TestCase $ assertEqual "Harmony Distribution Model" should got

testInference = let
    k = 2
    hdm1 = buildHarmonyDistributionModel k [[hl|[CM EM Am7 Dm7 G7 CM7]|], [hl|[CM EM Am7 D7 D#o7 CM7]|], [hl|[Dm7 G7 A7]|]]
    hdm2 = buildHarmonyDistributionModel k [[hl|[CM EM Am7 Dm7 G7 CM7]|], [hl|[FMa7 F#o7 Gm7 C7 Am7 Dm7 Gm7 C7]|], [hl|[FMa7 F#o7 Gm7 C7 Am7 Dm7 Cm7 F7]|], [hl|[BbMa7 Abm7 Db7 GbMa7 Em7 A7 DMa7 Abm7 Db7 GbMa7 Gm7 C7]|], [hl|[FMa7 F#o7 Gm7 C7 Bb7 Am7 D7 Gm7 C7 FMa7 Gm7 C7]|]]
    prog = [hl|[CM EM Am7 Dm7 G7 CM7]|]
    got = inferStyle [hdm1, hdm2] prog
    should = [0.25, 1]
    in
    TestCase $ assertEqual "Harmony Distribution Model" should got


