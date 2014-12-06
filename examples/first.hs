{-# LANGUAGE  QuasiQuotes #-}

module Examples.First where

import HarmLang.Interpreter
import HarmLang.Types
import HarmLang.InitialBasis
import HarmLang.QuasiQuoter
import HarmLang.Expression

import HarmLang.Probability
import HarmLang.HarmonyDistributionModel
import HarmLang.Priors

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
                   TestLabel "Quasi quoting: Whitespace 1" testQuasiQuotingInterval,
                   TestLabel "Quasi quoting: Whitespace 2" testQuasiQuotingInterval,

                   TestLabel "Show: Test 1" testShow1,
                   TestLabel "Show: Test 2" testShow2,
                   TestLabel "Show: Test 3" testShow3,

                   TestLabel "Test Chord Enum 1" testChordEnum1,
                   TestLabel "Test Chord Enum 2" testChordEnum2,
                   TestLabel "Test Chord Enum 3" testChordEnum3,
                   TestLabel "Test Chord Enum 4" testChordEnum4,

                   TestLabel "HarmonyDistributionModel Tests" testHarmonyDistributionModel,
                   TestLabel "Test Inference" testInference,
                   TestLabel "Key Agnosticism" testKeyAgnosticism,
                   TestLabel "Laplacian Prior" testLaplacianPrior

                 ]

-- parse tests
testNote = let
    got = interpretNote "A@5:4"
    should = Note (Pitch (PitchClass 0) (Octave 5)) (Time 4 4)
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
    should = [Pitch (PitchClass 0) (Octave 5), Pitch (PitchClass 4) (Octave 6)]
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
    should = [Note (Pitch (PitchClass 2) (Octave 5)) (Time 3 4), Note (Pitch (PitchClass 5) (Octave 6)) (Time 3 2)]
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
    should = [Note (Pitch (PitchClass 2) (Octave 5)) (Time 3 4), Note (Pitch (PitchClass 5) (Octave 6)) (Time 3 2)]
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

-- Quasiquoting with whitespace
testQuasiQuotingWhitespace1 = let
    got = [hl|

   	'A'      |]
    should = PitchClass 0
    in
    TestCase $ assertEqual "QuasiQuoting Whitespace" should got

testQuasiQuotingWhitespace2 = let
    got = [hl|   [A@7:4] 


  |]
    should = [Note (Pitch (PitchClass 0) (Octave 4)) (Time 7 4)]
    in
    TestCase $ assertEqual "QuasiQuoting Whitespace 2" should got


-- Test show

-- Test show
testShow1 = let
    original = [hl|'C#'|]
    reparsed = interpretPitchClass (show original)
    in
    TestCase $ assertEqual "Show identity 1" original reparsed

testShow2 = let
    original = [hl| [C#m7:4/4 REST:4/4] |]
    reparsed = interpretTimedChordProgression (show original)
    in
    TestCase $ assertEqual "Show identity 2" original reparsed

testShow3 = let
    original = [hl| [A@4:2/4  B@4:1/4 C@4:1/4] |]
    reparsed = interpretNoteProgression (show original)
    in
    TestCase $ assertEqual "Show identity 2" original reparsed

--Test chord enum

testChordEnum1 = let
    original = map Interval [1..11]
    new = (toEnum . fromEnum) original
    in
    TestCase $ assertEqual "Test Chord Enum 1" original new

testChordEnum2 = let
    original = Harmony (PitchClass 0) (map Interval [1..11])
    new = (toEnum . fromEnum) original
    in
    TestCase $ assertEqual "Test Chord Enum 2" original new

testChordEnum3 = TestCase $ assertEqual "Test Chord Enum 3" (length allChordTypes) numChordTypes

testChordEnum4 = TestCase $ assertEqual "Test Chord Enum 4" (length allChords) numChords

--HDM
testHarmonyDistributionModel = let
    k = 2
    hdm = buildHarmonyDistributionModel k [[hl|[Dm GM CM Dm GM CM]|], [hl|[Dm GM FM]|], [hl|[Dm GM E7]|]]
    got = [probv (distAfter hdm [hl|[Dm GM]|]) [hl|'CM'|], probv (distAfter hdm [hl|[Dm GM]|]) [hl|'FM'|], probv (distAfter hdm [hl|[Dm GM]|]) [hl|'CM7'|]]
    should = [0.5, 0.25, 0]
    in
    TestCase $ assertEqual "Harmony Distribution Model" should got

--Inference problem
testInference = let
    k = 3
    hdm1 = buildHarmonyDistributionModel k [[hl|[FM7 CM EM Am7 Dm7 G7 CM7]|], [hl|[FM7 CM EM Am7 D7 D#o7 CM7]|], [hl|[EbM7 Dm7 G7 A7]|]]
    hdm2 = buildHarmonyDistributionModel k [[hl|[FM7 CM EM Am7 Dm7 G7 CM7]|], [hl|[E7 FMa7 F#o7 Gm7 C7 Am7 Dm7 Gm7 C7]|], [hl|[E7 FMa7 F#o7 Gm7 C7 Am7 Dm7 Cm7 F7]|], [hl|[F7 BbMa7 Abm7 Db7 GbMa7 Em7 A7 DMa7 Abm7 Db7 GbMa7 Gm7 C7]|], [hl|[E7 FMa7 F#o7 Gm7 C7 Bb7 Am7 D7 Gm7 C7 FMa7 Gm7 C7]|]]
    prog = [hl|[CM EM Am7 Dm7 G7 CM7]|]
--    prog = [hl|[CM EM Am7 Dm7]|]
    got = inferStyle [hdm1, hdm2] prog
    should = [0.5, 1]
    in
    TestCase $ assertEqual "Inference test" should got

--Key Agnosticism
testKeyAgnosticism = let
    k = 2
    hdm = buildHarmonyDistributionModel k [[hl| [AM BM CM] |], [hl| [A#M B#M C#M] |], [hl| [BM C#M Dm] |]]
    query = [hl| [BM C#M] |]
    probs = [([hl| 'DM' |], 2.0 / 3.0), ([hl| 'Dm' |], 1.0 / 3.0) ]
    dist = distAfter hdm query
    in
    TestCase $ assertEqual "Key agnosticism" True (all (\ (val, theProb) -> (==) theProb (probv dist val)) probs)--TODO probably a better way than assertEqual

{-
--Intractable.
testLaplacianPrior = let
    k = 2
    hdm = buildHarmonyDistributionModelWithPrior k laplacianPrior 1.0 [[hl| [AM BM CM] |], [hl| [AM BM Cm] |]]
    query = [hl| [AM BM] |]
    dist = distAfter hdm query 
    should = (0.5 + (1.0 / (fromIntegral numChords))) / 2.0
    got = probv dist [hl| 'CM' |]
    in
    TestCase $ assertEqual ("Laplacian Prior Test: should = " ++ (show should) ++ ", got = " ++ (show got)) should got
-}

testLaplacianPrior = let
    k = 2
    prior = chordLimitedLaplacianPrior [map Interval [4, 7]]
    hdm = buildHarmonyDistributionModelWithPrior k prior 2.0 [[hl| [AM BM CM] |], [hl| [AM BM Cm] |]]
    queries = [ [hl| [AM BM] |], [hl| [Bbm7b5 EmMa7] |] ]
    dists = map (distAfter hdm) queries
    should = [(0.5 + (1.0 / 12.0)) / 2.0, 1.0 / 12.0]
    got = map (\dist -> probv dist [hl| 'CM' |]) dists
    in
    TestCase $ assertEqual ("Laplacian Prior Test: should = " ++ (show should) ++ ", got = " ++ (show got)) should got



