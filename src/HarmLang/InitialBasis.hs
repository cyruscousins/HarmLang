{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module HarmLang.InitialBasis where

import Data.Char
import Data.Bits
import qualified Data.List

import HarmLang.Types
import HarmLang.Utility

-- TEXT FUNCTIONS:
-- Used by the parser, but may be useful elsewhere.

interpretNoteChar :: Char -> PitchClass
interpretNoteChar 'A' = PitchClass 0
interpretNoteChar 'B' = PitchClass 2
interpretNoteChar 'C' = PitchClass 3
interpretNoteChar 'D' = PitchClass 5
interpretNoteChar 'E' = PitchClass 7
interpretNoteChar 'F' = PitchClass 8
interpretNoteChar 'G' = PitchClass 10
interpretNoteChar other = error $ "Invalid pitch class \"" ++ [other] ++ "\"."

interpretPitchClassModifierChar :: Char -> (PitchClass -> PitchClass)
interpretPitchClassModifierChar 'b' = pred
interpretPitchClassModifierChar '#' = succ

interpretNamedInterval :: String -> Interval
interpretNamedInterval "0th" = Interval 0
interpretNamedInterval "u" = Interval 0
interpretNamedInterval "unison" = Interval 0
interpretNamedInterval "m2nd" = Interval 1
interpretNamedInterval "-2nd" = Interval 1
interpretNamedInterval "minorsecond" = Interval 1
interpretNamedInterval "2nd" = Interval 2
interpretNamedInterval "second" = Interval 2
interpretNamedInterval "m3rd" = Interval 3
interpretNamedInterval "-3rd" = Interval 3
interpretNamedInterval "minorthird" = Interval 3
interpretNamedInterval "3rd" = Interval 4
interpretNamedInterval "third" = Interval 4
interpretNamedInterval "4th" = Interval 5
interpretNamedInterval "fourth" = Interval 5
interpretNamedInterval "+4th" = Interval 6
interpretNamedInterval "#4th" = Interval 6
interpretNamedInterval "augmentedfourth" = Interval 6
interpretNamedInterval "-5th" = Interval 6
interpretNamedInterval "d5th" = Interval 6
interpretNamedInterval "o5th" = Interval 6
interpretNamedInterval "b5th" = Interval 6
interpretNamedInterval "diminishedfifth" = Interval 6
interpretNamedInterval "tritone" = Interval 6
interpretNamedInterval "5th" = Interval 7
interpretNamedInterval "fifth" = Interval 7
interpretNamedInterval "+5th" = Interval 8
interpretNamedInterval "augmentedfifth" = Interval 8
interpretNamedInterval "m6th" = Interval 8
interpretNamedInterval "-6th" = Interval 8
interpretNamedInterval "minorsixth" = Interval 8
interpretNamedInterval "6th" = Interval 9
interpretNamedInterval "sixth" = Interval 9
interpretNamedInterval "7th" = Interval 10
interpretNamedInterval "d7th" = Interval 10
interpretNamedInterval "dom7th" = Interval 10
interpretNamedInterval "b7th" = Interval 10
interpretNamedInterval "seventh" = Interval 10
interpretNamedInterval "minorseventh" = Interval 10
interpretNamedInterval "dominantseventh" = Interval 10
interpretNamedInterval "ma7th" = Interval 11
interpretNamedInterval "majorseventh" = Interval 11
interpretNamedInterval "octave" = Interval 12
interpretNamedInterval other = error $ "Invalid interval \"" ++ other ++ "\"."

interpretNamedIntervalCaseInsensitive :: String -> Interval
interpretNamedIntervalCaseInsensitive s = interpretNamedInterval $ map toLower s


-- Take the name of a chord and convert to an interval set
chordNameToIntervalSet :: String -> ChordType
chordNameToIntervalSet name =
  let 
    nToInts "5" = [7]
    nToInts "M" = [4,7]
    nToInts "m" = [3,7]
    nToInts "b5" = [4,6]
    nToInts "dim" = [3,6]
    nToInts "o" = [3,6]
    nToInts "+" = [4,8]
    nToInts "7" = [4,7,10]
    nToInts "Ma7" = [4,7,11] --TODO delta 7
    nToInts "ma7" = [4,7,11]
    nToInts "M7" = [4,7,11]
    nToInts "m7" = [3,7,10]
    nToInts "mMa7" = [3,7,11]
    nToInts "m7b5" = [3,6,10]
    nToInts "dim7" = [3,6,9]
    nToInts "o7" = [3,6,9]
    nToInts "+7" = [4,8,10]
    nToInts "9" = [2,4,7,10]
    nToInts "b9" = [1,4,7,10]
    nToInts "#9" = [3,4,7,10]
    nToInts "Ma9" = [2,4,7,11] 
    nToInts "m11" = [3,5,7,10]
    nToInts "7b6" = [4,7,8,10]
    nToInts "13" = [2,4,7,9,10]
    nToInts "Ma13" = [2,4,7,9,11]
    nToInts _ = error "Invalid chord name."
  in map Interval $ nToInts name
  
-- CONVENIENCE FUNCTIONS

intervalAB :: PitchClass -> PitchClass -> Interval
intervalAB (PitchClass a) (PitchClass b) = Interval (mod ((-) b a) 12)

inverse :: Interval -> Interval
inverse (Interval a) = Interval (12 - a)

toChord :: PitchClass -> [PitchClass] -> Chord
toChord root rest = Harmony root $ sortedUnique (map (intervalAB $ root) rest)

getNotesFromChord :: Chord -> [PitchClass]
getNotesFromChord (Harmony r intervals) = r : (map (transpose r) intervals)
getNotesFromChord _ = []

--Get all the notes in a chord progression.
getNotesFromChordProgression :: ChordProgression -> [PitchClass]
getNotesFromChordProgression cp = Data.List.nub . concat $ map getNotesFromChord cp

-- TYPECLASSES

-- Many types are transposable, this means they can be transposed in the musical sense.
-- Transposition applies to any pitched entity, and is by an interval.

--TODO Flip the order of the arguments in transpose.  This is a disaster!
class Transposable a where
  transpose :: a -> Interval -> a

instance Transposable PitchClass where
  transpose (PitchClass p) (Interval i) = PitchClass $ toEnum $ (p + i) `mod` 12


instance Transposable Interval where
  transpose (Interval a) (Interval b) = toEnum $ (+) a b

--This one's a bit tricky, because the octave can change too!
instance Transposable Pitch where
  transpose (Pitch pco@(PitchClass c) (Octave o)) io@(Interval i) = let
    o_delta = if (c + i) > 0 then (c + i) `div` 12 else ((-1) * (abs(c + i) `div` 12))
    in
    Pitch (transpose pco (Interval $ i)) (Octave $ o + o_delta)

instance Transposable Note where
  transpose (Note p t) i = Note (transpose p i) t

instance Transposable Chord where
  transpose (Harmony pc chordIntervals) transposeInterval = Harmony (transpose pc transposeInterval) chordIntervals
  transpose other _ = other

instance Transposable TimedChord where
  transpose (TimedChord c t) i = TimedChord (transpose c i) t

--And for the final coup de grâce, arrays of transposables are transposable componentwise.

instance Transposable a => Transposable [a] where
  transpose list interval = map (flip transpose interval) list 

-- Many types have some sense of time

class Timed a where
  askTime :: a -> Time
  reTime :: a -> Time -> a

-- The Time type is trivially timed.

instance Timed Time where
  askTime time = time
  reTime time newTime = newTime

instance Timed Note where
  askTime (Note pitch time) = time
  reTime (Note pitch time) newTime = Note pitch newTime

instance Timed TimedChord where
  askTime (TimedChord chord time) = time
  reTime (TimedChord chord time) newTime = TimedChord chord newTime

-- TODO make lists of timed things timed as well?

-- TODO time stretch function?  Need a way to do this and keep reduced fractions.

--Better way to do this?
timeTop (Time a b) = a 

--Take the requested length of time worth of the given list.
--TODO implement this well (only works for things in quarter notes now).
takeByTime :: Timed a => [a] -> Time -> [a]
takeByTime (item:list) (Time num den) = if (>) num 0 then [item] else item : (takeByTime list (Time ((-) num (timeTop . askTime $ item)) den))
takeByTime _ _ = []

--TODO cleaner syntax?
--timeSubtract :: Time -> Time 

--TODO
--timeMultiplier a Timed :: -> Time -> a

--Add timing information to a progression
toTimedProgression :: Time -> ChordProgression -> TimedChordProgression
toTimedProgression time prog = (map (\c -> TimedChord c time) prog)

--Remove timing information from a progression.
toUntimedProgression :: TimedChordProgression -> ChordProgression
toUntimedProgression prog = (map (\ (TimedChord c _) -> c) prog)


--Enumeration of Chords

instance Enum [Interval] where
  toEnum intVal = map Interval $ filter (\a -> ((/=) 0) $ (.&.) intVal (2^(a-1))) [1..11]
  fromEnum intervals = sum $ map (\ interval -> 2 ^ (fromEnum interval - 1)) intervals

--Chord enum
instance Enum Chord where
  toEnum intVal = Harmony (PitchClass ((div) intVal (numChordTypes))) (toEnum intVal)
  fromEnum (Harmony (PitchClass pc) intervals) = pc * (numChordTypes) + (fromEnum intervals)

numChordTypes :: Int
numChordTypes = 2 ^ 11 --Chords implicitly have 1 note.

allChordTypes :: [ChordType]
allChordTypes = [[]..(map Interval [1..11])]

numChords :: Int 
numChords = 12 * numChordTypes

allChords :: [Chord]
allChords = [(Harmony (PitchClass 0) [])..(Harmony (PitchClass 11) (map Interval [1..11]))]

allIntervals :: [Interval]
allIntervals = map Interval [0..11]

allNonzeroIntervals :: [Interval]
allNonzeroIntervals = map Interval [1..11]

allPitchClasses :: [PitchClass]
allPitchClasses = map PitchClass [0..11]


--Get all the inversions of a chord
chordInversions :: Chord -> [Chord]
chordInversions chord@(Harmony pc ints) =
  let
    allNotes = getNotesFromChord chord
    allNoteRotations = allRotations allNotes
  in
    map (\l -> toChord (head l) (tail l)) allNoteRotations
chordInversions other = [other]

--Are the provided chords inversions of one another?
isInversion :: Chord -> Chord -> Bool
isInversion c1 c2 = elem c2 (chordInversions c1)







arpeggiate :: TimedChordProgression -> NoteProgression
arpeggiate [] = []
arpeggiate ((TimedChord (Harmony root intervals) (Time n d)):rest) = let 
    rootnote = Note (Pitch root (Octave 3)) (Time 1 d)
    others = map (\i -> transpose rootnote i) intervals
    notes = take n (cycle $ rootnote:others)
    in
    notes ++ arpeggiate rest



