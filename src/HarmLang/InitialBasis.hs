module HarmLang.InitialBasis where

import Data.Char

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
chordNameToIntervalSet :: String -> [Interval]
chordNameToIntervalSet name =
  let 
    nToInts "5" = [7]
    nToInts "M" = [4,7]
    nToInts "m" = [3,7]
    nToInts "b5" = [4,6]
    nToInts "dim" = [3,6]
    nToInts "o" = [3,6]
    nToInts "7" = [4,7,10]
    nToInts "Ma7" = [4,7,11] --TODO delta 7
    nToInts "M7" = [4,7,11]
    nToInts "m7" = [3,7,10]
    nToInts "mMa7" = [3,7,11]
    nToInts "m7b5" = [3,6,10]
    nToInts "dim7" = [3,6,9]
    nToInts "o7" = [3,6,9]
    nToInts "9" = [2,4,7,10]
    nToInts "b9" = [1,4,7,10]
    nToInts "#9" = [3,4,7,10]
    nToInts "Ma9" = [2,4,7,11] 
    nToInts "m11" = [2,3,5,7,10]
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
--TODO: This absolutely won't work without nub and sort!!!

-- TYPECLASSES

-- Many types are transposable, this means they can be transposed in the musical sense.
-- Transposition applies to any pitched entity, and is by an interval.

--TODO Flip the order of the arguments in transpose.  This is a disaster!
class Transposable a where
  transpose :: a -> Interval -> a

instance Transposable PitchClass where
  transpose (PitchClass p) (Interval i) = PitchClass $ toEnum ((+) p i)


instance Transposable Interval where
  transpose (Interval a) (Interval b) = toEnum $ (+) a b

--This one's a bit tricky, because the octave can change too!
instance Transposable Pitch where
  transpose (Pitch pco@(PitchClass c) o) io@(Interval i) = Pitch (transpose pco io) ((+) o (floor ((/) (fromIntegral ((+) c i)) 12)))

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




--TODO strip times
