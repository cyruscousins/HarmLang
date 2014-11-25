{-# LANGUAGE DeriveDataTypeable #-}

module HarmLang.Types where
import Data.Typeable
import Data.Data


-- An absolute class of pitches (element of Z12).
data PitchClass = PitchClass Int
  deriving (Data, Typeable)

instance Eq PitchClass where
  (==) (PitchClass a) (PitchClass b) = (Prelude.==) (mod a 12) (mod b 12)
  -- (<=) (PitchClass a) (PitchClass b) = (Prelude.<=) (mod a 12) (mod b 12)

instance Enum PitchClass where
  toEnum intval = PitchClass $ mod intval 12
  fromEnum (PitchClass p) = p

instance Show PitchClass where
  show (PitchClass p) = ["A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#"] !! (mod p 12)


-- A relative change in pitches.
data Interval = Interval Int  --TODO Better show?
  deriving (Show, Data, Typeable)


--TODO How?  Won't work with the other ==
instance Eq Interval where
  (==) (Interval a) (Interval b) = (Prelude.==) (mod a 12) (mod b 12)

instance Ord Interval where
  (<=) (Interval a) (Interval b) = (Prelude.<=) (mod a 12) (mod b 12)

instance Enum Interval where
	toEnum intval = Interval $ mod intval 12
	fromEnum (Interval p) = p

-- A pitchclass and an octave.
data Pitch = Pitch PitchClass Int
  deriving (Show, Eq, Data, Typeable)

-- Time is expressed as a fraction of a whole note.  The fraction is expected to be reduced.
data Time = Time Int Int 
  deriving (Show, Eq, Data, Typeable)

--instance Eq Time where
--  (==) (Time n1 d1) (Time n2 d2) = (Prelude.==) ((/) (toRational n1) (toRational d1)) ((/) (toRational n2) (toRational d2))

instance Ord Time where
  (<=) (Time n1 d1) (Time n2 d2) = 
    (Prelude.<=) 
    ((/) (toRational n1) (toRational d1)) 
    ((/) (toRational n2) (toRational d2))

data Note = Note Pitch Time deriving (Show, Eq, Data, Typeable) --TODO At some point we want to add the option for a rest in here as well.

-- Chord datatype is represented as a root and a list of intervals from the root or a special case.
-- The intervals are expected to be ordered and without repetition.
data Chord = Harmony PitchClass [Interval] | Other String deriving (Show, Eq, Data, Typeable)


data TimedChord = TimedChord Chord Time deriving (Show, Eq, Data, Typeable)


--TODO: Intervals, Times, and Harmonies have a logical normative representation, as well as other representations that aren't so logical.  Need to limit functionality so as to keep them in the normal forms.

type ChordProgression = [Chord]
type TimedChordProgression = [TimedChord]

