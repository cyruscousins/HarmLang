{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverlappingInstances, TypeSynonymInstances #-}

module HarmLang.Types where

import Data.Typeable
import Data.Data
import Data.List
import Data.Ratio

-- An absolute class of pitches (element of Z12).
data PitchClass = PitchClass Int
  deriving (Data, Typeable)

instance Eq PitchClass where
  (==) (PitchClass a) (PitchClass b) = (Prelude.==) (mod a 12) (mod b 12)

instance Enum PitchClass where
  toEnum intval = PitchClass $ mod intval 12
  fromEnum (PitchClass p) = p

instance Show PitchClass where
  show (PitchClass p) = ["A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#"] !! (mod p 12)


-- A relative change in pitches.
data Interval = Interval Int  --TODO Better show?
  deriving (Data, Typeable)

instance Show Interval where
  show (Interval interval) = show interval

--TODO How?  Won't work with the other ==
instance Eq Interval where
  (==) (Interval a) (Interval b) = (Prelude.==) (mod a 12) (mod b 12)

instance Ord Interval where
  (<=) (Interval a) (Interval b) = (Prelude.<=) (mod a 12) (mod b 12)

instance Enum Interval where
  toEnum intval = Interval $ mod intval 12
  fromEnum (Interval p) = p

data Octave = Octave Int
  deriving (Eq, Ord, Data, Typeable)

instance Show Octave where
  show (Octave o) = show o

instance Enum Octave where
  toEnum = Octave
  fromEnum (Octave o) = o

-- A pitchclass and an octave.
data Pitch = Pitch PitchClass Octave
  deriving (Eq, Data, Typeable)

instance Ord Pitch where
  (<=) (Pitch (PitchClass pc1) (Octave oct1)) (Pitch (PitchClass pc2) (Octave oct2)) = (Prelude.<=) (pc1 + (oct1 * 12)) (pc2 + (oct2 * 12))

instance Show Pitch where
  show (Pitch pc oct) = (show pc) ++ "@" ++ (show oct)

-- Time is expressed as a fraction of a whole note.
data Time = Time (Ratio Int)
  deriving (Eq, Data, Typeable)

instance Show Time where
  show (Time rat) = (show (numerator rat)) ++ "/" ++ (show (denominator rat))

instance Ord Time where
  (<=) (Time t0) (Time t1) = (Prelude.<=) t0 t1

data Note = Note Pitch Time deriving (Eq, Data, Typeable) --TODO At some point we want to add the option for a rest in here as well.

instance Show Note where
  show (Note pitch time) = (show pitch) ++ ":" ++ (show time)

-- Chord datatype is represented as a root and a list of intervals from the root or a special case.
-- The intervals are expected to be ordered and without repetition.
data Chord = Harmony PitchClass [Interval] | Other String deriving (Eq, Data, Typeable)

instance Show Chord where
  show (Harmony pc ints) = (show pc) ++ (show ints)
  show (Other str) = str

data TimedChord = TimedChord Chord Time deriving (Eq, Data, Typeable)

instance Show TimedChord where
  show (TimedChord chord time) = (show chord) ++ ":" ++ (show time)

type ChordProgression = [Chord] 
type TimedChordProgression = [TimedChord]
type NoteProgression = [Note]
type ChordType = [Interval] --TODO should be a fully fledged Data, and should check conditions.

hlArrayStr :: (Show a) => [a] -> String
hlArrayStr arr = "[" ++ (intercalate " " (map show arr)) ++ "]"

--Boilerplate to override default list show, to get HarmLang types to show values that can be interpreted.

instance Show ChordType where
  show arr = hlArrayStr arr

instance Show ChordProgression where --TODO TypeSynonymInstances?
  show arr = hlArrayStr arr

instance Show TimedChordProgression where
  show arr = hlArrayStr arr

instance Show NoteProgression where
  show arr = hlArrayStr arr


