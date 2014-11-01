module HarmLang.Types where

-- An absolute class of pitches (element of Z12).
data PitchClass = PitchClass Int

instance Eq PitchClass where
  (==) (PitchClass a) (PitchClass b) = (Prelude.==) (mod a 12) (mod b 12)
  -- (<=) (PitchClass a) (PitchClass b) = (Prelude.<=) (mod a 12) (mod b 12)

instance Enum PitchClass where
  toEnum intval = PitchClass $ mod intval 12
  fromEnum (PitchClass p) = p

instance Show PitchClass where
  show (PitchClass p) = ["A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#"] !! (mod p 12)


--A fancy experiment, but I decided that I wanted operators on the PitchClass type to be a bit different.

{-
--Converting to and from integers
--Is there a cleaner way to do this?  Maybe with templates?
class PitchClassCompatible a where
fromPitchClass :: PitchClass -> a
toPitchClass :: a -> PitchClass

instance PitchClassCompatible Int where
fromPitchClass (PitchClass val) = val
toPitchClass (val) = PitchClass val

liftUnaryPC :: (Int -> Int) -> PitchClass -> PitchClass
liftUnaryPC unop (PitchClass p) = PitchClass $ mod (unop p) 12

liftBinaryPC :: (Int -> Int -> Int) -> PitchClass -> PitchClass -> PitchClass
liftBinaryPC binop (PitchClass p1) (PitchClass p2) = PitchClass $ mod (binop p1 p2) 12

instance Num PitchClass where
(+) = liftBinaryPC (Prelude.+)
(-) = liftBinaryPC (Prelude.-)
(*) = liftBinaryPC (Prelude.*)
negate = liftBinaryPC (12 Prelude.-)
fromInteger int = PitchClass int
signum _ = error "PitchClasses lack the signum operator."
abs _ = error "PitchClasses lack the abs operator."

-}

-- A relative change in pitches.
data Interval = Interval Int deriving (Show) --TODO Better show?


--TODO How?  Won't work with the other ==
instance Eq Interval where
  (==) (Interval a) (Interval b) = (Prelude.==) (mod a 12) (mod b 12)

instance Ord Interval where
  (<=) (Interval a) (Interval b) = (Prelude.<=) (mod a 12) (mod b 12)

instance Enum Interval where
	toEnum intval = Interval $ mod intval 12
	fromEnum (Interval p) = p

-- A pitchclass and an octave.
data Pitch = Pitch PitchClass Int deriving (Show, Eq)

-- Time is expressed as a fraction of a whole note.  The fraction is expected to be reduced.
data Time = Time Int Int deriving (Show, Eq)

--instance Eq Time where
--  (==) (Time n1 d1) (Time n2 d2) = (Prelude.==) ((/) (toRational n1) (toRational d1)) ((/) (toRational n2) (toRational d2))

instance Ord Time where
  (<=) (Time n1 d1) (Time n2 d2) = (Prelude.<=) ((/) (toRational n1) (toRational d1)) ((/) (toRational n2) (toRational d2))

data Note = Note Pitch Time deriving (Show, Eq) --TODO At some point we want to add the option for a rest in here as well.

-- Chord datatype is represented as a root and a list of intervals from the root or a special case.
-- The intervals are expected to be ordered and without repetition.
data Chord = Harmony PitchClass [Interval] | Rest | Begin | End deriving (Show, Eq)

data TimedChord = TimedChord Chord Time deriving (Show, Eq)


--TODO: Intervals, Times, and Harmonies have a logical normative representation, as well as other representations that aren't so logical.  Need to limit functionality so as to keep them in the normal forms.
