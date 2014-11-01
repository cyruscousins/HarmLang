module HarmLang.Parser where

import Language.Haskell.TH.Quote
import Language.Haskell.TH

import Data.Tuple
import Data.Char
import Data.List

import qualified Control.Applicative as Ctrl

import Text.ParserCombinators.Parsec

--Harmlang imports
import HarmLang.Types
import HarmLang.InitialBasis

--PARSER COMPONENTS:

--Basic whitespace parser
parseWhiteSpace :: GenParser Char st ()
parseWhiteSpace =
  do
    --TODO Maybe need to ensure at least one? (use many1)
    _ <- char ' ' --TODO I pray that there is a better way.
    --whocares <- many $ oneOf " \t\n\r" --Why does this not work?
    return () --return the unit (don't care about the whitespace).

--Parse an alphanumeric string.
parseAlphaNumericString :: GenParser Char st String
parseAlphaNumericString = many1 $ oneOf (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'])

--Integer Parser

--Parse an unsigned integer.
parseUInteger :: GenParser Char st Int
parseUInteger = read Ctrl.<$> many1 digit

--Parse an integer preceded by a -.
parseNInteger :: GenParser Char st Int
parseNInteger =
  do
    minus <- char '-'
    int <- parseUInteger
    return (- int)

--Parse a positive or negative integer.
parseInteger :: GenParser Char st Int
parseInteger = parseNInteger <|> parseUInteger


-- Note Parser:

--Very primitive function to take a letter and map to an int representing the pitchclass.  This function shouldn't really be used except as a subroutine of parsePitchClass.
parseBasicPitchClass :: GenParser Char st PitchClass
parseBasicPitchClass =
  do
    rawLetter <- oneOf ['a'..'g'] <|> oneOf ['A'..'G'] <|> fail "Invalid note value."
    return . interpretNoteChar . toUpper $ rawLetter

--Parses a note with sharps and flats.
parsePitchClass :: GenParser Char st PitchClass
parsePitchClass =
  do
    rawNote <- parseBasicPitchClass
    modifiers <- many $ oneOf "b#"
    return (foldl (\a b -> interpretPitchClassModifierChar b $ a) rawNote modifiers)


-- Interval Parser:
    
--Parse a numeric interval (it's just a uint).
parseNumericInterval :: GenParser Char st Interval
parseNumericInterval = (>>=) parseInteger (return . Interval)

--Parse a named interval, using an initial basis function.
parseNamedInterval :: GenParser Char st Interval
parseNamedInterval = 
  do
    alphanum <- parseAlphaNumericString
    return $ interpretNamedIntervalCaseInsensitive alphanum

parseInterval = (try parseNamedInterval) <|> (try parseNumericInterval)

-- Pitch parser

parsePitch :: GenParser Char st Pitch
parsePitch =
  do
    pc <- parsePitchClass
    octave <- parseUInteger
    return $ Pitch pc octave



--Time parser

--Time with the denominator ommitted
parseSimpleTime :: GenParser Char st Time
parseSimpleTime =
  do
    num <- parseUInteger
    return $ Time num 4
  
--Time with denominator included
parseComplexTime :: GenParser Char st Time
parseComplexTime =
  do
    num <- parseUInteger
    _ <- (char '/')
    den <- parseUInteger
    return $ Time num den

--Time with or without deominator
parseTime :: GenParser Char st Time
parseTime = (try parseComplexTime) <|> parseSimpleTime

--Note parser
parseNote :: GenParser Char st Note
parseNote =
  do
   pitch <- parsePitch
   _ <- (char ':')
   time <- parseTime
   return $ Note pitch time



-- Chord parser
-- Parsing chords is difficult, seeing as there are a multitude of ways to notate them supported.

-- From notes (Of the form "A,C#,E")
parseChordFromNotes :: GenParser Char st Chord
parseChordFromNotes =
  do
    root <- parsePitchClass
    tag <- string ":n "
    rest <- sepBy parsePitchClass (char ',') <|> (return [])
    return $ toChord root rest

-- From intervals (Of the form "A,4,7" or "A,3rd,5th")
parseChordFromIntervals :: GenParser Char st Chord
parseChordFromIntervals =
  do
    root <- parsePitchClass
    tag <- string ":i "
    intervals <- sepBy parseInterval (string ",")
    return $ Harmony root intervals

parseChordNamed :: GenParser Char st Chord
parseChordNamed =
  do
    root <- parsePitchClass
    name <- parseAlphaNumericString
    return $ Harmony root $ chordNameToIntervalSet name

strToChordSpecial :: String -> Chord
strToChordSpecial "_" = Rest
strToChordSpecial "REST" = Rest
strToChordSpecial "SILENCE" = Rest
strToChordSpecial "BEGIN" = Begin
strToChordSpecial "START" = Begin
strToChordSpecial "END" = End
strToChordSpecial _ = error "Invalid special chord."

-- Parse a rest (Just '_')
parseChordFromOtherNotation :: GenParser Char st Chord
parseChordFromOtherNotation =
  do --TODO: Surely there is a function to do this
    str <- (string "_") <|> (string "REST") <|> (string "SILENCE") <|> (string "START") <|> (string "END")
    return $ strToChordSpecial str

parseChord :: GenParser Char st Chord
parseChord = (try parseChordFromOtherNotation) <|> (try parseChordFromNotes) <|> (try parseChordFromIntervals) <|> (parseChordNamed) --TODO I would rather have a greedy match so the tags aren't necessary.

parseTimedChord :: GenParser Char st TimedChord
parseTimedChord =
  do
    chord <- parseChord
    colon <- (char ':')
    time <- parseTime
    return $ TimedChord chord time



-- PITCH PROGRESSION PARSER
parsePitchProgression :: GenParser Char st [Pitch]
parsePitchProgression = sepBy parsePitch parseWhiteSpace

-- CHORD PROGRESSION PARSER
parseChordProgression :: GenParser Char st [Chord]
parseChordProgression = sepBy parseChord parseWhiteSpace

-- TIMED CHORD PROGRESSION PARSER
parseTimedChordProgression :: GenParser Char st [TimedChord]
parseTimedChordProgression = sepBy parseTimedChord parseWhiteSpace

-- NOTE PROGRESSION PARSER
parseNoteProgression :: GenParser Char st [Note]
parseNoteProgression = sepBy parseNote parseWhiteSpace


--To Test:
{-
parse chordProgression "(unknown)" "A,C,E B,C,C#"
parse parseBasicPitchClass "(unknown)" "A"
parse parsePitchClass "(unknown)" "A#"



-}
