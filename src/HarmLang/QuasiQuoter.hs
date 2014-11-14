module HarmLang.QuasiQuoter 
where

import HarmLang.Parser
import HarmLang.Types
import HarmLang.Expression
import qualified Language.Haskell.TH as TH
import qualified Control.Applicative as Ctrl
import Language.Haskell.TH.Quote
import Text.ParserCombinators.Parsec


parserMap :: GenParser Char st a -> (a -> b) -> GenParser Char st b
parserMap parser f = f Ctrl.<$> parser

hlParse :: GenParser Char st HLExp
hlParse = 
    parserMap parseIntervalSingle ExpInterval <|>
    parserMap parsePitchClassSingle ExpPitchClass <|>
    parserMap parsePitchSingle ExpPitch <|>
    parserMap parseTimedChordSingle ExpTimedChord <|>
    parserMap parseNoteSingle ExpNote <|>
    parserMap parseChordSingle ExpChord <|>
    parserMap parsePitchProgression ExpPitchProgression <|>
    parserMap parseChordProgression ExpChordProgression <|>
    parserMap parseNoteProgression ExpNoteProgression <|>
    parserMap parseTimedChordProgression ExpTimedChordProgression






-- Just for testing
hlInterpret :: String -> HLExp
hlInterpret = 
  let checkResult (Right var) = var
      checkResult _ = error "Invalid expression."
  in 
      checkResult . (parse hlParse "")