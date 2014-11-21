{-# LANGUAGE QuasiQuotes #-}

module HarmLang.QuasiQuoter 
where

import qualified Language.Haskell.TH as TH
import qualified Control.Applicative as Ctrl
import Language.Haskell.TH.Quote
import Text.ParserCombinators.Parsec
import Data.Generics

import HarmLang.Parser
import HarmLang.Types
import HarmLang.Expression

parserMap :: GenParser Char st a -> (a -> b) -> GenParser Char st b
parserMap parser f = f Ctrl.<$> parser

hlParse :: GenParser Char st HLExp
hlParse = 
    try (parserMap parsePitchClassSingle ExpPitchClass) <|>
    try (parserMap parsePitchSingle ExpPitch) <|>
    try (parserMap parseTimedChordSingle ExpTimedChord) <|>
    try (parserMap parseNoteSingle ExpNote) <|>
    try (parserMap parseChordSingle ExpChord) <|>
    try (parserMap parsePitchProgression ExpPitchProgression) <|>
    try (parserMap parseChordProgression ExpChordProgression) <|>
    try (parserMap parseNoteProgression ExpNoteProgression) <|>
    try (parserMap parseTimedChordProgression ExpTimedChordProgression) <|>
    try (parserMap parseIntervalSingle ExpInterval) <|> -- TODO: stop being sneaky ;)
    -- also, add parseIntervalProgression
    -- and pitch class progressions
    error "Invalid HarmLang expression."

------------------
--The following takes an HLExp and converts it to an ExpQ by unpacking the inner HL type.

hlExpToExpQ :: HLExp -> TH.ExpQ
hlExpToExpQ (ExpPitchClass a) = makeHlExpQ a
hlExpToExpQ (ExpInterval a) = makeHlExpQ a
hlExpToExpQ (ExpPitch a) = makeHlExpQ a
hlExpToExpQ (ExpTimedChord a) = makeHlExpQ a
hlExpToExpQ (ExpNote a) = makeHlExpQ a
hlExpToExpQ (ExpChord a) = makeHlExpQ a
hlExpToExpQ (ExpPitchProgression a) = makeHlExpQ a
hlExpToExpQ (ExpChordProgression a) = makeHlExpQ a
hlExpToExpQ (ExpTimedChordProgression a) = makeHlExpQ a
hlExpToExpQ (ExpNoteProgression a) = makeHlExpQ a


--Type driven hacks
--nothingness :: HLExp -> Maybe (TH.Q TH.Exp)
--nothingness _ = Nothing

--enothing :: HLExp -> Maybe (TH.Q TH.Exp)
--enothing = (const Nothing `extQ` nothingness)

makeHlExpQ :: Data a => a -> TH.ExpQ
makeHlExpQ = dataToExpQ (\ a -> Nothing)

-----------OK the bad stuff is over.

hl :: QuasiQuoter
hl =  QuasiQuoter { quoteExp = quoteHLExp,
                    quotePat = error "Not implemented" }


hlParseMonad :: Monad m => (String, Int, Int) -> String -> m HLExp
hlParseMonad (file, line, col) s =
    case runParser p () "" s of 
        Left err  -> fail $ show err
        Right e   -> return e
    where
    p = do  pos <- getPosition
            setPosition $
              (flip setSourceName) file $
              (flip setSourceLine) line $
              (flip setSourceColumn) col $
              pos
            spaces
            e <- hlParse
            eof
            return e


quoteHLExp :: String -> TH.ExpQ
quoteHLExp s =  do    
                      loc <- TH.location
                      let pos =  (TH.loc_filename loc,
                                 fst (TH.loc_start loc),
                                 snd (TH.loc_start loc))
                      hlExp <- hlParseMonad pos s
                      hlExpToExpQ hlExp --dataToExpQ (const Nothing `extQ` defnothing) hlExp

-- antiHLExp :: Expr -> Maybe (TH.Q TH.Exp)
-- antiHLExp  (AntiIntExpr v)  = Just $ TH.appE  (TH.conE (TH.mkName "IntExpr"))
--                                                 (TH.varE (TH.mkName v))
-- antiHLExp  (AntiExpr v)     = Just $ TH.varE  (TH.mkName v)
-- antiHLExp  _                = Nothing
 

-- quoteHLPat :: String -> TH.PatQ
-- quoteHLPat s = do   
--                       loc <- TH.location
--                       let pos =  (TH.loc_filename loc,
--                                  fst (TH.loc_start loc),
--                                  snd (TH.loc_start loc))
--                       hlExp <- hlParse pos s
--                       dataToPatQ (const Nothing `extQ` (\ _ -> Nothing)) hlExp
 
-- antiHLPat :: Expr -> Maybe (TH.Q TH.Pat)
-- antiHLPat  (AntiIntExpr v)  = Just $ TH.conP  (TH.mkName "IntExpr")
--                                                 [TH.varP (TH.mkName v)]
-- antiHLPat  (AntiExpr v)     = Just $ TH.varP  (TH.mkName v)
-- antiHLPat  _                = Nothing



-- Just for testing
hlInterpret :: String -> HLExp
hlInterpret = 
  let checkResult (Right var) = var
      checkResult _ = error "Invalid expression."
  in 
      checkResult . (parse hlParse "")
