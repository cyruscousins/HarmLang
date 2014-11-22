{-# LANGUAGE  QuasiQuotes #-}

import HarmLang.Interpreter
import HarmLang.Types
import HarmLang.InitialBasis
import HarmLang.QuasiQuoter

twinkle = [hl|[D@4:1 D@4:1 A@5:1 A@5:1 B@5:1 B@5:1 A@5:2 G@4:1 G@4:1 F#@4:1 F#@4:1 E@4:1 E@4:1 D@4:2]|] 

main :: IO ()
main = 
  do
    putStrLn "Twinkle Twinkle Little Star in DMa:"
    putStrLn . show $ twinkle
    putStrLn "Twinkle Twinkle Little Star in AMa:"
    putStrLn . show $ transpose twinkle (Interval 7)
