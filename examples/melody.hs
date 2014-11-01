import HarmLang.Interpreter
import HarmLang.Types
import HarmLang.InitialBasis


twinkle = interpretNoteProgression "D4:1 D4:1 A5:1 A5:1 B5:1 B5:1 A5:2 G4:1 G4:1 F#4:1 F#4:1 E4:1 E4:1 D4:2"

main :: IO ()
main = 
  do
    putStrLn "Twinkle Twinkle Little Star in DMa:"
    putStrLn . show $ twinkle
    putStrLn "Twinkle Twinkle Little Star in AMa:"
    putStrLn . show $ transpose twinkle (Interval 7)
