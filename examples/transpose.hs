import HarmLang.Interpreter
import HarmLang.Types
import HarmLang.InitialBasis

--In the future, this will be done with quasiquotation.
--The interpretChordProgression allows runtime interpretation of strings, but does not provide compile time guarantees.
progression = interpretChordProgression "CM C7 F7 C7 G7 F7 G7#9"

main :: IO ()
main = 
  do
    putStrLn "Original 12 bar blues in C."
    putStrLn . show $ progression
    putStrLn "Transposed 12 bar blues, to Bb (A#)."
    putStrLn . show $ transpose progression (inverse $ Interval 2)
    --_ <- (putStrLn . show) $ transpose (inverse $ Interval 10) progression


