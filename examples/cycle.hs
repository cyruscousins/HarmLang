import HarmLang.Interpreter
import HarmLang.Types
import HarmLang.InitialBasis

--In this example, we create an infinite chord progression by looping and take 128 bars (4 choruses) from it.

a1 = interpretTimedChordProgression "FMa7:4 F#o7:4 Gm7:4 C7:4 Am7:4 Dm7:4 Gm7:4 C7:4"
a2 = interpretTimedChordProgression "FMa7:4 F#o7:4 Gm7:4 C7:4 Am7:4 Dm7:4 Cm7:4 F7:4"
b  = interpretTimedChordProgression "BbMa7:4 Abm7:2 Db7:2 GbMa7:4 Em7:2 A7:2 DMa7:4 Abm7:2 Db7:2 GbMa7:4 Gm7:2 C7:2"
a3 = interpretTimedChordProgression "FMa7:4 F#o7:4 Gm7:4 C7:2 Bb7:2 Am7:4 D7:4 Gm7:4 C7:4 FMa7:4 Gm7:4 C7:4"

chorus = a1 ++ a2 ++ b ++ a3

infiniteChorus = repeat chorus

main :: IO ()
main = 
  do
    putStrLn "Original a1, a2, b, and a3 sections:"
    putStrLn . show $ a1
    putStrLn . show $ a2
    putStrLn . show $ b
    putStrLn . show $ a3
    putStrLn "Original Progression:"
    putStrLn . show $ chorus
    putStrLn "128 bars of looped music."
    --Want this:
    --_ <- putStrLn . show $ takeByTime infiniteChorus (Time (128 * 4) 4)
    --Equivalent to this, but this is less elegant
    putStrLn . show $ take ((*) (length chorus) 4) infiniteChorus
    putStrLn "Transposed 12 bar blues, to Bb (A#)."
