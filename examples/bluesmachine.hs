{-# LANGUAGE  QuasiQuotes #-}

import HarmLang.Interpreter
import HarmLang.Types
import HarmLang.InitialBasis
import HarmLang.QuasiQuoter


progression = [hl|[CM C7 F7 C7 G7 F7 G#7]|]


main :: IO ()
main = 
  do
    putStrLn "Welcome to the Blues Buddy!"
    putStrLn "Original 12 bar blues in C."
    putStrLn . show $ progression
    putStrLn "Please enter the key to which you wish to transpose."
    newKey <- fmap interpretPitchClass getLine
    putStrLn $ "Transposed 12 bar blues, to " ++ (show newKey)
    putStrLn . show $ transpose progression (intervalAB [hl|'C'|] newKey) --The original is written in C.  To transpose from C to the desired key, we need to find the interval from C to the desired key, accomplished via the initial basis function intervalAB.

