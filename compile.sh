#! /bin/bash

#ghc HarmLang-syntax.hs HarmLang-parser.hs HarmLang-initialbasis.hs HarmLang-quasiquotation.hs
#ghc HarmLang-syntax.hs HarmLang-parser.hs HarmLang-initialbasis.hs HarmLang-interpreter.hs

SOURCE=src/HarmLang/*.hs
ghc $SOURCE examples/transpose.hs
ghc $SOURCE examples/cycle.hs
