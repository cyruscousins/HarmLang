#!/usr/bin/sh
ghc ../src/initialbasis/HarmLang-initialbasis.hs  \
    ../src/syntax/HarmLang-syntax.hs  \
    ../src/parser/HarmLang-parser.hs  \
    ../src/interpreter/HarmLang-interpreter.hs  \
    cycle.hs

ghc ../src/initialbasis/HarmLang-initialbasis.hs  \
    ../src/syntax/HarmLang-syntax.hs  \
    ../src/parser/HarmLang-parser.hs  \
    ../src/interpreter/HarmLang-interpreter.hs  \
    transpose.hs

ghc ../src/initialbasis/HarmLang-initialbasis.hs  \
    ../src/syntax/HarmLang-syntax.hs  \
    ../src/parser/HarmLang-parser.hs  \
    ../src/interpreter/HarmLang-interpreter.hs  \
    melody.hs
