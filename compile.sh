#! /usr/bin/bash
mkdir -p build
ghc --make -o build/BFInterpreter src/BFMain.hs