#! /usr/bin/bash
mkdir -p build
ghc --make -i:src -o build/BFInterpreter src/main.hs