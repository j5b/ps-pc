#! /bin/bash

happy Parser.y
runhaskell Setup.hs configure --user
runhaskell Setup.hs build
runhaskell Setup.hs install
