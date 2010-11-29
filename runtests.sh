#! /bin/bash

if [ -e Parser.hs ]
then
    echo "Parser.hs found, which is good"
else
    echo "Compiling Parser.hs"
    happy Parser.y
fi
runhaskell Setup.hs configure
runhaskell Setup.hs test