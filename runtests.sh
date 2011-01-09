#! /bin/bash

if [ -e Parser.y ]
then
    echo "Building parser."
    happy Parser.y
    runhaskell Setup.hs configure
    runhaskell Setup.hs test
else
    echo "Error: Parser not found."
fi
