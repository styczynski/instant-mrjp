#!/bin/bash

rm -rfd src/Parser/Gen/ && \
mkdir src/Parser/Gen && \
/home/students/inf/PUBLIC/MRJP/bin/bnfc src/Parser/Latte.cf -p "Parser.Gen" --functor --generic -o src && \
mv src/Parser/Gen/TestLatte.hs src/Parser/Gen/TestLatte.txt && \
echo "Generated main parser"
