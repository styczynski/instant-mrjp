#!/bin/bash

rm -rfd src/IR/Parser/Gen/ && \
mkdir src/IR/Parser/Gen && \
/home/students/inf/PUBLIC/MRJP/bin/bnfc src/IR/Parser/IR.cf -p "IR.Parser.Gen" --functor --generic -o src && \
mv src/IR/Parser/Gen/TestIR.hs src/IR/Parser/Gen/TestIR.txt && \
echo "Generated IR parser"