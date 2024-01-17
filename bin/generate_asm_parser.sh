#!/bin/bash

rm -rfd src/Backend/X64/Parser/Gen && \
rm -rf src/Backend/X64/Parser/X64GAS.cf && \
python3 src/Backend/X64/Parser/generate_grammar.py src/Backend/X64/Parser/X64GAS.cf && \
mkdir src/Backend/X64/Parser/Gen && \
/home/students/inf/PUBLIC/MRJP/bin/bnfc src/Backend/X64/Parser/X64GAS.cf -p "Backend.X64.Parser.Gen" --functor --generic -o src && \
mv src/Backend/X64/Parser/Gen/TestXGAS.hs src/Backend/X64/Parser/TestXGAS.txt && \
python3 src/Backend/X64/Parser/generate_utils.py "Backend.X64.Parser" "Constructor" "XGAS" "src/Backend/X64/Parser/Constructor.hs" && \
#rm -rf src/Backend/X64/Parser/X64GAS.cf && \
echo "Generated X64 parser"