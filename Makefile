all: compiler

compiler:
	./bin/invoke_haskell_stack.sh install --profile --local-bin-path=$(shell pwd)

clean:
	./bin/invoke_haskell_stack.sh clean
	rm insc_llvm -f
	rm insc_jvm -f

format:
	ormolu --mode inplace $(shell find . -name '*.hs')

parser:
	./bin/generate_parser.sh

profile:
	./bin/invoke_haskell_stack.sh exec --profile -- latc_x86 ./test.lat +RTS -p -M 1844674407