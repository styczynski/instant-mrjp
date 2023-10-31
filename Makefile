all: compiler

compiler:
	./bin/invoke_haskell_stack.sh install --local-bin-path=$(shell pwd)

clean:
	./bin/invoke_haskell_stack.sh clean
	rm insc_llvm -f
	rm insc_jvm -f

format:
	ormolu --mode inplace $(git ls-files '*.hs')
