ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
GCC_PATHS_OPT:=-I$(ROOT_DIR)/src/Runtime/dependencies/_built_/include -L$(ROOT_DIR)/src/Runtime/dependencies/_built_/lib

all: compiler

compiler: lib/runtime.ext
	./bin/invoke_haskell_stack.sh install --local-bin-path=$(shell pwd)

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

lib/runtime.ext: src/Runtime/dependencies/libunistring.txt src/Runtime/runtime.h src/Runtime/runtime.c
	gcc -O2 $(GCC_PATHS_OPT) -c src/Runtime/runtime.c -o src/Runtime/runtime.o
	mkdir -p lib
	cp src/Runtime/runtime.o lib/runtime
	cp src/Runtime/asm_externs lib/runtime.ext

src/Runtime/dependencies/_built_/done.txt:
	mkdir -p $(ROOT_DIR)/src/Runtime/dependencies/_built_
	cd src/Runtime/dependencies/libunistring-1.1 && ./configure --prefix=$(ROOT_DIR)/src/Runtime/dependencies/_built_
	cd src/Runtime/dependencies/libunistring-1.1 && make
	cd src/Runtime/dependencies/libunistring-1.1 && make install
	touch src/Runtime/dependencies/_built_/done.txt