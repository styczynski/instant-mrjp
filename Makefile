ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
GCC_PATHS_OPT:=-I$(ROOT_DIR)/src/Runtime/dependencies/_built_/include -L$(ROOT_DIR)/src/Runtime/dependencies/_built_/lib
CFLAGS:=-fPIE

all: compiler

compiler: lib/runtime.ext src/Parser/Gen src/IR/Parser/Gen
	./bin/invoke_haskell_stack.sh install --local-bin-path=$(shell pwd)

clean:
	./bin/invoke_haskell_stack.sh clean
	rm insc_llvm -f
	rm insc_jvm -f

format:
	ormolu --mode inplace $(shell find . -name '*.hs')

src/Parser/Gen:
	./bin/generate_main_parser.sh

src/IR/Parser/Gen:
	./bin/generate_ir_parser.sh

src/Backend/X64/Parser/Gen:
	./bin/generate_asm_parser.sh

profile:
	./bin/invoke_haskell_stack.sh exec --profile -- latc_x86 ./test.lat +RTS -p -M 1844674407

lib/runtime.ext: src/Runtime/dependencies/_built_/done.txt src/Runtime/runtime.h src/Runtime/runtime.c
	gcc -O0 $(CFLAGS) $(GCC_PATHS_OPT) -c src/Runtime/runtime.c -o src/Runtime/runtime.o
	gcc -O0 -S -fverbose-asm $(CFLAGS) $(GCC_PATHS_OPT) src/Runtime/runtime.c -o src/Runtime/runtime.s
	mkdir -p lib
	cp src/Runtime/runtime.o lib/runtime
	cp src/Runtime/asm_externs lib/runtime.ext

src/Runtime/dependencies/_built_/done.txt:
	mkdir -p $(ROOT_DIR)/src/Runtime/dependencies/_built_
	cd src/Runtime/dependencies/libunistring-1.1 && CFLAGS=$(CFLAGS) ./configure --with-pic --prefix=$(ROOT_DIR)/src/Runtime/dependencies/_built_
	cd src/Runtime/dependencies/libunistring-1.1 && CFLAGS=$(CFLAGS) make
	cd src/Runtime/dependencies/libunistring-1.1 && CFLAGS=$(CFLAGS) make install
	touch src/Runtime/dependencies/_built_/done.txt

runtest:
	rm -rfd _test_temp_ 2> /dev/null > /dev/null
	stack test

# stack exec --profile -- ./latc_x86 ./simple.lat +RTS -p -xc -M380M -i0.001 -K1000
# stack exec --profile -- ./latc_x86 ./test.lat +RTS -p -xc