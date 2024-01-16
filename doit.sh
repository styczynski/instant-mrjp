#!/bin/bash

gcc -fPIE -Lsrc/Runtime/dependencies/_built_/lib lib/runtime -l:libunistring.a simplestr.s -o simplestr -z noexecstack
