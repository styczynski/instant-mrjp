#!/bin/bash

gcc -fPIE -Lsrc/Runtime/dependencies/_built_/lib lib/runtime -l:libunistring.a arr.s -o arr -z noexecstack
