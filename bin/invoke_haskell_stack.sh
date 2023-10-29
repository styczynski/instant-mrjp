#!/bin/bash

STACK_ARGS="$*"
STACK_VERSION="N/A"
STACK_EXEC="echo Stack not available"

for STACK_CMD in "haskell_stack" "stack" "/home/students/inf/PUBLIC/MRJP/Stack/stack"; do
    if command -v ${STACK_CMD} &> /dev/null; then
        OUT=$(${STACK_CMD} --version | head -n 1)
        if [ "$OUT" != "" ]; then
            STACK_EXEC="${STACK_CMD}"
            STACK_VERSION="${OUT}"
            echo "[i] Using Stack version ${STACK_VERSION}"
            break
        fi
    fi
done

echo "[i] Calling haskell stack (${STACK_EXEC}) with the following args: [${STACK_ARGS}]"
${STACK_EXEC} ${STACK_ARGS}

STATUS="$?"
exit ${STATUS}
