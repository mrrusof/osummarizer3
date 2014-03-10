#!/bin/bash

GREEN="\033[0;32m"
RED="\033[0;31m"
NO_COLOR="\033[0m"

if [ ! -x osummarizer ]; then
    make osummarizer
fi

for f in `ls tests/*/*.of`; do
    echo -n "$f "
    for (( i=${#f}; i < 66; i++ )); do
        echo -n .
    done
    echo -n ' '
    (   ./osummarizer $f ${f%.of}.qarmc >/dev/null 2>1 && \
        diff ${f%.of}.qarmc.expected ${f%.of}.qarmc >/dev/null 2>1 && \
        ../../qarmc5/qarmc ${f%.of}.qarmc | grep 'program is correct' >/dev/null 2>1 && \
        echo -e ${GREEN}PASSED$NO_COLOR
    ) || echo -e ${RED}FAILED$NO_COLOR
done
