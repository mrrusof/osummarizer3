#!/bin/bash

GREEN="\033[0;32m"
RED="\033[0;31m"
NO_COLOR="\033[0m"

if [ ! -x osummarizer ]; then
    make osummarizer
fi

for f in `ls tests/*/*.of`; do
    f=${f%.of}
    echo -n "$f.qarmc "
    for (( i=${#f}; i < 60; i++ )); do
        echo -n .
    done
    echo -n ' '
    (   ./osummarizer $f.of $f.qarmc >/dev/null 2>1 && \
        if [ "$f" = "${f%_false}" ]; then
            ../../qarmc5/qarmc $f.qarmc | grep 'program is correct' >/dev/null 2>1
        else
            ../../qarmc5/qarmc $f.qarmc | grep 'program is not correct' >/dev/null 2>1
        fi && \
        echo -e ${GREEN}PASSED$NO_COLOR
    ) || echo -e ${RED}FAILED$NO_COLOR
done
