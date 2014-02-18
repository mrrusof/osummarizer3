#!/bin/bash

GREEN="\033[0;32m"
RED="\033[0;31m"
NO_COLOR="\033[0m"

if [ ! -x osummarizer ]; then
    make osummarizer
fi

for f in `ls tests/*/*.of`; do
    echo -n "$f ... "
    (   ./osummarizer $f ${f%.of}.qarmc >/dev/null 2>1 && \
        echo -e ${GREEN}PASSED$NO_COLOR
    ) || echo -e ${RED}FAILED$NO_COLOR
done
