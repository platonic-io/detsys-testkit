#!/usr/bin/env bash

set -euo pipefail

PANDOC="pandoc --wrap=none --from markdown+smart --to markdown-smart --highlight-style=kate"

mv README.md /tmp/ATMC-README.md.old

for file in $(ls src/*.lhs); do
	  ${PANDOC} --from markdown+lhs --to gfm "${file}" >> README.md
    printf "\n---\n\n" >> README.md
done

sed -i 's/``` {.haskell .literate}/```haskell/' README.md
