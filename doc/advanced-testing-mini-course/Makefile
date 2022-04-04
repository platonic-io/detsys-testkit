PANDOC = pandoc --wrap=none -f markdown+smart -t markdown-smart --highlight-style=kate

all: README.md

README.md: src/ATMC.lhs src/ATMC/Lec1SMTesting.lhs
	$(PANDOC) --from markdown+lhs --to markdown $< >> $@
