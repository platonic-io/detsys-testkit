SOURCE=slides-journal.md
TARGET=slides-journal.pdf

all:
	@pandoc -t beamer $(SOURCE) -o $(TARGET) --bibliography=references.bib

clean:
	rm $(TARGET)
