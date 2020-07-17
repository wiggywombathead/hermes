LATEX = pdflatex
BIBTEX = bibtex

MAIN = interim
FILES = $(wildcard *.tex)

OUT = $(MAIN)

$(OUT).pdf: $(FILES) bibliography.bib
	$(LATEX) $(MAIN).tex
	$(BIBTEX) $(MAIN)
	$(LATEX) $(MAIN).tex
	$(LATEX) $(MAIN).tex

count:
	detex $(MAIN) | wc -w

clean:
	rm $(OUT).pdf
