TEXES = $(wildcard *.tex)
BIBTEXES = $(wildcard *.bib)
CLSS = $(wildcard *.cls)
IMAGES = $(wilcard images/*)

all: pdf

pdf: paper.pdf

paper.pdf: $(TEXES) $(BIBTEXES) $(CLSS) $(IMAGES)
	pdflatex paper.tex
	bibtex paper
	pdflatex paper.tex
	pdflatex paper.tex
