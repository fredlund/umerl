pdfs := $(patsubst %.svg,%.pdf,$(wildcard *.svg))

all: $(pdfs)

%.pdf: %.svg
	convert $< $@
