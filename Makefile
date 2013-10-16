SOURCES = $(notdir $(wildcard src/*.erl))
BEAMS = $(patsubst %.erl,ebin/%.beam,$(SOURCES))
HTMLS = $(patsubst %.erl,doc/%.html,$(SOURCES))
DOCDIR=doc
#HIPE=
%HIPE=+native

all: ebin $(BEAMS)

ebin: 
	mkdir -p ebin

ebin/%.beam: src/%.erl src/records.hrl
	erlc +debug_info $(HIPE) -o ebin $<

clean:
	rm -rf ebin/* 

dialyzer: 
	dialyzer ebin/*beam

test:
	erl -pa ebin -run test test -run erlang halt

edoc: 
	erl -noshell -run edoc_run files '["linkem.erl"]' '[{dir,"doc"}]'

$(DOCDIR)/%.html: %.erl
	erl -noshell -run edoc_run file '"$<"' '[{dir,"$(DOCDIR)"}]'







