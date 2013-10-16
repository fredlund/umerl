#SOURCES = $(notdir $(wildcard src/*.erl))
#EXAMPLES = $(wildcard examples/*.erl)

vpath %.erl src examples
BEAMS = $(patsubst %.erl,ebin/%.beam,$(wildcard src/*.erl))
BEAMS += $(patsubst %.erl,ebin/%.beam,$(wildcard examples/*.erl))

HTMLS = $(patsubst %.erl,doc/%.html,$(SOURCES))
DOCDIR=doc
#HIPE=
#HIPE=+native

all: ebin main

main: ${BEAMS}

ebin: 
	mkdir -p ebin

ebin/%.beam: %.erl src/records.hrl
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







