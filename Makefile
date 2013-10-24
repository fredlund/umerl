#SOURCES = $(notdir $(wildcard src/*.erl))
#EXAMPLES = $(wildcard examples/*.erl)

vpath %.erl src examples examples/trainDoors
BEAMS = $(patsubst src/%.erl,ebin/%.beam,$(wildcard src/*.erl))
BEAMS += $(patsubst examples/%.erl,ebin/%.beam,$(wildcard examples/*.erl))
BEAMS += $(patsubst examples/trainDoors/%.erl,ebin/%.beam,$(wildcard examples/trainDoors/*.erl))

HTMLS = $(patsubst %.erl,doc/%.html,$(SOURCES))
DOCDIR=doc
#HIPE=
#HIPE=+native

all: ebin main

main: ${BEAMS}

ebin: 
	mkdir -p ebin

ebin/%.beam: %.erl src/records.hrl
	erlc +debug_info -pa ebin $(HIPE) -o ebin $<

clean:
	rm -rf ebin/* 

dialyzer: 
	dialyzer ebin/*beam

test:
	erl -pa ebin -run test test -run erlang halt

edoc: 
	erl -noshell -run edoc_run files '["src/process.erl","src/machine.erl","src/uml.erl"]' '[{dir,"doc"}]'

$(DOCDIR)/%.html: %.erl
	erl -noshell -run edoc_run file '"$<"' '[{dir,"$(DOCDIR)"}]'







