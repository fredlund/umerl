sources = $(wildcard ../src/*.erl) 
sources += $(wildcard ../examples/*.erl)
sources += $(wildcard *.erl)
beams = $(patsubst %.erl,ebin/%.beam,$(sources))

MCERLANG = ~/gits/McErlang

$(beams): $(sources) ebin
	$(MCERLANG)/scripts/mcerl_compiler -pa ../ebin -sources $(sources)

run:
	$(MCERLANG)/scripts/mcerl -pa ebin

ebin: 
	mkdir -p ebin

clean:
	rm -f ebin/*




