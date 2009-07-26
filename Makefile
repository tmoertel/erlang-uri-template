SOURCE_FILES := $(wildcard src/*.erl)


all: ebin

ebin: $(SOURCE_FILES:src/%.erl=ebin/%.beam)

ebin/%.beam: src/%.erl
	@test -d ebin || mkdir ebin
	erlc -W +debug_info +warn_unused_vars +warn_unused_import -o ebin $<

clean:
	@rm -rf ebin erl_crash.dump

test:
	erl -noshell -pa ebin -s uri_template_test all -s init stop
