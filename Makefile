SHELL=/bin/sh

EFLAGS=-pa ebin -pa ../erlang-fmt/ebin

all: compile

compile: clean
	test -d ebin || mkdir ebin
	erl $(EFLAGS) -make

clean:
	rm -rf ebin erl_crash.dump

test: compile
	erl $(EFLAGS) -noshell -eval 'uri_template_test:all(), c:q().'
