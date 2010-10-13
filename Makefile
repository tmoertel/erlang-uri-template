all:
	./rebar compile

clean:
	./rebar clean

test:
	./rebar eunit


.PHONY: all clean test
