all: get_deps compile run

test:
	./rebar eunit skip_deps=true

get_deps:
	./rebar get-deps

compile:
	./rebar compile

no_deps:
	./rebar compile skip_deps=true

clean:
	rm -rf ebin deps rel/erl_proxy

run:
	erl -pa ebin -pa deps/*/ebin \
		-config config/sys.config \
		-sname ec -s ec -s sync go

.PHONY: test get-deps compile no_deps release clean all run
