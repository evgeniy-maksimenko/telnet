REBAR = `which rebar`
DEPS_PLT=$(CURDIR)/.deps_plt
DEPS=erts kernel stdlib

$(DEPS_PLT):
	- dialyzer --output_plt $(DEPS_PLT) --build_plt \
		--apps $(DEPS) -r deps

all: deps compile

deps:
	@( $(REBAR) get-deps )

compile: clean
	@( $(REBAR) compile )

clean:
	@( $(REBAR) clean )

run:
	@( erl -boot start_sasl -config app.config -pa ebin deps/*/ebin -s webserver )

d:
	dialyzer --src -I include src

dialyzer: $(DEPS_PLT)
	dialyzer --plt $(DEPS_PLT) -Wrace_conditions --src src

xref:
	rebar xref skip_deps=true

.PHONY: all deps compile clean run