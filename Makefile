REBAR = `which rebar`

all: deps compile

deps:
	@( $(REBAR) get-deps )

compile: clean
	@( $(REBAR) compile )

clean:
	@( $(REBAR) clean )

run:
	@( erl +K true +pc unicode -pa ./ebin -pa ./deps/*/ebin -pa -s rps )

daemon:
	@( erl +K true +pc unicode -pa ./ebin -pa ./deps/*/ebin -pa -s rps -detached)

.PHONY: all deps compile clean run
