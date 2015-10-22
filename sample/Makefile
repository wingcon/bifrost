all: compile

compile:
	rebar -v compile skip_deps=true

start: compile
	erl -pa ebin/ -eval 'application:start(bifrost_sample).'

clean:
	rebar clean skip_deps=true
	rm -f dialyzer.plt

dialyzer.plt:
	dialyzer --output_plt dialyzer.plt --build_plt --apps erts kernel stdlib crypto mnesia sasl inets ssh eunit ssl compiler runtime_tools public_key tools asn1 hipe syntax_tools

dialyze: dialyzer.plt compile
	dialyzer --plt dialyzer.plt --src src --src ../src

