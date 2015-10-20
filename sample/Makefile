all: compile

compile:
	rebar -v compile skip_deps=true

start: compile
	erl -pa ebin/ -eval 'application:start(bifrost_sample).'

clean:
	rebar clean skip_deps=true
