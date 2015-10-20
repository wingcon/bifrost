-module(bifrost_sample_app).
-behaviour(application).

-export([start/2, stop/1]).
-define(PORT, 2121).

start(_Type, _Args) ->
	case bifrost_sample_sup:start_link([{port, ?PORT}]) of
		{ok, Pid} ->
			error_logger:info_msg("Started at port ~w...", [?PORT]),
			{ok, Pid};
		Else ->
			error_logger:error_msg("Start error ~p", [Else]),
			Else
	end.

stop(_State) ->
    ok.
