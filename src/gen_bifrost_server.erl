%%%-------------------------------------------------------------------
%%% File    : gen_bifrost_server.erl
%%% Author  : Ryan Crum <ryan.j.crum@gmail.com>
%%% Description : Behavior for a Bifrost FTP server.
%%%-------------------------------------------------------------------

-module(gen_bifrost_server).
-include("bifrost.hrl").

-type state() 		:: #connection_state{}.
-type fileinfo() 	:: #file_info{}.
-type filepath()	:: file:filename().
-type stateok()		:: {ok, state()}.
-type stateerror()	:: {error, term(), state()} | {error, term()}.
												% {error, Reason}
												% {error, state()}

-type statechange()	:: stateok() | stateerror().
-type helpinfo()	:: {Name::string(), Description::string()}.

-callback init(State::state(), Options::list(proplists:property())) ->		state() | {error, term()}.
-callback login(State::state(), Username::string(), Passwd::string()) -> 	{boolean(), state()} | quit.
-callback check_user(State::state(), Username::string()) -> statechange() | quit.
% SECURE NOTE: Does not use check_user for testing is user available,
% and return {ok, State} for all unexisting users,
% because USER+PASS obfuscate is user exist and bad password, or no user
% This function should used for connection's settings like
% USER requires SSL, USER can access from some IP and etc

-callback current_directory(State::state()) -> filepath().
-callback make_directory(State::state(), Path::filepath()) -> statechange().
-callback change_directory(State::state(), Path::filepath()) -> statechange().
-callback list_files(State::state(), Path::filepath()) -> list(fileinfo()) | stateerror().
-callback remove_directory(State::state(), Path::filepath()) -> statechange().
-callback remove_file(State::state(), Path::filepath()) -> statechange().

-callback put_file	(State::state(), Path::filepath(), append | write, fun()) -> statechange();
					(State::state(), Path::filepath(), notification,	done | % next command is arrived
															timeout | %control_timeout is passed, but connection works
															terminated % control connection error
																			) -> statechange().

-callback get_file(State::state(), Path::filepath()) -> stateerror() | {ok, fun(), state()} | {ok, fun()}.

-callback file_info(State::state(), Path::filepath()) -> {ok, fileinfo()} | stateerror().

-callback rename_file(State::state(), FromPath::filepath(), ToPath::filepath()) -> statechange().

-callback site_command(State::state(), Command::string(), Args::string()) -> statechange().
-callback site_help(State::state()) -> {ok, list(helpinfo())} | stateerror().

-callback disconnect(State::state(), exit | {error, term()}) -> statechange(). % but this state is unused
