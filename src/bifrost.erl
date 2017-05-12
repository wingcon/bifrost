%%%=============================================================================
%%% File    : bifrost.erl
%%% Author  : Ryan Crum <ryan@ryancrum.org>
%%% Description : Pluggable FTP Server gen_server
%%%=============================================================================
-module(bifrost).

-behaviour(gen_server).
-include("bifrost.hrl").

-export([start_link/2, establish_control_connection/2, await_connections/2, supervise_connections/1,
                                                                            supervise_connections/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(MAX_TCPIP_PORT, 65535).
-define(REPORT_TAG, ?MODULE).
-define(ACK_TIMEOUT, 10000). % 10 second for init

%-------------------------------------------------------------------------------
start_link(HookModule, Opts) ->
    gen_server:start_link(?MODULE, {HookModule, Opts}, []).

%-------------------------------------------------------------------------------
% gen_server callbacks implementation
init({HookModule, Opts}) ->
    try
    DefState = #connection_state{module=HookModule},

    Port = proplists:get_value(port, Opts, 21),

    SslMode = case proplists:get_value(ssl, Opts, DefState#connection_state.ssl_mode) of
        disabled ->
            disabled;
        false ->
            disabled;	% legacy
        Mode when	Mode==enabled;
                    Mode==only;
                    Mode==true ->
            % is SSL module started?
            case lists:any(fun({ssl,_,_})->true;(_A)->false end, application:which_applications()) of
                true ->
                    % convert Mode to enabled/only
                    if true == Mode -> enabled; true -> Mode end;
                false ->
                    throw({stop, ssl_not_started})
            end
    end,

    SslKey = proplists:get_value(ssl_key, Opts),
    SslCert = proplists:get_value(ssl_cert, Opts),
    CaSslCert = proplists:get_value(ca_ssl_cert, Opts),

    UTF8 = proplists:get_value(utf8, Opts, DefState#connection_state.utf8),
    RecvBlockSize = proplists:get_value(recv_block_size, Opts, DefState#connection_state.recv_block_size),
    SendBlockSize = proplists:get_value(recv_block_size, Opts, DefState#connection_state.send_block_size),

    ControlTimeout = proplists:get_value(control_timeout, Opts, DefState#connection_state.control_timeout),

    PortRange = case proplists:get_value(port_range, Opts, DefState#connection_state.port_range) of
        0 						-> 0;
        1 						-> 0;
        {0, ?MAX_TCPIP_PORT}	-> 0;
        {1, ?MAX_TCPIP_PORT}	-> 0;

        N when is_integer(N), N>=0, ?MAX_TCPIP_PORT>=N ->
            {N, ?MAX_TCPIP_PORT};

        {N,M} when 	is_integer(N), N>=0, ?MAX_TCPIP_PORT>=N andalso
                    is_integer(M), M>=0, ?MAX_TCPIP_PORT>=M, M>=N  ->
            {N, M};

        _AnotherValue ->
            throw({stop, lists:flatten(io_lib:format("Invalid port_range ~p", [_AnotherValue]))})
    end,

    case listen_socket(Port, [{active, false}, {reuseaddr, true}, list]) of
        {ok, Listen} ->
            IpAddress = proplists:get_value(ip_address, Opts, get_socket_addr(Listen)),
            InitialState = DefState#connection_state{
                                ip_address=IpAddress,
                                ssl_mode=SslMode, ssl_key=SslKey, ssl_cert=SslCert, ssl_ca_cert=CaSslCert,
                                utf8=UTF8,
                                recv_block_size=RecvBlockSize,
                                send_block_size=SendBlockSize,
                                control_timeout=ControlTimeout,
                                port_range=PortRange},

            State = case HookModule:init(InitialState, Opts) of
                {error, EReason} ->				throw({stop, EReason});
                Value = #connection_state{} ->	Value
            end,
            Supervisor = proc_lib:spawn_link(?MODULE, supervise_connections, [State]),
            proc_lib:spawn_link(?MODULE,   await_connections, [Listen, Supervisor]),
            {ok, {listen_socket, Listen}};
        {error, Error} ->
            {stop, Error}
    end
    catch
        Type0:{stop, Reason} ->
            error_logger:error_report({?REPORT_TAG, {init, Type0, Reason}}),
            {stop, Reason};
        Type1:Exception ->
            error_logger:error_report({?REPORT_TAG, {init, Type1, Exception}}),
            {stop, Exception}
    end.

%-------------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%-------------------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%-------------------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%-------------------------------------------------------------------------------
terminate(_Reason, {listen_socket, Socket}) ->
    gen_tcp:close(Socket);

terminate(_Reason, _State) ->
    ok.

%-------------------------------------------------------------------------------
code_change(_OldVsn, _State, _Extra) ->
    {error, enotsup}.

%-------------------------------------------------------------------------------
get_socket_addr(Socket) ->
    case inet:sockname(Socket) of
        {ok, {Addr, _Port}} -> Addr;
        _Any ->
            undefined
    end.

%-------------------------------------------------------------------------------
get_socket_port(Socket) ->
    case inet:sockname(Socket) of
        {ok, {_Addr, Port}} -> Port;
        _Any ->
            undefined
    end.

%-------------------------------------------------------------------------------
listen_socket({Start, End}, _TcpOpts, _NextPort) when End < Start ->
    error_logger:error_report({?REPORT_TAG, {listen, "no free socket"}}),
    {error, emfile};

listen_socket({Start, End}, TcpOpts, random) ->
    % if the for [Start, End] Start<End => Start-1 < End and we have additional item for test
    listen_socket({Start-1, End}, TcpOpts, random:uniform(End-Start+1)+Start-1);

listen_socket({Start, End}, TcpOpts, TryPort) when is_integer(TryPort) ->
    case listen_socket(TryPort, TcpOpts) of
        {error, eaddrinuse} ->
            listen_socket({Start+1, End}, TcpOpts, Start+1);
        Another ->
            Another
    end.

listen_socket({Start, End}, TcpOpts0) ->
    % strategy - try a random port, after it - try from start to end
    % the assumption a lot of  ports and just few connections
    TcpOpts = case	proplists:get_value(reuseaddr, TcpOpts0, false) of
        true ->
            error_logger:warning_report({?REPORT_TAG, {listen, "find free listen socket with reuseaddr"}}),
            proplists:delete(reuseaddr, TcpOpts0);
        false ->
            TcpOpts0
    end,
    listen_socket({Start, End}, TcpOpts, random);

listen_socket(Port, TcpOpts) when is_integer(Port) ->
    gen_tcp:listen(Port, TcpOpts).

%-------------------------------------------------------------------------------
await_connections(Listen, Supervisor) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            case inet:peername(Socket) of
                {ok, {Addr, _Port}} ->
                    Supervisor!{new_connection, self(), {Socket, Addr}},
                    receive
                        {ack, Worker} ->
                            % ssl:ssl_accept/2 will return {error, not_owner} otherwise
                            % BUG - some ugly clients may disconnect in the same packet
                            gen_tcp:controlling_process(Socket, Worker)
                        after ?ACK_TIMEOUT ->
                            error_logger:error_report({?REPORT_TAG, {connection, Addr, timeout}}),
                            gen_tcp:close(Socket),
                            skip
                    end;
                {error, Reason} ->
                    % can not get info about client - skip it
                    error_logger:error_report({?REPORT_TAG, {peer, Reason}}),
                    gen_tcp:close(Socket),
                    skip
            end;
        Error ->
            error_logger:error_report({?REPORT_TAG, {accept, Error}}),
            exit(bad_accept)
    end,
    await_connections(Listen, Supervisor).

%-------------------------------------------------------------------------------
supervise_connections(InitialState) ->
    supervise_connections(InitialState, establish_control_connection).

supervise_connections(InitialState, ControlConnection) ->
    process_flag(trap_exit, true),
    receive
        {new_connection, Acceptor, {Socket, DstAddr}} ->
            WorkerState = updateState(InitialState, Socket, DstAddr),
            Worker = proc_lib:spawn_link(?MODULE, ControlConnection, [Socket, WorkerState]),
            Acceptor ! {ack, Worker};
        {'EXIT', _Pid, normal} -> % not a crash
            ok;
        {'EXIT', _Pid, shutdown} -> % manual termination, not a crash
            ok;
        {'EXIT', Pid, Info} ->
            error_logger:error_report({?REPORT_TAG, {control_connection, Pid, Info}}),
            ok;
        Notif ->
            error_logger:warning_report({?REPORT_TAG, {control_connection, unexpected, Notif}}),
            ok
    end,
    supervise_connections(InitialState, ControlConnection).

%-------------------------------------------------------------------------------
updateState(InitialState, Socket, DstAddr) ->
    IpAddress = case InitialState#connection_state.ip_address of
                    undefined 				-> get_socket_addr(Socket);
                    {0, 0, 0, 0} 			-> get_socket_addr(Socket);
                    {0, 0, 0, 0, 0, 0, 0, 0}-> get_socket_addr(Socket);
                    Ip -> Ip
                end,
    InitialState#connection_state{ip_address=IpAddress, remote_address=DstAddr}.

%-------------------------------------------------------------------------------
establish_control_connection(Socket, State) ->
    respond({gen_tcp, Socket}, 220, "FTP Server Ready"),
    control_loop(none, {gen_tcp, Socket}, State).

%-------------------------------------------------------------------------------
control_loop(HookPid, {SocketMod, RawSocket} = Socket, State0) ->
    case SocketMod:recv(RawSocket, 0, State0#connection_state.control_timeout) of
        {ok, Input} ->
            {Command, Arg} = parse_input(Input),
            State = prev_cmd_notify(Socket, State0, done), % get a valid command => let's notify about prev
            case ftp_command(Socket, State, Command, Arg) of
                {ok, NewState} ->
                    if is_pid(HookPid) ->
                            HookPid ! {new_state, self(), NewState},
                            receive
                                {ack, HookPid} ->
                                    control_loop(HookPid, Socket, NewState);
                                {done, HookPid} ->
                                    disconnect(State, {error, breaked}),
                                    {error, closed}
                            end;
                       true ->
                            control_loop(HookPid, Socket, NewState)
                    end;
                {new_socket, NewState, NewSock} ->
                    control_loop(HookPid, NewSock, NewState);
                {error, timeout} ->
                    respond(Socket, 412, "Timed out. Closing control connection."),
                    disconnect(State, {error, timeout}),
                    SocketMod:close(RawSocket),
                    {error, timeout};
                {error, closed} ->
                    disconnect(State, {error, closed}),
                    {error, closed};
                {error, auth} ->
                    disconnect(State, {error, auth}),
                    SocketMod:close(RawSocket),
                    {ok, quit};
                quit ->
                    disconnect(State, exit),
                    SocketMod:close(RawSocket),
                    {ok, quit}
            end;

        {error, timeout} ->
            NewState = prev_cmd_notify(Socket, State0, timeout),
            control_loop(HookPid, Socket, NewState);

        {error, Reason} ->
            State = prev_cmd_notify(Socket, State0, terminated),
            disconnect(State, {error, Reason}),
            error_logger:warning_report({?REPORT_TAG, {terminated, Reason, State0}}),
            {error, Reason}
    end.

%-------------------------------------------------------------------------------
respond(Socket, ResponseCode) ->
    respond(Socket, ResponseCode, response_code_string(ResponseCode) ++ ".").

respond({SocketMod, Socket}, ResponseCode, Message) ->
    Line = integer_to_list(ResponseCode) ++ " " ++ to_utf8(Message) ++ "\r\n",
    SocketMod:send(Socket, Line).

%-------------------------------------------------------------------------------
respond_raw({SocketMod, Socket}, Line) ->
    SocketMod:send(Socket, to_utf8(Line) ++ "\r\n").

%-------------------------------------------------------------------------------
respond_feature(Socket, Name, true) ->
    respond_raw(Socket, " " ++ Name);
respond_feature(_Socket, _Name, false) ->
    ok.

%-------------------------------------------------------------------------------
ssl_options(State) ->
    [{keyfile, State#connection_state.ssl_key},
     {certfile, State#connection_state.ssl_cert},
     {cacertfile, State#connection_state.ssl_ca_cert}].

%-------------------------------------------------------------------------------
data_connection(ControlSocket, State) ->
    respond(ControlSocket, 150),
    case establish_data_connection(State) of
        {ok, DataSocket} ->
            % switch socket's block
            case inet:setopts(DataSocket, [{recbuf, State#connection_state.recv_block_size}]) of
                ok ->
                    ok;
                {error, Reason} ->
                    error_logger:error_report({?REPORT_TAG, {data_connection, setopts, {Reason, State}}})
            end,

            case State#connection_state.protection_mode of
                clear ->
                    {gen_tcp, DataSocket};
                private ->
                    case ssl:ssl_accept(DataSocket,
                                        ssl_options(State)) of
                        {ok, SslSocket} ->
                            {ssl, SslSocket};
                        E ->
                            respond(ControlSocket, 425),
                            throw({error, E})
                    end
            end;
        {error, Error} ->
            respond(ControlSocket, 425),
            throw(Error)
    end.


%-------------------------------------------------------------------------------
% passive -- accepts an inbound connection
establish_data_connection(#connection_state{pasv_listen={passive, Listen, _}}) ->
    gen_tcp:accept(Listen);

% active -- establishes an outbound connection
establish_data_connection(#connection_state{data_port={active, Addr, Port}}) ->
    gen_tcp:connect(Addr, Port, [{active, false}, binary]).

%-------------------------------------------------------------------------------
pasv_connection(ControlSocket, State) ->
    case State#connection_state.pasv_listen of
        {passive, PasvListen, _} ->
            % We should only have one passive socket open at a time, so close the current one
            % and open a new one.
            gen_tcp:close(PasvListen),
            pasv_connection(ControlSocket, State#connection_state{pasv_listen=undefined});
        undefined ->
            case listen_socket(State#connection_state.port_range, [{active, false}, binary]) of
                {ok, Listen} ->
                    Port = get_socket_port(Listen),
                    Ip = State#connection_state.ip_address,
                    PasvSocketInfo = {passive,
                                      Listen,
                                      {Ip, Port}},
                    {P1,P2} = format_port(Port),
                    {S1,S2,S3,S4} = Ip,
                    respond(ControlSocket,
                            227,
                            lists:flatten(
                              io_lib:format("Entering Passive Mode (~p,~p,~p,~p,~p,~p)",
                                            [S1,S2,S3,S4,P1,P2]))),

                    {ok,
                     State#connection_state{pasv_listen=PasvSocketInfo}};
                {error, _} ->
                    respond(ControlSocket, 425),
                    {ok, State}
            end
    end.

%-------------------------------------------------------------------------------
%% put_file (stor) need a notification - when next command arrived there is a grarantee
%% that previouse 'stor' command was executed successfully
%% so this function notify about previous command
prev_cmd_notify(_Socket, State, Notif) ->
    case State#connection_state.prev_cmd_notify of
        undefined ->
            State;
        {stor, FileName} ->
            Mod = State#connection_state.module,
            State1 = case ftp_result(State, Mod:put_file(State, FileName, notification, Notif)) of
            {ok, NewState} ->
                NewState;
            {error, Reason, NewState} ->
                error_logger:warning_report({?REPORT_TAG, {notify, error, Reason, State}}),
                NewState
            end,
            State1#connection_state{prev_cmd_notify=undefined};
        {Command, _Arg} when is_atom(Command) ->
            %skip valid notification for another command
            State#connection_state{prev_cmd_notify=undefined};

        Notify ->
            %skip notify
            error_logger:warning_report({?REPORT_TAG, {nofity, unsupport, Notify, State}}),
            State#connection_state{prev_cmd_notify=undefined}
    end.

%-------------------------------------------------------------------------------
disconnect(State, Type) ->
    Mod = State#connection_state.module,
    Mod:disconnect(State, Type).

%-------------------------------------------------------------------------------
-spec ftp_result(#connection_state{}, term()) -> term().
ftp_result(State, {error}) ->
    ftp_result(State, error);

ftp_result(State, {error, error}) ->
    ftp_result(State, error);

ftp_result(State, error) ->
    ftp_result(State, {error, State});

ftp_result(_State, {error, #connection_state{}=NewState}) ->
    {error, undef, NewState};

ftp_result(State, {error, Reason}) ->
    {error, Reason, State};

ftp_result(_State, {error, Reason, #connection_state{}=NewState}) ->
    {error, Reason, NewState};

ftp_result(_State, {error, #connection_state{}=NewState, Reason}) ->
    {error, Reason, NewState};

ftp_result(_State, Data) ->
    Data.

-spec ftp_result(#connection_state{}, term(), fun()) -> term().
ftp_result(State, Data, UserFunction) ->
    ftp_result(State, UserFunction(State, Data)).

%-------------------------------------------------------------------------------
%% FTP COMMANDS
ftp_command(Socket, State, Command, RawArg) ->
    Mod = State#connection_state.module,
    case from_utf8(RawArg, State#connection_state.utf8) of
        {error, List, _RestData} ->
            error_logger:warning_report({?REPORT_TAG, {utf8, invalid, List, State}}),
            respond(Socket, 501),
            {ok, State};
        {incomplete, List, _Binary} ->
            error_logger:warning_report({?REPORT_TAG, {utf8, incomplete, List, State}}),
            respond(Socket, 501),
            {ok, State};
        Arg ->
            State1 = State#connection_state{prev_cmd_notify={Command, Arg}},
            ftp_command(Mod, Socket, State1, Command, Arg)
    end.

ftp_command(_Mod, Socket, _State, quit, _) ->
    respond(Socket, 200, "Goodbye."),
    quit;

ftp_command(_Mod, Socket, State=#connection_state{ssl_mode=disabled}, auth, _Arg) ->
    respond(Socket, 504),
    {ok, State};

ftp_command(_Mod, {_, RawSocket} = Socket, State, auth, Arg) ->
    case string:to_lower(Arg) of
        "tls" ->
            respond(Socket, 234, "Command okay."),
            case ssl:ssl_accept(RawSocket, ssl_options(State)) of
                {ok, SslSocket} ->
                    {new_socket,State#connection_state{ssl_socket=SslSocket},{ssl, SslSocket}};
                {error, Reason} ->
                    % Command itself is executed and 234 is sent
                    % but if issue at SSL level - just disconnect is a solution - so returns quit
                    error_logger:error_report({?REPORT_TAG, {ssl_accept, Reason, State}}),
                    quit
            end;
        _Method ->
            respond(Socket, 502, "Unsupported security extension."),
            {ok, State}
    end;

ftp_command(_Mod, Socket, State, feat, _Arg) ->
    respond_raw(Socket, "211-Features"),
    respond_feature(Socket, "UTF8", State#connection_state.utf8),
    respond_feature(Socket, "AUTH TLS", State#connection_state.ssl_mode =/= disabled),
    respond_feature(Socket, "PROT", State#connection_state.ssl_mode =/= disabled),
    respond(Socket, 211, "End"),
    {ok, State};

ftp_command(_Mod, Socket, State, opts, Arg) ->
    case string:to_upper(Arg) of
        "UTF8 ON" when State#connection_state.utf8 =:= true ->
            respond(Socket, 200, "Accepted.");
        _Option ->
            respond(Socket, 501)
    end,
    {ok, State};


% Allow only commands 'QUIT', 'AUTH', 'FEAT', 'OPTS'
%  for ssl_mode == only
ftp_command(_, Socket, State=#connection_state{ssl_socket=undefined, ssl_mode=only}, _, _) ->
    respond(Socket, 534, "Request denied for policy reasons (only ftps allowed)."),
    {ok, State};

ftp_command(_, Socket, State, pasv, _) ->
    pasv_connection(Socket, State);


ftp_command(_, Socket, State, prot, Arg) ->
    ProtMode = case string:to_lower(Arg) of
                   "c" -> clear;
                   _ -> private
               end,
    respond(Socket, 200),
    {ok, State#connection_state{protection_mode=ProtMode}};

ftp_command(_, Socket, State, pbsz, "0") ->
    respond(Socket, 200),
    {ok, State};

ftp_command(Mod, Socket, State, user, Arg) ->
    case ftp_result(State, Mod:check_user(State, Arg)) of
        {ok, NewState} ->
            respond(Socket, 331),
            {ok, NewState#connection_state{user_name=Arg, authenticated_state=unauthenticated}};

    {error, Reason, _State} ->
        error_logger:warning_report({?REPORT_TAG, {command, user, Reason, State}}),
        respond(Socket, 421, format_error("Login requirements", Reason)),
        {error, auth}
    end;

ftp_command(_, Socket, State, port, Arg) ->
    case parse_address(Arg) of
        {ok, {Addr, Port}} ->
            respond(Socket, 200),
            {ok, State#connection_state{data_port = {active, Addr, Port}}};
         _ ->
            respond(Socket, 452, "Error parsing address.")
        end;

ftp_command(Mod, Socket, State, pass, Arg) ->
    case Mod:login(State, State#connection_state.user_name, Arg) of
        {true, NewState} ->
            respond(Socket, 230),
            {ok, NewState#connection_state{authenticated_state=authenticated}};
        {false, NewState} ->
            respond(Socket, 530, "Login incorrect."),
            {ok, NewState#connection_state{user_name=none, authenticated_state=unauthenticated}};
        _Quit ->
            respond(Socket, 530, "Login incorrect."),
            {error, auth}
     end;

%% ^^^ from this point down every command requires authentication ^^^
ftp_command(_, Socket, State=#connection_state{authenticated_state=unauthenticated}, _, _) ->
    respond(Socket, 530),
    {ok, State};

ftp_command(_, Socket, State, rein, _) ->
    respond(Socket, 200),
    {ok,
     State#connection_state{user_name=none,authenticated_state=unauthenticated}};

ftp_command(Mod, Socket, State, pwd, _) ->
    respond(Socket, 257, "\"" ++ Mod:current_directory(State) ++ "\""),
    {ok, State};

ftp_command(Mod, Socket, State, cdup, _) ->
    ftp_command(Mod, Socket, State, cwd, "..");

ftp_command(Mod, Socket, State, cwd, Arg) ->
    case ftp_result(State, Mod:change_directory(State, Arg)) of
        {ok, NewState} ->
            respond(Socket, 250, "Directory changed to \"" ++ Mod:current_directory(NewState) ++ "\"."),
            {ok, NewState};
        {error, Reason, NewState} ->
            respond(Socket, 550, format_error("Unable to change directory", Reason)),
            {ok, NewState}
    end;

ftp_command(Mod, Socket, State, mkd, Arg) ->
    case ftp_result(State, Mod:make_directory(State, Arg)) of
        {ok, NewState} ->
            respond(Socket, 250, "\"" ++ Arg ++ "\" directory created."),
            {ok, NewState};
        {error, Reason, NewState} ->
            respond(Socket, 550, format_error("Unable to create directory", Reason)),
            {ok, NewState}
    end;

ftp_command(Mod, Socket, State, nlst, Arg) ->
    case ftp_result(State, Mod:list_files(State, Arg)) of
        {error, Reason, NewState} ->
            respond(Socket, 451, format_error("Unable to list", Reason)),
            {ok, NewState};
        Files when is_list(Files)->
            DataSocket = data_connection(Socket, State),
            list_file_names_to_socket(DataSocket, Files),
            respond(Socket, 226),
            bf_close(DataSocket),
            {ok, State}
    end;

ftp_command(Mod, Socket, State, list, Arg) ->
    case ftp_result(State, Mod:list_files(State, Arg)) of
        {error, Reason, NewState} ->
            respond(Socket, 451, format_error("Unable to list", Reason)),
            {ok, NewState};
        Files when is_list(Files)->
            DataSocket = data_connection(Socket, State),
            list_files_to_socket(DataSocket, Files),
            respond(Socket, 226),
            bf_close(DataSocket),
            {ok, State}
    end;

ftp_command(Mod, Socket, State, rmd, Arg) ->
    case ftp_result(State, Mod:remove_directory(State, Arg)) of
        {ok, NewState} ->
            respond(Socket, 200),
            {ok, NewState};
        {error, Reason, NewState} ->
            respond(Socket, 550, format_error("Unable to remove directory", Reason)),
            {ok, NewState}
        end;

ftp_command(_, Socket, State, syst, _) ->
    respond(Socket, 215, "UNIX Type: L8"),
    {ok, State};

ftp_command(Mod, Socket, State, dele, Arg) ->
    case ftp_result(State, Mod:remove_file(State, Arg)) of
        {ok, NewState} ->
            respond(Socket, 250), % see RFC 959
            {ok, NewState};
        {error, Reason, NewState} ->
            respond(Socket, 450, format_error("Unable to delete file", Reason)),
            {ok, NewState}
        end;

ftp_command(Mod, Socket, State, stor, Arg) ->
    DataSocket = data_connection(Socket, State),
    Fun = fun() ->
                  case bf_recv(DataSocket) of
                      {ok, Data} ->
                          {ok, Data, size(Data)};
                      {error, closed} ->
                          done
                  end
          end,
    RetState = case ftp_result(State, Mod:put_file(State, Arg, write, Fun)) of
                   {ok, NewState} ->
                       respond(Socket, 226),
                       NewState;
                    {error, Reason, NewState} ->
                       respond(Socket, 451, format("Error ~p when storing a file.", [Reason])),
                       NewState#connection_state{prev_cmd_notify=undefined}
               end,
    bf_close(DataSocket),
    {ok, RetState};

ftp_command(Mod, Socket, State, appe, Arg) ->
    DataSocket = data_connection(Socket, State),
    Fun = fun() ->
                  case bf_recv(DataSocket) of
                      {ok, Data} ->
                          {ok, Data, size(Data)};
                      {error, closed} ->
                          done
                  end
          end,
    RetState = case ftp_result(State, Mod:put_file(State, Arg, append, Fun)) of
                   {ok, NewState} ->
                       respond(Socket, 226),
                       NewState;
                    {error, Reason, NewState} ->
                       respond(Socket, 451, format("Error ~p when appending to a file.", [Reason])),
                       NewState#connection_state{prev_cmd_notify=undefined}
               end,
    bf_close(DataSocket),
    {ok, RetState};

ftp_command(_, Socket, State, type, Arg) ->
    case Arg of
        "I" ->
            respond(Socket, 200);
        "A" ->
            respond(Socket, 200);
        _->
            respond(Socket, 501, "Only TYPE I or TYPE A may be used.")
    end,
    {ok, State};

ftp_command(Mod, Socket, State, site, Arg) ->
    [Command | Sargs] = string:tokens(Arg, " "),
    case ftp_result(State, Mod:site_command(State, list_to_atom(string:to_lower(Command)), string:join(Sargs, " "))) of
        {ok, NewState} ->
            respond(Socket, 200),
            {ok, NewState};
        {error, not_found, NewState} ->
            respond(Socket, 500),
            {ok, NewState};
        {error, Reason, NewState} ->
            respond(Socket, 501, format("Error completing command (~p).", [Reason])),
            {ok, NewState}
    end;

ftp_command(Mod, Socket, State, site_help, _) ->
    case ftp_result(State, Mod:site_help(State)) of
        {error, Reason, NewState} ->
            respond(Socket, 500, format_error("Unable to help site", Reason)),
            {ok, NewState};
        {ok, []} ->
            respond(Socket, 500),
            {ok, State};
        {ok, Commands} ->
            respond_raw(Socket, "214-The following commands are recognized"),
            lists:map(fun({CmdName, Descr}) ->
                              respond_raw(Socket, CmdName ++ " : " ++ Descr)
                      end,
                      Commands),
            respond(Socket, 214, "Help OK"),
            {ok, State}
    end;

ftp_command(Mod, Socket, State, help, Arg) ->
    LowerArg =  string:to_lower(Arg),
    case LowerArg of
        "site" ->
            ftp_command(Mod, Socket, State, site_help, undefined);
        _ ->
            respond(Socket, 500),
            {ok, State}
    end;

ftp_command(Mod, Socket, State, retr, Arg) ->
    try
        case ftp_result(State, Mod:get_file(State, Arg),
                    fun	(S, {ok, Fun}) when is_function(Fun)-> {ok, Fun, S};
                        (_S, Any) -> Any	end) of

            {ok, Fun, State0} ->
                  DataSocket = data_connection(Socket, State0),
                  case ftp_result(State0, write_fun(State0#connection_state.send_block_size, DataSocket, Fun)) of
                  {ok, NewState} ->
                      bf_close(DataSocket),
                      respond(Socket, 226),
                    {ok, NewState};

                  {error, Reason, NewState} ->
                      bf_close(DataSocket),
                    respond(Socket, 451, format_error("Unable to get file", Reason)),
                    {ok, NewState}
                  end;

            {error, Reason, NewState} ->
                  respond(Socket, 550, format_error("Unable to get file", Reason)),
                  {ok, NewState}
        end
    catch
        T:Error ->
            error_logger:error_report({?REPORT_TAG, {get_file, {Mod, T, Error}, State}}),
            respond(Socket, 550),
            {ok, State}
    end;

ftp_command(Mod, Socket, State, mdtm, Arg) ->
    case ftp_result(State, Mod:file_info(State, Arg)) of
        {ok, FileInfo} ->
            respond(Socket, 213, format_mdtm_date(FileInfo#file_info.mtime)),
            {ok, State};
        {error, Reason, NewState} ->
            respond(Socket, 550, format_error(550, Reason)),
            {ok, NewState}
    end;

ftp_command(_, Socket, State, rnfr, Arg) ->
    respond(Socket, 350, "Ready for RNTO."),
    {ok, State#connection_state{rnfr=Arg}};

ftp_command(Mod, Socket, State, rnto, Arg) ->
    case State#connection_state.rnfr of
        undefined ->
            respond(Socket, 503, "RNFR not specified."),
            {ok, State};
        Rnfr ->
            case ftp_result(State, Mod:rename_file(State, Rnfr, Arg)) of
                {error, Reason, NewState} ->
                    respond(Socket, 550, io_lib:format("Unable to rename (~p).", [Reason])),
                    {ok, NewState};
                {ok, NewState} ->
                    respond(Socket, 250, "Rename successful."),
                    {ok, NewState#connection_state{rnfr=undefined}}
            end
    end;

ftp_command(Mod, Socket, State, xcwd, Arg) ->
    ftp_command(Mod, Socket, State, cwd, Arg);

ftp_command(Mod, Socket, State, xcup, Arg) ->
    ftp_command(Mod, Socket, State, cdup, Arg);

ftp_command(Mod, Socket, State, xmkd, Arg) ->
    ftp_command(Mod, Socket, State, mkd, Arg);

ftp_command(Mod, Socket, State, xpwd, Arg) ->
    ftp_command(Mod, Socket, State, pwd, Arg);

ftp_command(Mod, Socket, State, xrmd, Arg) ->
    ftp_command(Mod, Socket, State, rmd, Arg);

ftp_command(_Mod, Socket, State, size, _Arg) ->
    respond(Socket, 550),
    {ok, State};

ftp_command(_, Socket, State, Command, _Arg) ->
    error_logger:warning_report({?REPORT_TAG, {command, Command, unrecognized, State}}),
    respond(Socket, 500),
    {ok, State}.

%-------------------------------------------------------------------------------
write_fun(SendBlockSize,Socket, Fun) ->
    case Fun(SendBlockSize) of
        {ok, Bytes, NextFun} ->
            bf_send(Socket, Bytes),
            write_fun(SendBlockSize,Socket, NextFun);
        {done, NewState} ->
            {ok, NewState};
        Another -> % errors and etc
            Another
    end.

%-------------------------------------------------------------------------------
strip_newlines(S) ->
    lists:foldr(fun(C, A) ->
                       string:strip(A, right, C) end,
                S,
                "\r\n").

%-------------------------------------------------------------------------------
parse_input(Input) ->
    Tokens = string:tokens(Input, " "),
    [Command | Args] = lists:map(fun(S) -> strip_newlines(S) end,
                                 Tokens),
    {list_to_atom(string:to_lower(Command)), string:join(Args, " ")}.

%-------------------------------------------------------------------------------
list_files_to_socket(DataSocket, Files) ->
    lists:map(fun(Info) ->
                      bf_send(DataSocket,
                              to_utf8(file_info_to_string(Info)) ++ "\r\n") end,
              Files),
    ok.

%-------------------------------------------------------------------------------
list_file_names_to_socket(DataSocket, Files) ->
    lists:map(fun(Info) ->
                      bf_send(DataSocket,
                              to_utf8(Info#file_info.name) ++ "\r\n") end,
              Files),
    ok.

%-------------------------------------------------------------------------------
bf_send({SockMod, Socket}, Data) ->
    SockMod:send(Socket, Data).

%-------------------------------------------------------------------------------
bf_close({SockMod, Socket}) ->
    SockMod:close(Socket).

%-------------------------------------------------------------------------------
bf_recv({SockMod, Socket}) ->
    SockMod:recv(Socket, 0, infinity).

%-------------------------------------------------------------------------------
response_code_string(150) -> "File status okay; about to open data connection";
response_code_string(200) -> "Command okay";
response_code_string(221) -> "Service closing control connection";
response_code_string(225) -> "Data connection open; no transfere in progress";
response_code_string(226) -> "Closing data connection";
response_code_string(227) -> "Entering Passive Mode (h1,h2,h3,h4,p1,p2)";
response_code_string(230) -> "User logged in, proceed";
response_code_string(250) -> "Requested file action okay, completed";
response_code_string(257) -> "PATHNAME created";
response_code_string(331) -> "User name okay, need password";
response_code_string(332) -> "Need account for login";
response_code_string(350) -> "Requested file action pending further information";
response_code_string(421) -> "Service not available, closing control connection";
response_code_string(425) -> "Can't open data connection";
response_code_string(426) -> "Connection closed; transfere aborted";
response_code_string(450) -> "Requested file action not taken";
response_code_string(451) -> "Requested action not taken: local error in processing";
response_code_string(452) -> "Requested action not taken";
response_code_string(500) -> "Syntax error, command unrecognized";
response_code_string(501) -> "Syntax error in parameters or arguments";
response_code_string(502) -> "Command not implemented";
response_code_string(530) -> "Not logged in";
response_code_string(550) -> "Requested action not taken";
response_code_string(552) -> "Requested file action aborted";
response_code_string(_) -> "N/A".

%-------------------------------------------------------------------------------
% Taken from jungerl/ftpd
file_info_to_string(Info) ->
    format_type(Info#file_info.type) ++
        format_access(Info#file_info.mode) ++ " " ++
        format_number(type_num(Info#file_info.type), 2, $ ) ++ " " ++
        format_number(Info#file_info.uid,5,$ ) ++ " " ++
        format_number(Info#file_info.gid,5,$ ) ++ " "  ++
        format_number(Info#file_info.size,8,$ ) ++ " " ++
        format_date(Info#file_info.mtime) ++ " " ++
        Info#file_info.name.

%-------------------------------------------------------------------------------
format_mdtm_date({{Year, Month, Day}, {Hours, Mins, Secs}}) ->
    lists:flatten(io_lib:format("~4..0B~2..0B~2..0B~2..0B~2..0B~2..0B",
                                [Year, Month, Day, Hours, Mins, erlang:trunc(Secs)])).

%-------------------------------------------------------------------------------
format_date({Date, Time}) ->
    {Year, Month, Day} = Date,
    {Hours, Min, _} = Time,
    {LDate, _LTime} = calendar:local_time(),
    {LYear, _, _} = LDate,
    format_month_day(Month, Day) ++
        if LYear > Year ->
                format_year(Year);
           true ->
                format_time(Hours, Min)
        end.

%-------------------------------------------------------------------------------
format_month_day(Month, Day) ->
    io_lib:format("~s ~2.2w", [month(Month), Day]).

%-------------------------------------------------------------------------------
format_year(Year) ->
    io_lib:format(" ~5.5w", [Year]).

%-------------------------------------------------------------------------------
format_time(Hours, Min) ->
    io_lib:format(" ~2.2.0w:~2.2.0w", [Hours, Min]).

%-------------------------------------------------------------------------------
format_type(file) -> "-";
format_type(dir) -> "d";
format_type(_) -> "?".

%-------------------------------------------------------------------------------
type_num(file) ->
    1;
type_num(dir) ->
    4;
type_num(_) ->
    0.

%-------------------------------------------------------------------------------
format_access(Mode) ->
    format_rwx(Mode bsr 6) ++ format_rwx(Mode bsr 3) ++ format_rwx(Mode).

%-------------------------------------------------------------------------------
format_rwx(Mode) ->
    [if Mode band 4 == 0 -> $-; true -> $r end,
     if Mode band 2 == 0 -> $-; true -> $w end,
     if Mode band 1 == 0 -> $-; true -> $x end].

%-------------------------------------------------------------------------------
format_number(X, N, LeftPad) when X >= 0 ->
    Ls = integer_to_list(X),
    Len = length(Ls),
    if Len >= N -> Ls;
       true ->
            lists:duplicate(N - Len, LeftPad) ++ Ls
    end.

%-------------------------------------------------------------------------------
month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

%-------------------------------------------------------------------------------
% parse address on form:
% d1,d2,d3,d4,p1,p2  => { {d1,d2,d3,d4}, port} -- ipv4
% h1,h2,...,h32,p1,p2 => {{n1,n2,..,n8}, port} -- ipv6
% Taken from jungerl/ftpd
parse_address(Str) ->
    paddr(Str, 0, []).

%-------------------------------------------------------------------------------
paddr([X|Xs],N,Acc) when X >= $0, X =< $9 -> paddr(Xs, N*10+(X-$0), Acc);
paddr([X|Xs],_N,Acc) when X >= $A, X =< $F -> paddr(Xs,(X-$A)+10, Acc);
paddr([X|Xs],_N,Acc) when X >= $a, X =< $f -> paddr(Xs, (X-$a)+10, Acc);
paddr([$,,$,|_Xs], _N, _Acc) -> error;
paddr([$,|Xs], N, Acc) -> paddr(Xs, 0, [N|Acc]);
paddr([],P2,[P1,D4,D3,D2,D1]) -> {ok,{{D1,D2,D3,D4}, P1*256+P2}};
paddr([],P2,[P1|As]) when length(As) == 32 ->
    case addr6(As,[]) of
        {ok,Addr} -> {ok, {Addr, P1*256+P2}};
        error -> error
    end;
paddr(_, _, _) -> error.

%-------------------------------------------------------------------------------
addr6([H4,H3,H2,H1|Addr],Acc) when H4<16,H3<16,H2<16,H1<16 ->
    addr6(Addr, [H4 + H3*16 + H2*256 + H1*4096 |Acc]);
addr6([], Acc) -> {ok, list_to_tuple(Acc)};
addr6(_, _) -> error.

%-------------------------------------------------------------------------------
format_port(PortNumber) ->
    [A,B] = binary_to_list(<<PortNumber:16>>),
    {A, B}.

%-------------------------------------------------------------------------------
-spec format(string(), list()) -> string().
format(FormatString, Args) ->
    case catch io_lib:format(FormatString, Args) of
        {'EXIT',{badarg,_}} ->
            error_logger:error_report({?REPORT_TAG, {format_error, {FormatString, Args}}}),
            "Invalid format";
        Data ->
            lists:flatten(Data)
    end.

%-------------------------------------------------------------------------------
-spec format_error(integer() | string(), term()) -> string().
format_error(Code, Reason) when is_integer(Code) ->
    format_error(response_code_string(Code), Reason);

format_error(Message, undef) ->
    Message ++ ".";

format_error(Message, Reason) ->
    Format = case io_lib:printable_unicode_list(Reason) of
        true ->
            "~ts (~ts)";
        _False ->
            "~ts (~p)"
    end,
    format(Format, [Message, Reason]) ++ ".".

%-------------------------------------------------------------------------------
from_utf8(String, true) ->
    unicode:characters_to_list(erlang:list_to_binary(String), utf8);

from_utf8(String, false) ->
    String.

%-------------------------------------------------------------------------------
to_utf8(String) ->
    to_utf8(String, true).

to_utf8(String, true) ->
    erlang:binary_to_list(unicode:characters_to_binary(String, utf8));

to_utf8(String, false) ->
    [if C > 255 orelse C<0 -> $?; true -> C end || C <- String].

%===============================================================================
% EUNIT TEST
%-------------------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%===============================================================================
% Simple test(s) for small functions, pure functions
%-------------------------------------------------------------------------------
strip_newlines_test() ->
    "testing 1 2 3" = strip_newlines("testing 1 2 3\r\n"),
    "testing again" = strip_newlines("testing again").

%-------------------------------------------------------------------------------
parse_input_test() ->
    {test, "1 2 3"} = parse_input("TEST 1 2 3"),
    {test, ""} = parse_input("Test\r\n"),
    {test, "awesome"} = parse_input("Test awesome\r\n").

%-------------------------------------------------------------------------------
format_access_test() ->
    "rwxrwxrwx" = format_access(8#0777),
    "rw-rw-rw-" = format_access(8#0666),
    "r--rwxrwx" = format_access(8#0477),
    "---------" = format_access(0).

%-------------------------------------------------------------------------------
format_number_test() ->
    "005" = format_number(5, 3, $0),
    "500" = format_number(500, 2, $0),
    "500" = format_number(500, 3, $0).

%-------------------------------------------------------------------------------
parse_address_test() ->
    {ok, {{127,0,0,1}, 2000}} = parse_address("127,0,0,1,7,208"),
    error = parse_address("MEAT MEAT").

%-------------------------------------------------------------------------------
ftp_result_test() ->
    % all results from gen_bifrost_server.erl
    State 		= #connection_state{authenticated_state=unauthenticated},
    NewState	= State#connection_state{authenticated_state=authenticated},
    ?assertEqual({ok, NewState}, ftp_result(State, {ok, NewState})),

    ?assertEqual({error, undef, NewState}, 	ftp_result(State, {error, NewState})),

    ?assertEqual({error, "Error", 	State}, ftp_result(State, {error, "Error"})),
    ?assertEqual({error, not_found, State}, ftp_result(State, {error, not_found})),

    ?assertEqual({error, not_found, NewState}, ftp_result(State, {error, not_found, NewState})),
    ?assertEqual({error, not_found, NewState}, ftp_result(State, {error, NewState, not_found})),

    % a special results
    ?assertEqual("/path", ftp_result(State, "/path")), %current_directory

    ?assertEqual({error, undef, NewState}, ftp_result(State, {error, NewState})), %list_files
    ?assertEqual([], ftp_result(State, [])), %list_files

    Fun = fun(_BytesCount) -> ok end,
    ?assertEqual({error, undef, State}, ftp_result(State, error)), %get_file
    ?assertMatch({ok, Fun}, ftp_result(State, {ok, Fun})), %get_file

    ?assertMatch({ok, Fun, State}, ftp_result(State, {ok, Fun},
                                            fun (S, {ok, Fn}) when is_function(Fn)-> {ok, Fn, S};
                                                        (_S, Any) -> Any    end)),

    ?assertMatch({ok, Fun, NewState}, ftp_result(State, {ok, Fun, NewState},
                                            fun (S, {ok, Fn}) when is_function(Fn)-> {ok, Fn, S};
                                                        (_S, Any) -> Any    end)),

    ?assertMatch({ok, NewState}, ftp_result(State, {ok, NewState},
                                            fun (S, {ok, Fn}) when is_function(Fn)-> {ok, Fn, S};
                                                        (_S, Any) -> Any    end)),

    ?assertMatch({error, undef, NewState}, ftp_result(State, {error, NewState},
                                            fun (S, {ok, Fn}) when is_function(Fn)-> {ok, Fn, S};
                                                        (_S, Any) -> Any    end)),

    ?assertMatch({ok, file_info}, ftp_result(State, {ok, file_info})),	%file_info
    ?assertMatch({error, "ErrorCause", State}, ftp_result(State, {error, "ErrorCause"})),	%file_info

    ?assertMatch({ok, [help_info]}, ftp_result(State, {ok, [help_info]})),			%site_help
    ?assertMatch({error, undef, NewState}, ftp_result(State, {error, NewState})),	%site_help
    ok.

%===============================================================================
% Functional tests
%-------------------------------------------------------------------------------
fixture_setup() ->
    error_logger:tty(false),
    ok = meck:new(error_logger, [unstick, passthrough]),
    ok = meck:new(gen_tcp, [unstick]),
    ok = meck:new(inet, [unstick, passthrough]),
    ok = meck:new(fake_server, [non_strict]),
    ok = meck:expect(fake_server, init, fun(InitialState, _Opt) -> InitialState end),
    ok = meck:expect(fake_server, disconnect, fun(_, {error, breaked}) -> ok end),
    fake_server:init(#connection_state{module=fake_server}, []).

%-------------------------------------------------------------------------------
fixture_cleanup(_State) ->
    meck:unload(fake_server),
    meck:unload(inet),
    meck:unload(gen_tcp),
    meck:unload(error_logger),
    error_logger:tty(true).

%===============================================================================
% GenServer functional tests
%-------------------------------------------------------------------------------
genserver_test_() ->
    {foreach, fun fixture_setup/0, fun fixture_cleanup/1,[	fun genserver_test_ti/1]}.

%-------------------------------------------------------------------------------
genserver_test_ti(_State) ->
    ?assertMatch({reply, _, state}, handle_call(request, from, state)),
    ?assertMatch({noreply, state}, handle_cast(message, state)),
    ?assertMatch({noreply, state}, handle_info(info, state)),
    ?assertMatch({error, enotsup}, code_change(old, state, extra)),
    ?assertMatch(ok, 	terminate(reason, stop)),
    ok = meck:expect(gen_tcp, close, fun(socket) -> ok end),
    ?assertMatch(ok,  terminate(reason, {listen_socket, socket})),
    [?_assert(true)].

%===============================================================================
% Init functional tests
%-------------------------------------------------------------------------------
init_test_() ->
    {foreach, fun fixture_setup/0, fun fixture_cleanup/1,[	fun init_test_ti/1]}.

%-------------------------------------------------------------------------------
init_test_ti(_State) ->
    ok = meck:expect(gen_tcp, listen, fun(21, _TcpOpts) -> {error, already_used} end),
    ?assertEqual({stop, already_used}, init({fake_server, []})),

    ok = meck:expect(gen_tcp, listen, fun(21, _TcpOpts) -> {ok, listen} end),
    ok = meck:expect(inet, sockname, fun(listen) -> {error, badarg} end),
    ok = meck:expect(fake_server, init, fun(_InitialState, _Opt) -> throw("fake_exception") end),
    ?assertEqual({stop, "fake_exception"}, init({fake_server, []})),
    ok = meck:expect(fake_server, init, fun(_InitialState, _Opt) -> {error, "fake_error"} end),
    ?assertEqual({stop, "fake_error"}, init({fake_server, []})),

    ok = meck:expect(gen_tcp, listen, fun(6666, _TcpOpts) -> {ok, listen} end),
    ok = meck:expect(inet, sockname, fun(listen) -> {ok, {localip, localport}} end),
    ?assertEqual({stop, "fake_error"}, init({fake_server, [{port, 6666}, {ssl, disabled}]})),

    ok = meck:expect(gen_tcp, listen, fun(6667, _TcpOpts) -> {ok, listen} end),
    ?assertEqual({stop, ssl_not_started}, init({fake_server, [{port, 6667}, {ssl, enabled}, {port_range, 0}]})),

    IsStarted = (ok =:= ssl:start()), % start for SSL
    ?assertEqual({stop, "fake_error"}, init({fake_server, [{port, 6667}, {ssl, enabled}, {port_range, 0}]})),
    ?assertEqual({stop, "fake_error"}, init({fake_server, [{port, 6667}, {ssl, false}, {port_range, 100}]})),
    ?assertEqual({stop, "fake_error"}, init({fake_server, [{port, 6667}, {ssl, only}, {port_range, {6000, 8000}}]})),
    IsStarted andalso ssl:stop(), % stop SSL if started by our code
    [?_assert(true)].

%===============================================================================
%===============================================================================
% Estabilish connection functional tests
%-------------------------------------------------------------------------------
connection_test_() ->
    {	foreach, fun fixture_setup/0, fun fixture_cleanup/1,
        [	fun await_invalid/1,
            fun await_timeout/1,
            fun await_peer_closed/1,
            fun await_control_closed/1,
            fun await_sock_closed/1,
            fun await_ok/1]
    }.

%-------------------------------------------------------------------------------
establish_control_connection_simulator(socket, _State) ->
    receive
        after 1000 -> ok
    end.

%-------------------------------------------------------------------------------
await_invalid(_InitialState) ->
    ok = meck:expect(gen_tcp, accept, fun(listen) -> terminate end),
    ?assertExit(bad_accept, await_connections(listen, nopid)),
    [?_assert(true)].

%-------------------------------------------------------------------------------
await_timeout(_InitialState) ->
    ok = meck:expect(gen_tcp, accept, fun(listen) ->
        ok = meck:expect(gen_tcp, accept, fun(listen) -> terminate end),
        {ok, socket} end),
    ok = meck:expect(gen_tcp, close, fun(socket) -> ok end),
    ok = meck:expect(inet, peername, fun(socket) -> {ok, {remote_addr, remote_port}} end),
    SupervisorPid = self(),
    ?assertExit(bad_accept, await_connections(listen, SupervisorPid)),
    [?_assert(true)].

%-------------------------------------------------------------------------------
await_peer_closed(InitialState) ->
    ok = meck:expect(gen_tcp, accept, fun(listen) ->
        ok = meck:expect(gen_tcp, accept, fun(listen) -> terminate end),
        {ok, socket} end),
    ok = meck:expect(gen_tcp, close, fun(socket) -> ok end),
    ok = meck:expect(inet, peername, fun(socket) -> {error, closed} end),

    ?assert(is_function(fun establish_control_connection_simulator/2)), % create ref
    SupervisorPid = spawn_link(?MODULE, supervise_connections,
                                    [InitialState, establish_control_connection_simulator]),
    ?assertExit(bad_accept, await_connections(listen, SupervisorPid)),
    [?_assert(true)].

%-------------------------------------------------------------------------------
await_control_closed(InitialState) ->
    ok = meck:expect(gen_tcp, accept, fun(listen) ->
        ok = meck:expect(gen_tcp, accept, fun(listen) -> terminate end),
        {ok, socket} end),
    ok = meck:expect(gen_tcp, close, fun(socket) -> ok end),
    ok = meck:expect(inet, peername, fun(socket) -> {ok, {remote_addr,remote_port}} end),
    ok = meck:expect(gen_tcp, controlling_process, fun(socket, _Pid) -> {error, closed} end),
    ok = meck:expect(inet, sockname, fun(socket) -> {error, closed} end),

    ?assert(is_function(fun establish_control_connection_simulator/2)), % create ref
    SupervisorPid = spawn_link(?MODULE, supervise_connections,
                                    [InitialState, establish_control_connection_simulator]),
    ?assertExit(bad_accept, await_connections(listen, SupervisorPid)),
    [?_assert(true)].

%-------------------------------------------------------------------------------
await_sock_closed(InitialState) ->
    ok = meck:expect(gen_tcp, accept, fun(listen) ->
        ok = meck:expect(gen_tcp, accept, fun(listen) -> terminate end),
        {ok, socket} end),
    ok = meck:expect(gen_tcp, close, fun(socket) -> ok end),
    ok = meck:expect(inet, peername, fun(socket) -> {ok, {remote_addr,remote_port}} end),
    ok = meck:expect(gen_tcp, controlling_process, fun(socket, _Pid) -> ok end),
    ok = meck:expect(inet, sockname, fun(socket) -> {error, closed} end),

    ?assert(is_function(fun establish_control_connection_simulator/2)),
    SupervisorPid = spawn_link(?MODULE, supervise_connections,
                                    [InitialState, establish_control_connection_simulator]),
    ?assertExit(bad_accept, await_connections(listen, SupervisorPid)),
    [?_assert(true)].

%-------------------------------------------------------------------------------
await_ok(InitialState) ->
    ok = meck:expect(gen_tcp, accept, fun(listen) ->
        ok = meck:expect(gen_tcp, accept, fun(listen) -> terminate end),
        {ok, socket} end),
    ok = meck:expect(inet, peername, fun(socket) -> {ok, {remote_addr,remote_port}} end),
    ok = meck:expect(gen_tcp, controlling_process, fun(socket, _Pid) -> ok end),
    ok = meck:expect(inet, sockname, fun(socket) -> {ok, {local_addr, local_port}} end),

    ?assert(is_function(fun establish_control_connection_simulator/2)),
    SupervisorPid = spawn_link(?MODULE, supervise_connections,
                                    [InitialState, establish_control_connection_simulator]),
    ?assertExit(bad_accept, await_connections(listen, SupervisorPid)),
    [?_assert(true)].

%===============================================================================

%===============================================================================
% Functional integration tests
-define(dataSocketTest(TEST_NAME),
        TEST_NAME() ->
               TEST_NAME(active),
               TEST_NAME(passive)).

%-------------------------------------------------------------------------------
setup() ->
    fixture_setup().

%-------------------------------------------------------------------------------
execute(ListenerPid) ->
    State = fake_server:init(#connection_state{module=fake_server}, []),
    receive
        {ack, ListenerPid} ->
            control_loop(ListenerPid, {gen_tcp, socket}, State#connection_state{ip_address={127,0,0,1}}),
            meck:validate(fake_server),
            meck:validate(gen_tcp)
    end,
    fixture_cleanup(execute).

%-------------------------------------------------------------------------------
% Awkward, monadic interaction sequence testing
script_dialog([]) ->
    meck:expect(gen_tcp,
                recv,
                fun(_, _, infinity) -> {error, closed} end);

script_dialog([{Request, Response} | Rest]) ->
    meck:expect(gen_tcp,
                recv,
                fun(Socket, _, infinity) ->
                        script_dialog([{resp, Socket, Response}] ++ Rest),
                        {ok, Request}
                end);

script_dialog([{resp, Socket, Response} | Rest]) ->
    meck:expect(gen_tcp, send,
                    fun(S, C) ->
                        ?assertEqual(Socket, S),
                        ?assertEqual(Response, unicode:characters_to_list(C)),
                    script_dialog(Rest),
                    ok
                end);

script_dialog([{resp_bin, Socket, Response} | Rest]) ->
    meck:expect(gen_tcp,
                send,
                fun(S, C) ->
                        ?assertEqual(Socket, S),
                        ?assertEqual(Response, C),
                        script_dialog(Rest),
                        ok
                end);

script_dialog([{resp_error, Socket, Error} | Rest]) ->
    meck:expect(gen_tcp,
                send,
                fun(S, _C) ->
                        ?assertEqual(Socket, S),
                        script_dialog(Rest),
                        {error, Error}
                end);

script_dialog([{req_error, Socket, Error} | Rest]) ->
    meck:expect(gen_tcp,
                recv,
                fun(S, _, infinity) ->
                        ?assertEqual(S, Socket),
                        script_dialog(Rest),
                        {error, Error}
                end);

script_dialog([{req, Socket, Request} | Rest]) ->
    meck:expect(gen_tcp,
                recv,
                fun(S, _, infinity) ->
                        ?assertEqual(S, Socket),
                        script_dialog(Rest),
                        {ok, Request}
                end).

%-------------------------------------------------------------------------------
% executes the next step in the test script
step(Pid) ->
    Pid ! {ack, self()},  	% 1st ACK will be 'eaten' by execute
                % so valid sequence will be
    receive
        {new_state, Pid, State} ->
            {ok, State};
        _ ->
            ?assert(fail)
    end.

%-------------------------------------------------------------------------------
% stops the script
finish(Pid) ->
    ?assertEqual({error, closed}, 	gen_tcp:recv(dummy_socket, 0, infinity)), % if fails - some step() forgotten
    Pid ! {done, self()}.

%-------------------------------------------------------------------------------
login_test_user(SocketPid) ->
    login_test_user(SocketPid, []).

%-------------------------------------------------------------------------------
login_test_user(SocketPid, Script) ->
    script_dialog([{req, socket, "USER meat"},
                   {resp, socket, "331 User name okay, need password.\r\n"},
                   {req, socket, "PASS meatmeat"},
                   {resp, socket, "230 User logged in, proceed.\r\n"}] ++ Script),

    ok = meck:expect(fake_server, check_user, fun(S, _A) -> {ok, S} end),
    step(SocketPid), % USER meat

    ok = meck:expect(fake_server,
                login,
                fun(St, "meat", "meatmeat") ->
                        {true, St#connection_state{authenticated_state=authenticated}}
                end),
    {ok, State1} = step(SocketPid),
    ?assertMatch(#connection_state{authenticated_state=authenticated}, State1),
    {ok, State1}.

%-------------------------------------------------------------------------------
authenticate_successful_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      login_test_user(ControlPid),
                      finish(ControlPid)
              end),
    execute(Child).

%-------------------------------------------------------------------------------
authenticate_failure_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                script_dialog([{"USER meat", "331 User name okay, need password.\r\n"},
                                  {"PASS meatmeat", "530 Login incorrect.\r\n"}]),
                ok = meck:expect(gen_tcp, close, fun(socket) -> ok end),
                ok = meck:expect(fake_server, login, fun(_, "meat", "meatmeat") -> {error} end),
                ok = meck:expect(fake_server, check_user, fun(S, _A) -> {ok, S} end),
                ok = meck:expect(fake_server, disconnect, fun(_, {error, auth}) -> ok end),
                {ok, State} = step(ControlPid),
                ?assertMatch(#connection_state{authenticated_state=unauthenticated}, State),
                step(ControlPid), % last event will  be disconnect with reason auth fail
                finish(ControlPid)
              end),

    execute(Child).

%-------------------------------------------------------------------------------
requirements_failure_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                script_dialog([{"USER meat", "331 User name okay, need password.\r\n"},
                               {"USER heat", "421 Login requirements (DENY).\r\n"}]),
                ok = meck:expect(gen_tcp, close, fun(socket) -> ok end),
                ok = meck:expect(fake_server, check_user, fun(S, "meat") -> {ok, S};
                                                             (S, "heat") -> {error, "DENY", S} end),
                ok = meck:expect(fake_server, disconnect, fun(_, {error, auth}) -> ok end),
                {ok, State} = step(ControlPid),
                ?assertMatch(#connection_state{authenticated_state=unauthenticated}, State),
                {ok, State} = step(ControlPid),
                ?assertMatch(#connection_state{authenticated_state=unauthenticated}, State),
                step(ControlPid), % last event will  be disconnect with reason auth fail
                finish(ControlPid)
              end),
    execute(Child).

unauthenticated_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                    script_dialog([	{"CWD /hamster", "530 Not logged in.\r\n"},
                                      {"MKD /unicorns", "530 Not logged in.\r\n"}]),
                    {ok, StateCmd} = step(ControlPid),
                    ?assertMatch(#connection_state{authenticated_state=unauthenticated}, StateCmd),

                    {ok, StateMkd} = step(ControlPid),
                    ?assertMatch(#connection_state{authenticated_state=unauthenticated}, StateMkd),
                    finish(ControlPid)
                end),
    execute(Child).

ssl_only_test() ->
    setup(),
    ControlPid = self(),
    ok = meck:expect(fake_server, init, fun(InitialState, _Opt) ->
                       InitialState#connection_state{ssl_mode=only, utf8=false} end),

   Child = spawn_link(fun() ->
                       script_dialog([ {"USER hamster", "534 Request denied for policy reasons (only ftps allowed).\r\n"},
                                       {"MKD /unicorns", "534 Request denied for policy reasons (only ftps allowed).\r\n"},
                                       {"CWD /hamster", "534 Request denied for policy reasons (only ftps allowed).\r\n"},
                                       {"PWD", "534 Request denied for policy reasons (only ftps allowed).\r\n"},
                                       {"OPTS UTF8 ON", "501 Syntax error in parameters or arguments.\r\n"},
                                        {"FEAT", "211-Features\r\n"},
                                          {resp, socket, " AUTH TLS\r\n" },
                                          {resp, socket, " PROT\r\n" },
                                          {resp, socket, "211 End\r\n" }
                                           ]),
                       step(ControlPid),
                       step(ControlPid),
                       step(ControlPid),
                       step(ControlPid),
                       step(ControlPid),
                       step(ControlPid),
                       finish(ControlPid)
                   end),
   execute(Child).

mkdir_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      login_test_user(ControlPid,
                                      [{"MKD test_dir", "250 \"test_dir\" directory created.\r\n"},
                                       {"XMKD xdir", "250 \"xdir\" directory created.\r\n"},
                                       {"MKD test_dir_2", "550 Unable to create directory.\r\n"}]),

                      meck:expect(fake_server, make_directory, fun(State, "test_dir") -> {ok, State} end),
                      step(ControlPid),

                      meck:expect(fake_server, make_directory, fun(State, "xdir") -> {ok, State} end),
                      step(ControlPid),

                      meck:expect(fake_server, make_directory, fun(_State, "test_dir_2") -> {error, error} end),
                      step(ControlPid),

                      finish(ControlPid)
              end),
    execute(Child).


rmdir_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      login_test_user(ControlPid,
                                      [{"rmd killdir", "200 Command okay.\r\n"},
                                       {"xrmd xdirkill", "200 Command okay.\r\n"},
                                       {"rmd dir2kill", "550 Unable to remove directory.\r\n"}]),

                      meck:expect(fake_server, remove_directory, fun(State, "killdir") -> {ok, State} end),
                      step(ControlPid),

                      meck:expect(fake_server, remove_directory, fun(State, "xdirkill") -> {ok, State} end),
                      step(ControlPid),

                      meck:expect(fake_server, remove_directory, fun(_State, "dir2kill") -> {error, error} end),
                      step(ControlPid),

                      finish(ControlPid)
              end),
    execute(Child).

cwd_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      meck:expect(fake_server,
                                  change_directory,
                                  fun	(State, "/meat/bovine/bison") ->
                                            {ok, State}
                                  end),
                      meck:expect(fake_server,
                                  current_directory,
                                  fun(_) -> "/meat/bovine/bison" end),

                      login_test_user(ControlPid,
                            [{"CWD /meat/bovine/bison", "250 Directory changed to \"/meat/bovine/bison\".\r\n"},
                            {"CWD /meat/bovine/auroch", "550 Unable to change directory.\r\n"},
                            {"CWD /meat/bovine/elefant", "550 Unable to change directory (denied).\r\n"},
                            {"XCWD /meat/bovine/auroch", "550 Unable to change directory.\r\n"}]),
                      step(ControlPid),

                      meck:expect(fake_server, change_directory,
                                  fun(State, "/meat/bovine/auroch") -> {error, State} end),
                      step(ControlPid),

                      meck:expect(fake_server, change_directory,
                                 fun(_State, "/meat/bovine/elefant") -> {error, denied} end),
                      step(ControlPid),

                      meck:expect(fake_server, change_directory,
                                  fun(State, "/meat/bovine/auroch") -> {error, State} end),
                      step(ControlPid),

                      finish(ControlPid)
              end),
    execute(Child).

cdup_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      meck:expect(fake_server, change_directory, fun(State, "..") -> {ok, State} end),
                      login_test_user(ControlPid,
                                      [{"CDUP", "250 Directory changed to \"/zero\".\r\n"},
                                       {"CDUP", "250 Directory changed to \"/1st\".\r\n"},
                                       {"CDUP", "250 Directory changed to \"/2nd\".\r\n"},
                                       {"XCUP", "250 Directory changed to \"/3rd\".\r\n"}]),

                      meck:expect(fake_server, current_directory, fun(_) -> "/zero" end),
                      step(ControlPid),

                      meck:expect(fake_server, current_directory, fun(_) -> "/1st" end),
                      step(ControlPid),

                      meck:expect(fake_server, current_directory, fun(_) -> "/2nd" end),
                      step(ControlPid),

                      meck:expect(fake_server, current_directory, fun(_) -> "/3rd" end),
                      step(ControlPid),

                      finish(ControlPid)
              end),
    execute(Child).

pwd_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      login_test_user(ControlPid, [	{"PWD", "257 \"/meat/bovine/bison\"\r\n"},
                                                      {"XPWD","257 \"/opt/local/share\"\r\n"}	]),

                      meck:expect(fake_server, current_directory, fun(_) -> "/meat/bovine/bison" end),
                      step(ControlPid),

                      meck:expect(fake_server, current_directory, fun(_) -> "/opt/local/share" end),
                      step(ControlPid),

                      finish(ControlPid)
              end),
    execute(Child).

passive_anyport_successful_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                meck:expect(gen_tcp, listen, fun(0, _) -> {ok, listen_socket} end),
                meck:expect(inet, sockname,  fun(listen_socket) -> {ok, {{127, 0, 0, 1}, 2000}} end),

                login_test_user(ControlPid, [{"PASV", "227 Entering Passive Mode (127,0,0,1,7,208)\r\n"}]),
                ?assertMatch({ok, #connection_state{pasv_listen={passive, listen_socket, {{127,0,0,1}, 2000}}}},
                    step(ControlPid)),
                finish(ControlPid)
              end),
    execute(Child).

passive_anyport_failure_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                meck:expect(gen_tcp, listen, fun(0, _) -> {error, eaddrinuse} end),
                login_test_user(ControlPid, [{"PASV", "425 Can't open data connection.\r\n"}]),
                ?assertMatch({ok, #connection_state{pasv_listen=undefined}}, step(ControlPid)),
                finish(ControlPid)
              end),
    execute(Child).

passive_port_range_successful_test() ->
    setup(),
    ok = meck:expect(fake_server, init,
        fun(InitialState, _Opt) -> InitialState#connection_state{port_range={2000, 3000}} end),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                meck:expect(gen_tcp, listen, fun	(2500, _) -> {ok, listen_socket_2500};
                                                     (N, _) when is_integer(N) -> {error, eaddrinuse} end),

                meck:expect(inet, sockname,  fun(listen_socket_2500) -> {ok, {{127, 0, 0, 1}, 2500}} end),

                login_test_user(ControlPid, [{"PASV", "227 Entering Passive Mode (127,0,0,1,9,196)\r\n"}]),
                  ok = meck:new(random, [unstick]),
                ok = meck:expect(random, uniform, fun(M) -> M end),
                ?assertMatch({ok, #connection_state{pasv_listen={passive, listen_socket_2500, {{127,0,0,1}, 2500}}}},
                    step(ControlPid)),
                  ok = meck:unload(random),
                finish(ControlPid)
              end),
    execute(Child).

passive_port_range_failure_test() ->
    setup(),
    ok = meck:expect(fake_server, init,
        fun(InitialState, _Opt) -> InitialState#connection_state{port_range={2000, 4000}} end),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                meck:expect(gen_tcp, listen, fun(N, _) when is_integer(N), N>=2000, 4000>=N -> {error, eaddrinuse} end),
                  ok = meck:new(random, [unstick]),
                ok = meck:expect(random, uniform, fun(M) -> M end),
                login_test_user(ControlPid, [{"PASV", "425 Can't open data connection.\r\n"}]),
                ?assertMatch({ok, #connection_state{pasv_listen=undefined}}, step(ControlPid)),
                  ok = meck:unload(random),
                finish(ControlPid)
              end),
    execute(Child).

login_test_user_with_data_socket(ControlPid, Script, passive) ->
    ok = meck:expect(gen_tcp, listen, fun(0, _) -> {ok, listen_socket} end),
    ok = meck:expect(gen_tcp, accept, fun(listen_socket) -> {ok, data_socket} end),

    ok = meck:expect(inet, sockname,  fun	(listen_socket) ->	{ok, {{127, 0, 10, 1}, 2000}};
                                             (data_socket) ->	{ok, {{127, 0, 10, 1}, 2001}} end),
    ok = meck:expect(inet, peername, fun	(listen_socket) ->	{ok, {{127, 0, 0, 1}, 2000}};
                                            (data_socket) ->	{ok, {{127, 0, 0, 1}, 2001}} end),

    login_test_user(ControlPid, [{"PASV", "227 Entering Passive Mode (127,0,0,1,7,208)\r\n"}] ++ Script),
    ?assertMatch(
        {ok, #connection_state{pasv_listen={passive, listen_socket, {{127,0,0,1}, 2000}}}}, 	step(ControlPid));

login_test_user_with_data_socket(ControlPid, Script, active) ->
    meck:expect(gen_tcp,
                connect,
                fun(_, _, _) ->
                        {ok, data_socket}
                end),
    ok = meck:expect(inet, sockname,  fun	(listen_socket) ->	{ok, {{127, 0, 10, 1}, 2000}};
                                             (data_socket) ->	{ok, {{127, 0, 10, 1}, 2001}} end),
    ok = meck:expect(inet, peername, fun	(listen_socket) ->	{ok, {{127, 0, 0, 1}, 2000}};
                                            (data_socket) ->	{ok, {{127, 0, 0, 1}, 2001}} end),
    login_test_user(ControlPid, [{"PORT 127,0,0,1,7,208", "200 Command okay.\r\n"}] ++ Script),
    ?assertMatch({ok, #connection_state{data_port={active, {127,0,0,1}, 2000}}}, step(ControlPid)).

?dataSocketTest(nlst_test).
nlst_test(Mode) ->
    setup(),
    meck:expect(fake_server,
                list_files,
                fun(_, _) ->
                        [#file_info{type=file, name="edward"},
                         #file_info{type=dir, name="Aethelred"}]
                end),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      ok = meck:expect(gen_tcp, close, fun(data_socket) -> ok end),
                      ok = meck:expect(inet, setopts, fun(data_socket,[{recbuf, _Size}]) -> ok end),
                      login_test_user_with_data_socket(ControlPid,
                                                       [{"NLST", "150 File status okay; about to open data connection.\r\n"},
                                                       {resp, data_socket, "edward\r\n"},
                                                       {resp, data_socket, "Aethelred\r\n"},
                                                       {resp, socket, "226 Closing data connection.\r\n"}],
                                                       Mode),
                      step(ControlPid),
                      finish(ControlPid)
              end),
    execute(Child).

?dataSocketTest(list_test).
list_test(Mode) ->
    setup(),
    meck:expect(fake_server,
                list_files,
                fun(_, _) ->
                        [#file_info{type=file,
                                    name="edward",
                                    mode=511,
                                    gid=0,
                                    uid=0,
                                    mtime={{3019,12,12},{12,12,12}},
                                    size=512},
                         #file_info{type=dir,
                                    name="Aethelred",
                                    mode=200,
                                    gid=0,
                                    uid=0,
                                    mtime={{3019,12,12},{12,12,12}},
                                    size=0}]
                end),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      Script = [{"LIST", "150 File status okay; about to open data connection.\r\n"},
                                {resp, data_socket, "-rwxrwxrwx  1     0     0      512 Dec 12 12:12 edward\r\n"},
                                {resp, data_socket, "d-wx--x---  4     0     0        0 Dec 12 12:12 Aethelred\r\n"},
                                {resp, socket, "226 Closing data connection.\r\n"}],

                      ok = meck:expect(gen_tcp, close, fun(data_socket) -> ok end),
                      ok = meck:expect(inet, setopts, fun(data_socket,[{recbuf, _Size}]) -> ok end),
                      login_test_user_with_data_socket(ControlPid,Script,Mode),
                      step(ControlPid),
                      finish(ControlPid)
              end),
    execute(Child).

remove_file_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
             fun() ->
                     meck:expect(fake_server,
                                 remove_file,
                                 fun(St, "cheese.txt") ->
                                         {ok, St}
                                 end),

                     login_test_user(ControlPid, [{"DELE cheese.txt", "250 Requested file action okay, completed.\r\n"},
                                              {"DELE cheese.txt", "450 Unable to delete file.\r\n"}]),
                     step(ControlPid),

                     meck:expect(fake_server,
                                 remove_file,
                                 fun(_, "cheese.txt") ->
                                         {error, error}
                                 end),

                     step(ControlPid),
                     finish(ControlPid)
             end),
    execute(Child).

?dataSocketTest(stor_test).
stor_test(Mode) ->
    setup(),
    ControlPid = self(),

    ok = meck:expect(fake_server, init, fun(InitialState, _Opt) ->
                        InitialState#connection_state{recv_block_size=1024*1024} end),
    Child = spawn_link(
             fun() ->
                     Script = [{"STOR file.txt", "150 File status okay; about to open data connection.\r\n"},
                               {req, data_socket, <<"SOME DATA HERE">>},
                               {resp, socket, "226 Closing data connection.\r\n"},
                               {"PWD", "257 \"/\"\r\n"}
                               ],
                     meck:expect(fake_server,
                                 put_file,
                                 fun(S, "file.txt", write, F) ->
                                         {ok, Data, DataSize} = F(),
                                         BinData = <<"SOME DATA HERE">>,
                                         ?assertEqual(Data, BinData),
                                         ?assertEqual(DataSize, size(BinData)),
                                         {ok, S}
                                 end),

                     ok = meck:expect(gen_tcp, close, fun(data_socket) -> ok end),
                     ok = meck:expect(inet, setopts,
                                     fun(data_socket, Opts) ->
                                         ?assertEqual(1024*1024, proplists:get_value(recbuf, Opts)),
                                         ok
                                    end),

                     login_test_user_with_data_socket(ControlPid, Script, Mode),
                     step(ControlPid), % STOR

                     ok = meck:expect(fake_server,	put_file,
                             fun(S, "file.txt", notification, done) -> {ok, S} end),

                     ok = meck:expect(fake_server,	current_directory,	fun(_) -> "/" end),
                     step(ControlPid), % PWD


                     finish(ControlPid)
             end),
    execute(Child).

?dataSocketTest(stor_user_failure_test).
stor_user_failure_test(Mode) ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
        fun() ->
            Script=[	{"STOR elif.txt", "150 File status okay; about to open data connection.\r\n"},
                        {req, data_socket, <<"SOME DATA HERE">>},
                        {resp, socket, "451 Error access_denied when storing a file.\r\n"},
                        {"QUIT", "200 Goodbye.\r\n"}],
            ok = meck:expect(fake_server, put_file,
                    fun(_, "elif.txt", write, F) ->
                        F(),
                        {error, access_denied}
                    end),

            ok = meck:expect(gen_tcp, close, fun(data_socket) -> ok end),
            ok = meck:expect(inet, setopts, fun(data_socket,[{recbuf, _Size}]) -> ok end),
            login_test_user_with_data_socket(ControlPid, Script, Mode),
            step(ControlPid),

            meck:expect(fake_server, disconnect, fun(_, exit) -> ok end),
            meck:expect(gen_tcp, close, fun(socket) -> ok end),
%			not needed, because USER kills itself
%			meck:expect(fake_server,put_file, fun(S, "elif.txt", notification, terminated) -> {ok, S} end),
            step(ControlPid),
            finish(ControlPid)
        end),
    execute(Child).

?dataSocketTest(stor_notificaion_test).
stor_notificaion_test(Mode) ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
    fun() ->
        Script=[	{"STOR ok.txt", "150 File status okay; about to open data connection.\r\n"},
                    {req, data_socket, <<"SOME DATA HERE">>},
                    {resp, socket, "226 Closing data connection.\r\n"},

                    {"STOR bad.txt", "150 File status okay; about to open data connection.\r\n"},
                    {req, data_socket, <<"SOME DATA HERE">>},
                    {resp, socket, "226 Closing data connection.\r\n"},
                    {req_error, socket, {error, closed}}
                ],

            ok = meck:expect(inet, setopts, fun(data_socket,[{recbuf, _Size}]) -> ok end),
            ok = meck:expect(gen_tcp, close, fun(data_socket) -> ok end),
            ok = meck:expect(fake_server, put_file, fun(S, "ok.txt", write, F) ->
                                        {ok, Data, DataSize} = F(),
                                        BinData = <<"SOME DATA HERE">>,
                                        ?assertEqual(Data, BinData),
                                        ?assertEqual(DataSize, size(BinData)),
                                        {ok, S}
                                    end),

            ok = meck:expect(gen_tcp, close, fun(data_socket) -> ok end),
            login_test_user_with_data_socket(ControlPid, Script, Mode),
            step(ControlPid), % STOR(OK)

            ok = meck:expect(fake_server, put_file,
                                fun	(S, "bad.txt", write, F) ->
                                        {ok, Data, DataSize} = F(),
                                            BinData = <<"SOME DATA HERE">>,
                                            ?assertEqual(Data, BinData),
                                            ?assertEqual(DataSize, size(BinData)),
                                        {ok, S};
                                    (S, "ok.txt", notification, done) ->
                                        {ok, S}
                                end),
            step(ControlPid), % STOR(BAD)


            ok = meck:expect(fake_server, put_file,
                                fun	(S, "bad.txt", notification, Result) ->
                                        ?assertEqual(terminated, Result),
                                        {ok, S}
                                end),
            ok = meck:expect(fake_server, disconnect, fun(_, {error, {error, closed}}) -> ok end),
            step(ControlPid), % CRASH CONNECTION
            finish(ControlPid)
        end),
    execute(Child).


?dataSocketTest(stor_failure_test).
stor_failure_test(Mode) ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
        fun() ->
            Script=[	{"STOR elif.txt", "150 File status okay; about to open data connection.\r\n"},
                        {req, data_socket, <<"SOME DATA HERE">>},
                        {resp, socket, "451 Error access_denied when storing a file.\r\n"},
                        {"QUIT", "200 Goodbye.\r\n"}],
            ok = meck:expect(fake_server, put_file,
                    fun(_, "elif.txt", write, F) ->
                        F(),
                        {error, access_denied}
                    end),

            ok = meck:expect(gen_tcp, close, fun(data_socket) -> ok end),
            ok = meck:expect(inet, setopts, fun(data_socket,[{recbuf, _Size}]) -> ok end),
            login_test_user_with_data_socket(ControlPid, Script, Mode),
            step(ControlPid),

            meck:expect(fake_server, disconnect, fun(_, exit) -> ok end),
            meck:expect(gen_tcp, close, fun(socket) -> ok end),
%			meck:expect(fake_server,put_file, fun(S, "elif.txt", notification, terminated) -> {ok, S} end),
%			not needed, because USER kills itself
            step(ControlPid),
            finish(ControlPid)
        end),
    execute(Child).

?dataSocketTest(retr_test).
retr_test(Mode) ->
    setup(),
    ok = meck:expect(fake_server, init, fun(InitialState, _Opt) ->
                        InitialState#connection_state{send_block_size=1024*1024} end),
    ControlPid = self(),
    Child = spawn_link(
             fun() ->
                     Script = [{"RETR bologna.txt", "150 File status okay; about to open data connection.\r\n"},
                               {resp, data_socket, "SOME DATA HERE"},
                               {resp, data_socket, "SOME MORE DATA"},
                               {resp, socket, "226 Closing data connection.\r\n"}],
                     meck:expect(fake_server,
                                 get_file,
                                 fun(State, "bologna.txt") ->
                                         {ok,
                                          fun(1024*1024) ->
                                                  {ok,
                                                   list_to_binary("SOME DATA HERE"),
                                                   fun(1024*1024) ->
                                                           {ok,
                                                            list_to_binary("SOME MORE DATA"),
                                                            fun(1024*1024) -> {done, State} end}
                                                   end}
                                          end}
                                 end),

                     ok = meck:expect(gen_tcp, close, fun(data_socket) -> ok end),
                     ok = meck:expect(inet, setopts, fun(data_socket,[{recbuf, _Size}]) -> ok end),

                     login_test_user_with_data_socket(ControlPid, Script, Mode),
                     step(ControlPid),
                     finish(ControlPid)
             end),
    execute(Child).

?dataSocketTest(retr_failure_test).
retr_failure_test(Mode) ->
    setup(),
    ok = meck:expect(fake_server, init, fun(InitialState, _Opt) ->
                        InitialState#connection_state{send_block_size=1024} end),
    ControlPid = self(),
    Child = spawn_link(
             fun() ->
                     Script = [{"RETR bologna.txt", "150 File status okay; about to open data connection.\r\n"},
                               {resp, data_socket, "SOME DATA HERE"},
                               {resp, socket, "451 Unable to get file (Disk error).\r\n"}],
                     meck:expect(fake_server,
                                 get_file,
                                 fun(State, "bologna.txt") ->
                                         {ok,
                                          fun(1024) ->
                                                  {ok,
                                                   list_to_binary("SOME DATA HERE"),
                                                   fun(1024) ->
                                                               {error, "Disk error", State}
                                                   end}
                                          end}
                                 end),

                     ok = meck:expect(gen_tcp, close, fun(data_socket) -> ok end),
                     ok = meck:expect(inet, setopts, fun(data_socket,[{recbuf, _Size}]) -> ok end),

                     login_test_user_with_data_socket(ControlPid, Script, Mode),
                     step(ControlPid),
                     finish(ControlPid)
             end),
    execute(Child).

rein_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      login_test_user(ControlPid, [{"REIN", "200 Command okay.\r\n"}]),
                      ControlPid ! {ack, self()},
                      receive
                          {new_state, _, #connection_state{authenticated_state=unauthenticated}} ->
                              ok;
                          _ ->
                              ?assert(fail)
                      end,
                      finish(ControlPid)
              end),
    execute(Child).

mdtm_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      meck:expect(fake_server,
                                  file_info,
                                  fun(_, "cheese.txt") ->
                                          {ok,
                                           #file_info{type=file,
                                                      mtime={{2012,2,3},{16,3,12}}}}
                                  end),
                      login_test_user(ControlPid, [{"MDTM cheese.txt", "213 20120203160312\r\n"}]),
                      step(ControlPid),
                      finish(ControlPid)
              end),
    execute(Child).

mdtm_truncate_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      meck:expect(fake_server,
                                  file_info,
                                  fun(_, "mould.txt") ->
                                          {ok,
                                           #file_info{type=file,
                                                      mtime={{2012,2,3},{16,3,11.933844}}}}
                                  end),
                      login_test_user(ControlPid, [{"MDTM mould.txt", "213 20120203160311\r\n"}]),
                      step(ControlPid),
                      finish(ControlPid)
              end),
    execute(Child).

rnfr_rnto_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      login_test_user(ControlPid,
                                      [{"RNTO mushrooms.txt", "503 RNFR not specified.\r\n"},
                                       {"RNFR cheese.txt", "350 Ready for RNTO.\r\n"},
                                       {"RNTO mushrooms.txt", "250 Rename successful.\r\n"}]),
                      step(ControlPid),

                      meck:expect(fake_server,
                                  rename_file,
                                  fun(S, "cheese.txt", "mushrooms.txt") ->
                                          {ok, S}
                                  end),
                      step(ControlPid),
                      step(ControlPid),
                      finish(ControlPid)
              end),
    execute(Child).

type_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      login_test_user(ControlPid,
                                      [{"TYPE I", "200 Command okay.\r\n"},
                                       {"TYPE X", "501 Only TYPE I or TYPE A may be used.\r\n"}]),
                      step(ControlPid),
                      step(ControlPid),
                      finish(ControlPid)
              end
             ),
    execute(Child).

site_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      meck:expect(fake_server,
                                  site_command,
                                  fun(S, monkey, "cheese bits") ->
                                          {ok, S}
                                  end),
                      login_test_user(ControlPid,
                                      [{"SITE MONKEY cheese bits", "200 Command okay.\r\n"},
                                       {"SITE GORILLA cheese", "500 Syntax error, command unrecognized.\r\n"}]),
                      step(ControlPid),

                      meck:expect(fake_server,
                                  site_command,
                                  fun(_, gorilla, "cheese") ->
                                          {error, not_found}
                                  end),
                      step(ControlPid),
                      finish(ControlPid)
              end
             ),
    execute(Child).

help_site_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      meck:expect(fake_server,
                                  site_help,
                                  fun (_) ->
                                          {ok, [{"MEAT", "devour the flesh of beasts."}]}
                                  end),
                      Script = [{"HELP SITE", "214-The following commands are recognized\r\n"},
                                {resp, socket, "MEAT : devour the flesh of beasts.\r\n"},
                                {resp, socket, "214 Help OK\r\n"}],
                      login_test_user(ControlPid, Script),
                      step(ControlPid),
                      finish(ControlPid)
              end
             ),
    execute(Child).

unrecognized_command_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      login_test_user(ControlPid, [{"FEED buffalo", "500 Syntax error, command unrecognized.\r\n"}]),
                      step(ControlPid),
                      finish(ControlPid)
              end
             ),
    execute(Child).

quit_test() ->
    setup(),
    meck:expect(gen_tcp, close, fun(socket) -> ok end),
    ControlPid = self(),
    Child = spawn_link(
              fun () ->
                      meck:expect(fake_server, disconnect, fun(_, exit) ->  ok end),
                      login_test_user(ControlPid, [{"QUIT", "200 Goodbye.\r\n"}]),
                      step(ControlPid),
                      finish(ControlPid)
              end),
    execute(Child).

feat_test() ->
   setup(),
   ControlPid = self(),
   Child = spawn_link(
           fun() ->
               login_test_user(ControlPid,
                   [   {"FEAT", "211-Features\r\n"},
                       {resp, socket, " UTF8\r\n" },
                       {resp, socket, "211 End\r\n" }]),
               step(ControlPid),
               finish(ControlPid)
           end),
   execute(Child).

?dataSocketTest(utf8_success_test).
utf8_success_test(Mode) ->
   setup(),
   ControlPid = self(),
   Child = spawn_link(
           fun() ->
               FileName = "Молоко-Яйки", %milk-eggs
               UtfFileName = to_utf8(FileName), %milk-eggs
               BinData = <<"SOME DATA HERE">>,

               Script=[{"PWD " ++ UtfFileName, "257 \""++ UtfFileName ++"\"\r\n"},
                          {"OPTS UTF8 ON", "200 Accepted.\r\n"},
                       {"CWD " ++ UtfFileName, "250 Directory changed to \""++ UtfFileName ++"\".\r\n"},
                       {"STOR " ++ UtfFileName, "150 File status okay; about to open data connection.\r\n"},
                       {req, data_socket, BinData},
                       {resp, socket, "226 Closing data connection.\r\n"},
                       {"LIST", "150 File status okay; about to open data connection.\r\n"},
                       {resp, data_socket, "d-wx--x---  4     0     0        0 Dec 12 12:12 "++UtfFileName++"\r\n"},
                       {resp, socket, "226 Closing data connection.\r\n"}],

               ok = meck:expect(fake_server,current_directory, fun(_) -> FileName end),

               login_test_user_with_data_socket(ControlPid, Script, Mode),
               step(ControlPid),
               step(ControlPid),

               meck:expect(fake_server,change_directory,
                               fun(State, InFileName) ->
                                   ?assertEqual(InFileName, FileName),
                                   {ok, State}
                               end),
               step(ControlPid),

               meck:expect(fake_server,put_file,
                               fun(S, InFileName, write, F) ->
                                   ?assertEqual(InFileName, FileName),
                                   {ok, Data, DataSize} = F(),
                                   ?assertEqual(Data, BinData),
                                   ?assertEqual(DataSize, size(BinData)),
                                   {ok, S}
                               end),

               ok = meck:expect(gen_tcp, close, fun(data_socket) -> ok end),
               ok = meck:expect(inet, setopts, fun(data_socket,[{recbuf, _Size}]) -> ok end),

               step(ControlPid),

               meck:expect(fake_server, put_file,
                                fun(S, InFileName, notification, done) ->
                                   ?assertEqual(InFileName, FileName),
                                   {ok, S}
                               end),

               meck:expect(fake_server, list_files,
                               fun(_, _) ->
                                   [#file_info{type=dir,name=FileName,mode=200,gid=0,uid=0,
                                               mtime={{3019,12,12},{12,12,12}},size=0}]
                               end),

               ok = meck:expect(gen_tcp, close, fun(data_socket) -> ok end),
               ok = meck:expect(inet, setopts, fun(data_socket,[{recbuf, _Size}]) -> ok end),

               step(ControlPid),
               finish(ControlPid)
           end),
   execute(Child).

?dataSocketTest(utf8_failure_test).
utf8_failure_test(Mode) ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
        fun() ->
            FileName = "Молоко-Яйки", %milk-eggs
            UtfFileNameOk = to_utf8(FileName), %milk-eggs
            {UtfFileNameErr, _} = lists:split(length(UtfFileNameOk)-1, UtfFileNameOk),

            Script =[	{"OPTS UTF8 ON", "200 Accepted.\r\n"},
                        {"CWD " ++ UtfFileNameErr, "501 Syntax error in parameters or arguments.\r\n"}],

            ok = meck:expect(gen_tcp, close, fun(data_socket) -> ok end),
            ok = meck:expect(error_logger, warning_report, fun({?REPORT_TAG, Report}) ->
                ?assertMatch({utf8, incomplete, _, _}, Report),
                ok end),
            login_test_user_with_data_socket(ControlPid, Script, Mode),
            step(ControlPid),
            step(ControlPid),
            finish(ControlPid)
        end),
    execute(Child).

-endif.
