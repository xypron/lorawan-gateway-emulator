% SPDX-License-Identifier: MIT
%%%-------------------------------------------------------------------
%%% @author Heinrich Schuchardt <xypron.glpk@gmx.de>
%%% @copyright (C) 2018, Heinrich Schuchardt <xypron.glpk@gmx.de>
%%% @doc
%%% This server waits for command line input
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(lge_cmdline).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("lge_db.hrl").

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the comand line server.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    greet(),
    gen_server:cast(self(), start),
    {ok, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(start, State) ->
    command_line(),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages.
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed.
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_eui(Fieldname) ->
    Value = list_to_binary(
                string:to_upper(string:trim(io:get_line(Fieldname ++ ": ")))),
    Hex = lge_util:hex_to_binary(Value),
    case bit_size(Hex) of
        64 ->
            {ok, Hex};
        _else ->
            {error}
    end.

get_ip(Fieldname) ->
    Value = list_to_binary(string:trim(io:get_line(Fieldname ++ ": "))),
    List = string:split(Value, ".", all),
    Tuple = lists:foldl(
        fun(T, Acc) ->
            erlang:append_element(Acc, list_to_integer(binary_to_list(T)))
        end, {}, List),
    case size(Tuple) of
        4 ->
            {ok, Tuple};
        _else ->
            {error}
    end.

get_port(Fieldname) ->
    Value = list_to_binary(string:trim(io:get_line(Fieldname ++ ": "))),
    Port = list_to_integer(binary_to_list(Value)),
    if
        (Port > 0) and (Port =< 16#FFFF) ->
            {ok, Port};
        true ->
            {error}
    end.

get_string(Fieldname) ->
    Value = string:trim(io:get_line(Fieldname ++ ": ")),
    case string:length(Value) of
        0 ->
            {error};
        _else ->
            {ok, Value}
    end.

create([Keyword | _Arguments]) ->
    case Keyword of
    "gateway" ->
        try
            {ok, Eui} = get_eui("EUI-64"),
            {ok, Name} = get_string("Name"),
            {ok, Server} = get_string("Server"),
            Gateway = #gateway{eui = Eui, name = Name, server = Server},
            F = fun() ->
                case mnesia:read({server, Server}) of
                    [] ->
                        io:format("Server ~p does not exist~n", [Server]),
                        {error, unknown_server};
                    _else ->
                        mnesia:write(Gateway)
                end
            end,
            case mnesia:activity(transaction, F) of
                ok ->
                    io:format("Gateway ~p created~n", [Name]);
                _else ->
                    io:format("Failed to create gateway~n")
            end
        catch
            _ -> io:format("Invalid input~n")
        end,
        ok;
    "server" ->
        try
            {ok, Name} = get_string("Name"),
            {ok, Ip} = get_ip("Ip address"),
            {ok, Port} = get_port("Port"),
            Server = #server{name = Name, ip = Ip, port = Port},
            F = fun() ->
                mnesia:write(Server)
            end,
            case mnesia:activity(transaction, F) of
                ok ->
                    io:format("Server ~p created~n", [Name]);
                _else ->
                    io:format("Failed to create server~n")
            end
        catch
            _ -> io:format("Invalid input~n")
        end,
        ok;
    "otaa" -> ok;
    "device" -> ok;
        _else -> help(["create"])
    end,
    ok.

delete([Keyword | _Arguments]) ->
    ok.

list([Keyword | _Arguments]) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Write welcome message.
%%
%% @spec greet() -> ok
%% @end
%%--------------------------------------------------------------------
greet() ->
    io:format("~s~n", ["'quit' to exit"]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Provide online help.
%%
%% @spec help([Keyword | Arguments]) -> ok
%% @end
%%--------------------------------------------------------------------
help([]) ->
    io:format(
        "create - create item~n" ++
        "delete - delete item~n" ++
        "help - display this help~n" ++
        "list - list items~n" ++
        "quit - exit the program~n" ++
        "troff - stop trace output~n" ++
        "tron - start trace output~n"),
    ok;
help([Keyword | _Arguments]) ->
    case Keyword of
        "create" ->
            io:format(
                "create gateway~n" ++
                "create server~n" ++
                "create otaa~n" ++
                "create device~n");
        "delete" ->
            io:format(
                "delete gateway~n" ++
                "delete server~n" ++
                "delete otaa~n" ++
                "delete device~n");
        "list" ->
            io:format(
                "list gateways~n" ++
                "list servers~n" ++
                "list otaas~n" ++
                "list devices~n");
        _else ->
            help([])
    end,
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle entry on command line.
%%
%% @spec command_line() -> ok
%% @end
%%--------------------------------------------------------------------
command_line() ->
    Line = string:trim(io:get_line("> ")),
    [Keyword | Arguments] = string:split(Line, " "),
    case Keyword of
        "create" ->
            case Arguments of
                [] ->
                    help([Keyword]);
                _else ->
                    create(Arguments)
            end;
        "delete" ->
            case Arguments of
                [] ->
                    help([Keyword]);
                _else ->
                    delete(Arguments)
            end;
        "help" ->
            help(Arguments);
        "list" ->
            case Arguments of
                [] ->
                    help([Keyword]);
                _else ->
                    list(Arguments)
            end;
        "quit" ->
            io:format("stopping~n"),
            init:stop();
        "troff" ->
            gen_server:call(lge_log, troff);
        "tron" ->
            gen_server:call(lge_log, tron);
        _else ->
            help([])
    end,
    command_line().
