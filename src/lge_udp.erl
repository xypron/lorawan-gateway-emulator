% SPDX-License-Identifier: MIT
%%%-------------------------------------------------------------------
%%% @author Heinrich Schuchardt <xypron.glpk@gmx.de>
%%% @copyright (C) 2018, Heinrich Schuchardt <xypron.glpk@gmx.de>
%%% @doc
%%% This server handles UDP messages.
%%%
%%% @end
%%% Created : 2018-11-25 13:05:01.521613
%%%-------------------------------------------------------------------
-module(lge_udp).

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

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
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
    Socket = openSocket(),
    State = #{
        socket => Socket
    },
    {ok, State}.

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
handle_cast({send, Msg}, State) ->
    << _:8, _:16, Type:8, _/binary>> = Msg,
    case Type of
        % PUSH_DATA
        0 ->
            io:format("<- PUSH_DATA~n");
        % PULL_DATA
        2 ->
            io:format("<- PULL_DATA~n");
        % TX_ACK
        5 ->
            io:format("<- TX_ACK~n");
        _ELSE ->
            io:format("<- Unknow message type ~p~n", [Type])
    end,
    Socket = maps:get(socket, State),
    Ip = lge_util:get_ip(),
    Port = 1680,
    gen_udp:send(Socket, Ip, Port, Msg),
    {noreply, State};
handle_cast(Msg, State) ->
    io:format("unknown cast ~p", [Msg]),
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
handle_info({udp, _Socket, _Ip, _Port, Msg}, State) ->
    << _:8, _:16, Type:8, _/binary>> = Msg,
    case Type of
        % PUSH_ACK
        1 ->
            io:format("-> PUSH_ACK~n");
        % PULL_RESP
        3 ->
            io:format("-> PULL_RESP~n"),
            gen_server:cast(lge_push_data, {resp, Msg});
        % PULL_ACK
        4 ->
            io:format("-> PULL_ACK~n");
        _ELSE ->
            io:format("-> Unknow message type ~p~n", [Type])
    end,
    {noreply, State};
handle_info(Info, State) ->
    io:format("unknown info ~p", [Info]),
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
terminate(_Reason, State) ->
    Socket = maps:get(socket, State),
    udp_gen:close(Socket),
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

openSocket() ->
    case gen_udp:open(1681, [binary]) of
        {ok, Socket} -> Socket;
        {error, Reason} -> error(Reason)
    end.
