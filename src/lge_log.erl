% SPDX-License-Identifier: MIT
%%%-------------------------------------------------------------------
%%% @author Heinrich Schuchardt <xypron.glpk@gmx.de>
%%% @copyright (C) 2018, Heinrich Schuchardt <xypron.glpk@gmx.de>
%%% @doc
%%% This server provides the command line interface.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(lge_log).

-behaviour(gen_server).

%% API
-export([start_link/0, debug/1, debug/2]).

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
    {ok, troff}.

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
handle_call(troff, _From, State) ->
    {reply, State, troff};
handle_call(tron, _From, State) ->
    {reply, State, tron};
handle_call(_Request, _From, State) ->
    io:format("?~n"),
    {reply, ok, State}.

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
handle_cast({debug, Msg}, State) ->
    case State of
        tron ->
            io:format(Msg);
        _else ->
            ok
    end,
    {noreply, State};
handle_cast(Msg, State) ->
    io:format("handle_cast(~w,_)~n", [Msg]),
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
handle_info(Info, State) ->
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

%%--------------------------------------------------------------------
%% @doc
%% Print debug message.
%%
%% @spec debug(Format, Data) -> ok
%% @end
%%--------------------------------------------------------------------
debug(Format) ->
    debug(Format, []).
debug(Format, Data) ->
    Msg = io_lib:format(Format, Data),
    gen_server:cast(lge_log, {debug, Msg}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

