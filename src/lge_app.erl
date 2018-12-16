% SPDX-License-Identifier: MIT
%%%-------------------------------------------------------------------
%%% @author Heinrich Schuchardt <xypron.glpk@gmx.de>
%%% @copyright (C) 2018, Heinrich Schuchardt <xypron.glpk@gmx.de>
%%% @doc
%%% LoRaWAN Gateway Emulator.
%%%
%%% @end
%%% Created : 2018-11-25 12:10:39.018312
%%%-------------------------------------------------------------------
-module(lge_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% This function starts the application.
%%
%% @spec start() -> {ok, Started} |
%%                  {error, Reason}
%% @end
%%--------------------------------------------------------------------
start() ->
    {ok, _Started} = application:ensure_all_started(lge).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function starts the application after checking the OTP
%% Release.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    ok = check_otp_release(19),
    ok = lge_db:create_tables(),
    lge_sup:start_link().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever the application has stopped. The
%% return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check that the Erlang version is high enough
%%
%% @spec check_otp_release(Integer) -> ok |
%%                                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
check_otp_release(Min) ->
    Num = list_to_integer(erlang:system_info(otp_release)),
    if Num >= Min -> ok;
        true -> {error, "OTP release too low"}
    end.
