% SPDX-License-Identifier: MIT
%%%-------------------------------------------------------------------
%%% @author Heinrich Schuchardt <xypron.glpk@gmx.de>
%%% @copyright (C) 2018, Heinrich Schuchardt <xypron.glpk@gmx.de>
%%% @doc
%%% Top level supervisor.
%%%
%%% @end
%%% Created : 2018-11-25 12:52:16.081031
%%%-------------------------------------------------------------------
-module(lge_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},

    ChildSpecs = [
        #{id => lge_cmdline,
          start => {lge_cmdline, start_link, []},
          shutdown => brutal_kill,
          modules => [lge_cmdline]}
          ,
        #{id => lge_pull_data,
          start => {lge_pull_data, start_link, []},
          shutdown => brutal_kill,
          modules => [lge_pull_data]}
          ,
        #{id => lge_push_data,
          start => {lge_push_data, start_link, []},
          shutdown => brutal_kill,
          modules => [lge_push_data]}
          ,
        #{id => lge_udp,
          start => {lge_udp, start_link, []},
          shutdown => brutal_kill,
          modules => [lge_udp]}
    ],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
