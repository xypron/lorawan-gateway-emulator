% SPDX-License-Identifier: MIT
%%%-------------------------------------------------------------------
%%% @author Heinrich Schuchardt <xypron.glpk@gmx.de>
%%% @copyright (C) 2018, Heinrich Schuchardt <xypron.glpk@gmx.de>
%%% @doc
%%% This module contains routines to access the database.
%%%
%%% @end
%%% Created : 2018-12-02 15:05:01.521613
%%%-------------------------------------------------------------------

-module(lge_db).

-export([create_tables/0]).

%%% @headerfile "lge_db.hrl"
-include("lge_db.hrl").

%%--------------------------------------------------------------------
%% @doc
%% This function creates the database tables.
%%
%% @spec create_tables() -> ok
%%
%% @end
%%--------------------------------------------------------------------
create_tables() ->
    % Create the database scheme
    stopped = mnesia:stop(),
    case mnesia:create_schema([node()]) of
        ok ->
            ok;
        {error, {_, {already_exists, _}}} ->
            ok
    end,
    % Start the data base
    ok = mnesia:start(),
    % Create the tables
    lists:foreach(fun({T, A}) -> create_table(T, A) end, [
        {servers, [
                {attributes, record_info(fields, server)},
                {disc_copies, [node()]}
            ]},
        {gateways, [
                {attributes, record_info(fields, gateway)},
                {disc_copies, [node()]}
            ]},
        {otaas, [
                {attributes, record_info(fields, otaa)},
                {disc_copies, [node()]}
            ]},
        {devices, [
                {attributes, record_info(fields, device)},
                {disc_copies, [node()]}
            ]}
        ]
    ),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% This function creates a single database table.
%%
%% @spec create_table(Table, Attributes) -> ok
%%
%% @end
%%--------------------------------------------------------------------
create_table(Table, Attributes) ->
    case lists:member(Table, mnesia:system_info(tables)) of
        false ->
            {atomic, ok} = mnesia:create_table(Table, Attributes),
            ok = mnesia:wait_for_tables([Table], 2000);
        _true ->
            ok
    end.
