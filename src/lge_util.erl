%%%-------------------------------------------------------------------
%%% @author Heinrich Schuchardt <xypron.glpk@gmx.de>
%%% @copyright (C) 2018, Heinrich Schuchardt <xypron.glpk@gmx.de>
%%% @doc
%%% Utility functions.
%%%
%%% @end
%%% Created : 2018-11-25 12:59:34.101272
%%%-------------------------------------------------------------------
-module(lge_util).

%% API
-export([get_ip/0, get_mac/0, hex_to_binary/1, rand16/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Get ip address.
%%
%% @spec get_ip() -> {}
%% @end
%%--------------------------------------------------------------------
get_ip() ->
    V = application:get_env(lge, ip),
    if V == undefined ->
            Ip = {127, 0, 0, 1};
       true -> {_, Ip} = V
    end,
    if not is_tuple(Ip) ->
            {127, 0, 0, 1};
       4 /= tuple_size(Ip) ->
            {127, 0, 0, 1};
       true -> Ip
    end.

%%--------------------------------------------------------------------
%% @doc
%% Get own hardware address.
%%
%% @spec get_mac() -> binary
%% @end
%%--------------------------------------------------------------------
get_mac() ->
    V = application:get_env(lge, mac),
    if V == undefined ->
            Mac = <<1, 0, 0, 16#ff, 16#fe, 0, 0, 0>>;
       true -> {_, Mac} = V
    end,
    if not is_binary(Mac) ->
            <<3, 0, 0, 16#ff, 16#fe, 0, 0, 0>>;
       8 /= byte_size(Mac) ->
            <<2, 0, 0, 16#ff, 16#fe, 0, 0, 0>>;
       true -> Mac
    end.

%%--------------------------------------------------------------------
%% @doc
%% Get 16bit random number.
%%
%% @spec rand16() -> Integer
%% @end
%%--------------------------------------------------------------------
rand16() ->
    round(65536 * rand:uniform()) band 65535.

%%--------------------------------------------------------------------
%% @doc
%% Convert hexadecimal string to binary.
%%
%% @spec hex_to_binary(Hex) -> binary.
%% @end
%%--------------------------------------------------------------------
hex_to_binary(Hex) ->
    hex_to_binary(Hex, <<>>).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert hexadecimal string to binary.
%%
%% @spec hex_to_binary(Hex, <<>>) -> binary.
%% @end
%%--------------------------------------------------------------------
hex_to_binary(<<>>, Acc) ->
    Acc;
hex_to_binary(<<H:8, T/binary>>, Acc) ->
    if
        (H >= $0) and (H =< $9) ->
            N = <<(H - $0):4>>;
        (H >= $A) and (H =< $F) ->
            N = <<(H - $7):4>>;
        true ->
            N = <<>>
    end,
    hex_to_binary(<<T/binary>>, <<Acc/bitstring, N/bitstring>>).
