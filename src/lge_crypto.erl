% SPDX-License-Identifier: MIT
%%%-------------------------------------------------------------------
%%% @author Heinrich Schuchardt <xypron.glpk@gmx.de>
%%% @copyright (C) 2018, Heinrich Schuchardt <xypron.glpk@gmx.de>
%%% @doc
%%% This module contains routines for encryption and decryption of
%%% LoRaWAN messages
%%%
%%% @end
%%% Created : 2018-12-02 15:05:01.521613
%%%-------------------------------------------------------------------
-module(lge_crypto).

-export([encrypt_up/4, mic_up/4, xor_bin/2]).

-type session_key() :: [<<_:128>>].

-type message_integrity_code() :: [<<_:32>>].

-export_type([session_key/0, message_integrity_code/0]).

%%--------------------------------------------------------------------
%% @doc
%% Calculate message integrity code for uplink frames.
%%
%% @spec mic_up(Msg::binary, DevAddr::integer, Fcnt::integer,
%%              NetwkSKey::session_key) -> message_integrity_code
%% @end
%%--------------------------------------------------------------------
mic_up(Msg, DevAddr, Fcnt, NetwkSKey) ->
    B0 = <<73, 0, 0, 0, 0, 0, DevAddr:32/little-unsigned-integer,
           Fcnt:32/little-unsigned-integer, 0, (byte_size(Msg))>>,
    erlang:binary_part(crypto:cmac(aes_cbc128, NetwkSKey,
                       <<B0/binary, Msg/binary>>), 0, 4).

%%--------------------------------------------------------------------
%% @doc
%% Bytewise xor binary strings.
%%
%% @spec xor_bin(S1::binary, S2::binary) -> binary
%% @end
%%--------------------------------------------------------------------
xor_bin(S1, S2) ->
    xor_bin(S1, S2, <<>>).

xor_bin(<<>>, _S2, Acc) ->
    Acc;
xor_bin(<<H1:8, T1/binary>>, <<H2:8, T2/binary>>, Acc) ->
    xor_bin(T1, T2, <<Acc/binary, (H1 bxor H2):8>>).

%%--------------------------------------------------------------------
%% @doc
%% Encrypt push message.
%%
%% @spec(Msg::binary, Count::integer,
%%       DevAddr::integer, AppSKey/binary) -> binary.
%% @end
%%--------------------------------------------------------------------
encrypt_up(Msg, DevAddr, Fcnt, AppSKey) ->
    Count = (byte_size(Msg) + 15) div 16,
    S = encoding_up(Count, 1, DevAddr, Fcnt, AppSKey, <<>>),
    xor_bin(Msg, S).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create xor template for encryption of push message.
%%
%% @spec(Count::integer, Pkg::integer, DevAddr::integer,
%%       Fcnt::integer, AppSKey/binary, Acc/binary) -> binary.
%% @end
%%--------------------------------------------------------------------
encoding_up(0, _Pkg, _DevAddr, _Fcnt, _AppSKey, <<Acc/binary>>) ->
    Acc;
encoding_up(Count, Pkg, DevAddr, Fcnt, AppSKey, <<Acc/binary>>) ->
    A = <<1, 0, 0, 0, 0, 0, DevAddr:32/little-unsigned-integer,
          Fcnt:32/little-unsigned-integer, 0, Pkg>>,
    S = crypto:block_encrypt(aes_ecb, AppSKey, A),
    encoding_up(Count - 1, Pkg + 1, DevAddr, Fcnt, AppSKey,
                <<Acc/binary, S/binary>>).
