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

-export([mic_up/4]).

-type session_key() :: [<< _:128 >>].
-type message_integrity_code() :: [<< _:32 >>].

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
    B0 = << 16#49, 0, 0, 0, 0, 0, DevAddr:32/little-unsigned-integer,
         Fcnt:32/little-unsigned-integer, 0, (byte_size(Msg)) >>,
    erlang:binary_part(crypto:cmac(aes_cbc128, NetwkSKey,
                       << B0 /binary, Msg/binary>>), 0, 4).

