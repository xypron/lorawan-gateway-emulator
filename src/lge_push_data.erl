% SPDX-License-Identifier: MIT
%%%-------------------------------------------------------------------
%%% @author Heinrich Schuchardt <xypron.glpk@gmx.de>
%%% @copyright (C) 2018, Heinrich Schuchardt <xypron.glpk@gmx.de>
%%% @doc
%%% This server triggers the PULL_DATA messages to the Gateway Server.
%%%
%%% @end
%%% Created : 2018-11-25 13:05:01.521613
%%%-------------------------------------------------------------------
-module(lge_push_data).

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
-define(INTERVAl, 5731).

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
    Timer = erlang:send_after(?INTERVAl, self(), keepalive),
    State = #{
        timer => Timer,
        mac => lge_util:get_mac(),
        token => lge_util:rand16()
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
handle_cast({resp, _Msg}, State) ->
    % << _:8, Token:16, _/binary>> = Msg,
    % send TX_ACK
    % gen_server:cast(lge_udp, {send, <<2, Token:16, 5,
    %                           "{\"txpk_ack\":{\"error\":\"NONE\"}}" >>}),
    % schedule PUSH_DATA(stat)
    Dwnb = maps:get(dwnb, State, 0),
    erlang:send_after(2500, self(), stat),
    {noreply, State#{dwnb => Dwnb + 1}};
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
handle_info(stat, State) ->
    {noreply, do_stat(State)};
handle_info(keepalive, OldState) ->
    OldTimer = maps:get(timer, OldState),
    erlang:cancel_timer(OldTimer),
    State = do_push(OldState),
    Timer = erlang:send_after(?INTERVAl, self(), keepalive),
    {noreply, State#{timer => Timer}};
handle_info({udp, _Socket, _Ip, _Port, _Msg}, State) ->
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

time_stamp(T) ->
    calendar:system_time_to_rfc3339(T, [{unit,nanosecond},{offset,"Z"}]).

time_stamp32(T) ->
    (T div 1000000) rem 4294967296.

payload(Fcnt) ->
    {ok, DevAddr} = application:get_env(lge, devaddr),
    {ok, AppSKey} = application:get_env(lge, appskey),
    {ok, NetwkSKey} = application:get_env(lge, netwkskey),
    A1 = << 1, 0, 0, 0, 0, 0, DevAddr:32/little-unsigned-integer, Fcnt:32/little-unsigned-integer, 0, 1 >>,
    S = crypto:block_encrypt(aes_ecb, AppSKey, A1),
    << S0, S1, S2, S3, S4, S5, S6, S7, _/binary>> = S,
    P = << (16#53 bxor S0), (16#01 bxor S1), (1 bxor S2), (0 bxor S3),
           (0 bxor S4), (0 bxor S5), (16#00 bxor S6), (16#5A bxor S7) >>,
    Q = << 128, DevAddr:32/little-unsigned-integer, 128, Fcnt:16/little-unsigned-integer, 8, P/binary >>,
    B0 = << 16#49, 0, 0, 0, 0, 0, DevAddr:32/little-unsigned-integer, Fcnt:32/little-unsigned-integer, 0, (byte_size(Q)) >>,
    MIC = erlang:binary_part(crypto:cmac(aes_cbc128, NetwkSKey, << B0 /binary, Q/binary>>), 0, 4),
    base64:encode_to_string(<<Q/binary, MIC/binary>>).

get_json(Fcnt) ->
    T = erlang:system_time(),
    jsx:encode(
        #{
            rxpk => [ #{
                time => list_to_atom(time_stamp(T)),
                tmst => time_stamp32(T),
                chan => 0,
                rfch => 0,
                freq => 868.1,
                stat => 1,
                modu => list_to_atom("LORA"),
                datr => list_to_atom("SF7BW125"),
                codr => list_to_atom("4/5"),
                rssi => -35,
                lsnr => 5.1,
                size => 21,
                data => list_to_atom(payload(Fcnt))
            } ]
        }
    ).

do_stat(State) ->
    Mac = lge_util:get_mac(),
    Token = lge_util:rand16(),
    T = erlang:system_time(),
    Ts=lists:flatten(
            string:replace(
                calendar:system_time_to_rfc3339(
                    T div 1000000000,
                    [{unit,second},{offset,"Z"},{time_designator,$\s}]),
                "Z", " GMT")),
    Dwnb = maps:get(dwnb, State, 0),
    Rxfw = maps:get(rxfw, State, 0),
    Json = jsx:encode(
        #{
            stat => #{
                time => list_to_atom(Ts),
                lati => 51,
                long => 7,
                alti => 40,
                rxnb => Rxfw,
                rxok => Rxfw,
                rxfw => Rxfw,
                ackr => 100.0,
                dwnb => Dwnb,
                txnb => Dwnb
            }
        }
    ),
    Msg = << 2, Token:16 , 0, Mac/binary, Json/binary >>,
    gen_server:cast(lge_udp, {send, Msg}),
    State#{token => Token, dwnb => 0, rxfw => 0}.

do_push(State) ->
    Mac = lge_util:get_mac(),
    Token = lge_util:rand16(),
    Fcnt = maps:get(fcnt, State, 0),
    Json = get_json(Fcnt),
    Msg = << 2, Token:16 , 0, Mac/binary, Json/binary >>,
    Rxfw = maps:get(rxfw, State, 0) + 1,
    gen_server:cast(lge_udp, {send, Msg}),
    State#{token => Token, rxfw => Rxfw, fcnt => Fcnt + 1}.


