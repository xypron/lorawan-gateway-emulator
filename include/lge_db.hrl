% SPDX-License-Identifier: MIT
%
% Copyright (c) 2018 Heinrich Schuchardt <xypron.glpk@gmx.de>
%

%%--------------------------------------------------------------------
%% @type
%% eui64() = <<_:64>>.
%% 64-bit global identifier (EUI-64).
%%
%% 64-bit global identifiers are definded in
%%
%% <a href="https://standards.ieee.org/content/dam/ieee-standards/standards/web/documents/tutorials/eui.pdf">
%% IEEE, "Guidelines for 64-bit Global Identifier (EUI-64) Registration
%% Authority"</a>
%% https://standards.ieee.org/content/dam/ieee-standards/standards/web/documents/tutorials/eui.pdf
%% @end
%%--------------------------------------------------------------------
-type eui64() :: <<_:64>>.

%%--------------------------------------------------------------------
%% @type
%% devaddr() = <<_:32>>.
%% Device address.
%%
%% Every connected device is identified by a unique device address.
%% @end
%%--------------------------------------------------------------------
-type devaddr() :: <<_:32>>.

%%--------------------------------------------------------------------
%% @type
%% skey() = <<_:128>>.
%% Session key.
%%
%% Session keys are used for AES128 encryptions.
%% @end
%%--------------------------------------------------------------------
-type skey() :: <<_:128>>.


%%--------------------------------------------------------------------
%% @type
%% otaa() = #otaa{deveui = eui64(),
%%                appkey = skey(),
%%                device = devaddr()
%%            }.
%% A device that can be connected via OTAA.
%% @end
%%--------------------------------------------------------------------
-record(otaa, {
        deveui :: eui64(),
        appkey :: skey(),
        device :: devaddr()
    }).

%%--------------------------------------------------------------------
%% @type
%% device() = #device{devaddr = devaddr(),
%%                    appskey = skey(),
%%                    netwkskey = skey(),
%%                    fcntup = integer(),
%%                    fcntdown = integer()
%%          }.
%% A device that can be connected via ABP.
%% @end
%%--------------------------------------------------------------------
-record(device, {
        devaddr :: devaddr(),
        appskey :: skey(),
        netwkskey :: skey(),
        fcntup :: integer(),
        fcntdown :: integer()
    }).

%%--------------------------------------------------------------------
%% @type
%% gateway() = #gateway{eui = eui64(),
%%                      name = nonempty_string(),
%%                      server = {inet:ip_address(), inet:port()}
%%             }.
%% A gateway.
%% @end
%%--------------------------------------------------------------------
-record(gateway, {
        eui :: eui64(),
        name :: nonempty_string(),
        server :: {inet:ip_address(), inet:port()}
    }).

%%--------------------------------------------------------------------
%% @type
%% server() = #server{ip_address = {inet:ip_address(), inet:port()},
%%                    name = nonempty_string()
%%            }.
%% A LoRaWAN server.
%% @end
%%--------------------------------------------------------------------
-record(server, {
        ip_address :: {inet:ip_address(), inet:port()},
        name :: nonempty_string()
    }).
