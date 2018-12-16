# LoRaWAN Gateway Emulator

This program emulates a LoRaWAN gateway. It uses the Semtech LoRa forwarder
protocol[1] to communicate with a LoRaWAN server.

## Building the emulator

The following prerequisites must be installed:

* [rebar3](https://github.com/erlang/rebar3)
* [Erlang OTP](https://www.erlang.org/), version 21 or later

Build with command

    make

Copy local.config.template to local.config and fill in the relevant data.

## Running the emulator

Run the emulator with the following command

    make run

Type *stop* to exit the program.

The emulator sends PULL\_DATA requests every two seconds.

Every few seconds it sends a PUSH\_DATA message emulating an RAK LB801N button
with button 1 pressed (hexadecimal string 53 01 01 00 00 00 00 5A).

## License

This program is licensed according to the MIT license. See the accompanying
file LICENSE.

## Links

* [1](https://github.com/Lora-net/packet_forwarder/blob/master/PROTOCOL.TXT)
     https://github.com/Lora-net/packet_forwarder/blob/master/PROTOCOL.TXT \-
     Basic communication protocol between Lora gateway and server
