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

## Configuration

Copy *local.config.template* to *local.config*. Edit *local.config*.

* mac - Gateway EUI-64
* devadaddr - 32 bit device address
* appskey - application session key used to encrypt messages
* netwkskey - network session key used to sign and encrypt messages
* ip - IP address of the LoRaWAN server
* port - UDP port of the LoRaWAN server

## Running the emulator

Run the emulator with the following command

    make run

Type *stop* to exit the program.

## License

This program is licensed according to the MIT license. See the accompanying
file LICENSE.

## Links

* [1](https://github.com/Lora-net/packet_forwarder/blob/master/PROTOCOL.TXT)
     https://github.com/Lora-net/packet_forwarder/blob/master/PROTOCOL.TXT \-
     Basic communication protocol between Lora gateway and server
