
release:
	rebar3 release

build:
	rebar3 compile

run:	release
	erl -noshell -s lge_app -sname lge -config local.config \
	    -pa _build/default/rel/lorawan-gateway-emulator/lib/*/ebin/

shell:	release
	rebar3 shell

clean:
	rm -rf _build erl_crash.dump rebar_lock

.PHONY: doc
doc:
	rebar3 edoc
