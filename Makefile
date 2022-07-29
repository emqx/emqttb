.PHONY: all dialyzer compile
all:
	rebar3 do compile, dialyzer, escriptize, eunit, ct

dialyzer:
	rebar3 do dialyzer

compile:
	rebar3 do compile

.PHONY: README.md
README.md:
	rebar3 escriptize
	./emqttb @make-docs --src "$$(pwd)/doc/src/conf.xml"
	pandoc -o "$@" --to gfm-gfm_auto_identifiers --from docbook 'docs/EMQTT bench daemon.xml'
