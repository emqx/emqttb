.PHONY: all
all:
	rebar3 do compile, dialyzer, escriptize, eunit, ct

.PHONY: README.md
README.md:
	./emqttb @make-docs
	pandoc -o "$@" --to gfm-gfm_auto_identifiers --from docbook 'docs/EMQTT bench daemon.xml'
