.PHONY: all
all:
	rebar3 do compile, dialyzer, escriptize, eunit, ct

README.md: docs/EMQTT\ bench\ daemon.xml
	pandoc -o "$@" --to gfm --from docbook "$<"
