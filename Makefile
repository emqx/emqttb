.PHONY: all
all:
	rebar3 do compile, dialyzer, escriptize, eunit, ct
