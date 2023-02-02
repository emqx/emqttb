REBAR ?= rebar3

.PHONY: all
all:
	$(REBAR) do compile, dialyzer, eunit, ct

.PHONY: compile
compile:
	$(REBAR) compile

.PHONY: dialyzer
dialyzer:
	$(REBAR) do compile, dialyzer

.PHONY: test
test:
	$(REBAR) do eunit, ct

.PHONY: release
release:
	$(REBAR) do compile, tar

.PHONY: README.md
README.md: compile
	./emqttb @make-docs --src "$$(pwd)/doc/src/conf.xml"
	pandoc -o "$@" --to gfm-gfm_auto_identifiers --from docbook 'docs/EMQTT bench daemon.xml'

.PHONY: clean
clean: distclean

.PHONY: distclean
distclean:
	@rm -rf _build erl_crash.dump rebar3.crashdump rebar.lock emqttb

