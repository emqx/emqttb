REBAR ?= $(CURDIR)/rebar3
REBAR_VERSION ?= 3.19.0-emqx-1

.PHONY: all
all: $(REBAR)
	$(REBAR) do compile, dialyzer, eunit, ct

.PHONY: compile
compile: $(REBAR)
	$(REBAR) compile

.PHONY: dialyzer
dialyzer: $(REBAR)
	$(REBAR) do compile, dialyzer

.PHONY: test
test: $(REBAR)
	$(REBAR) do eunit, ct

.PHONY: release
release: compile
	@$(REBAR) as emqttb tar
	@$(CURDIR)/scripts/rename-package.sh

.PHONY: README.md
README.md: compile
	./emqttb @make-docs --src "$$(pwd)/doc/src/conf.xml"
	pandoc -o "$@" --to gfm-gfm_auto_identifiers --from docbook 'docs/EMQTT bench daemon.xml'

.PHONY: clean
clean: distclean

.PHONY: distclean
distclean:
	@rm -rf _build erl_crash.dump rebar3.crashdump rebar.lock emqttb

.PHONY: ensure-rebar3
ensure-rebar3:
	$(CURDIR)/scripts/ensure-rebar3.sh $(REBAR_VERSION)

$(REBAR): ensure-rebar3
