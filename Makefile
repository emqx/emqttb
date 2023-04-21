REBAR ?= $(CURDIR)/rebar3
REBAR_URL ?= https://s3.amazonaws.com/rebar3/rebar3

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

$(REBAR):
	@curl -skfL "$(REBAR_URL)" -o $@
	@chmod +x $@
