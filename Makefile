REBAR ?= $(CURDIR)/rebar3
REBAR_URL ?= https://s3.amazonaws.com/rebar3/rebar3
TEXINFO := doc/src/emqttb.texi doc/lee/cli_params.texi doc/lee/os_env.texi doc/lee/value.texi

define MATHJAX_OPTS
loader: {\
    load: ['[tex]/physics'],\
    versionWarnings: false\
},\
tex: {\
  packages: {'[+]': ['physics']}\
}
endef

.PHONY: all
all: $(REBAR)
	$(REBAR) do compile, dialyzer, xref, eunit, ct

.PHONY: compile
compile: $(REBAR)
	$(REBAR) compile

.PHONY: dialyzer
dialyzer: $(REBAR)
	$(REBAR) do compile, dialyzer

.PHONY: test
test: $(REBAR)
	$(REBAR) do compile, eunit, ct

.PHONY: release
release: compile docs
	@$(REBAR) as emqttb tar
	@$(CURDIR)/scripts/rename-package.sh

.PHONY: docs
docs: doc/info/emqttb.info doc/html/index.html

doc/info/emqttb.info: $(TEXINFO)
	texi2any -I doc/lee --info -o $@ $<

doc/html/index.html: $(TEXINFO)
# -c MATHJAX_CONFIGURATION="$(MATHJAX_OPTS)"
	texi2any -I doc/lee --html -c INFO_JS_DIR=js -c HTML_MATH=mathjax -o doc/html/ $<

$(TEXINFO): scripts/docgen.escript compile
	scripts/docgen.escript doc/lee

.PHONY: clean
clean: distclean

.PHONY: distclean
distclean:
	@rm -rf _build erl_crash.dump rebar3.crashdump rebar.lock emqttb doc/lee doc/html doc/info

$(REBAR):
	@curl -skfL "$(REBAR_URL)" -o $@
	@chmod +x $@
