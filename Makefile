REBAR ?= $(CURDIR)/rebar3
REBAR_URL ?= https://s3.amazonaws.com/rebar3/rebar3
XSLTNG := _build/lee_doc/docbook-xslTNG-2.1.2/libs/docbook-xslTNG-2.1.2.jar
DOCBOOK := _build/lee_doc/src/output.xml
MANPAGE_STYLESHEET ?= /usr/share/xml/docbook/stylesheet/docbook-xsl/manpages/docbook.xsl

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

_build/lee_doc/src/output.xml: release
	mkdir -p "$$(dirname $@)"
	./emqttb @make-docs -o "$@"

.PHONY: docs
docs: _build/lee_doc/man/emqttb.1 _build/lee_doc/html/index.html

_build/lee_doc/man/emqttb.1: $(DOCBOOK)
	xsltproc -o "$$(dirname $<)/../man/" $(MANPAGE_STYLESHEET) "$<"

_build/lee_doc/html/index.html: $(DOCBOOK) $(XSLTNG)
	mkdir -p "$$(dirname $@)"
	cd _build/lee_doc/html ;\
	java -jar $(CURDIR)/$(XSLTNG) resource-base-uri='./' chunk-output-base-uri='./' \
                                verbatim-syntax-highlight-languages='bash erlang' \
                                chunk=index.html persistent-toc=true chunk-nav=true $(CURDIR)/$<
	cp -R _build/lee_doc/docbook-xslTNG-2.1.2/resources/* _build/lee_doc/html/

$(XSLTNG):
	cd _build/lee_doc/ && \
	wget https://github.com/docbook/xslTNG/releases/download/2.1.2/docbook-xslTNG-2.1.2.zip && \
	unzip docbook-xslTNG-2.1.2.zip

.PHONY: clean
clean: distclean

.PHONY: distclean
distclean:
	@rm -rf _build erl_crash.dump rebar3.crashdump rebar.lock emqttb

$(REBAR):
	@curl -skfL "$(REBAR_URL)" -o $@
	@chmod +x $@
