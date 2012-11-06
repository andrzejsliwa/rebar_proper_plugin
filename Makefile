REBAR?=./rebar

all: build

dev: devbuild

doc: dev
	$(REBAR) -C rebar.dev.config doc

clean:
	$(REBAR) clean

distclean: clean
	@rm -rf deps

build: deps
	$(REBAR) compile

deps:
	$(REBAR) get-deps


# development
#
devclean:
	$(REBAR) -C rebar.dev.config clean

devbuild: devdeps
	$(REBAR) -C rebar.dev.config compile

devdeps:
	$(REBAR) -C rebar.dev.config get-deps


.PHONY: doc deps
