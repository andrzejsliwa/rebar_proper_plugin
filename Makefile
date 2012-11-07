REBAR = ./rebar

# Tells to make that deps is not a file/directory
.PHONY: doc deps

# default task
all: deps compile

dev: dev_compile

# generate docs - dev config
doc: dev
	@$(REBAR) -C rebar.dev.config doc

# Compiles erlang sources
compile:
	@$(REBAR) compile

# Cleans all files
clean:
	@$(REBAR) clean

# Pulls all dependencies
deps:
	@$(REBAR) get-deps

# Removes whole dependencies
dist_clean:
	@$(REBAR) delete-deps

# Runs eunit tests
eunit:
	@$(REBAR) $(REBAR_SKIP_DEPS) eunit skip_deps=true

# Runs all tests clean
test: clean all eunit

# Cleans all files - dev config
dev_clean:
	@$(REBAR) -C rebar.dev.config clean

# Compiles erlang sources - dev config
dev_compile: dev_deps
	@$(REBAR) -C rebar.dev.config compile

# Pulls all dependencies - dev config
dev_deps:
	@$(REBAR) -C rebar.dev.config get-deps
