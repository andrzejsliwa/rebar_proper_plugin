REBAR = `which rebar || echo ./rebar`

# Tells to make that deps is not a file/directory
.PHONY: doc deps

# default task
all: dist_clean deps compile

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
