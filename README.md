in "~/.erlang":

    code:add_pathz("/Users/andrzejsliwa/.erlang_tools/deps/proper/ebin").

in "rebar.config":
    {deps, [
    	{rebar_proper_plugin, ".*", {git, "...", "master"}}
    ]}.

    {plugins, [rebar_proper_plugin]}.

    {proper_opts, [{numtests, 200}]}.
    {proper_check_spec, [{example, is_empty, 1}]}.
    %% or {proper_check_spec, [example]}.

in command line:

    rebar proper