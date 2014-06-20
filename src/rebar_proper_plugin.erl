%% See LICENSE for licensing information.
-module(rebar_proper_plugin).
-export([proper/2]).

-define(PROPER_DIR, ".proper").

%% +-----------------------------------------------------------------+
%% | INTERFACE FUNCTIONS                                             |
%% +-----------------------------------------------------------------+
proper(Config, _AppFile) ->
    run_proper(Config, proper_opts(Config), proper_check_spec(Config)).

%% +-----------------------------------------------------------------+
%% | LOGGING MACROS                                                  |
%% +-----------------------------------------------------------------+

-define(DEBUG(Str, Args), rebar_log:log(debug, Str, Args)).
-define(CONSOLE(Str, Args), io:format(Str, Args)).
-define(ABORT(Str, Args), rebar_utils:abort(Str, Args)).

%% +-----------------------------------------------------------------+
%% | PRIVATE FUNCTIONS                                               |
%% +-----------------------------------------------------------------+
run_proper(Config, PropOpts, CheckSpecs) ->
    ?DEBUG("proper_opts: ~p~n", [PropOpts]),
    ok = ensure_dirs(),
    CodePath = setup_codepath(),
    CompileOnly = rebar_utils:get_experimental_global(Config, compile_only, false),
    {ok, SrcErls} = rebar_erlc_compiler:test_compile(Config, "proper", ?PROPER_DIR),

    case CompileOnly of
        "true" ->
            true = code:set_path(CodePath),
            ?CONSOLE("Compiled modules for PropEr~n", []);
        false ->
            PropsOutput = run_props(PropOpts),
            CheckSpecOutput = run_checkspec(CheckSpecs, PropOpts, SrcErls),
            handle_proper_output(PropsOutput, CheckSpecOutput, CodePath)
    end.

handle_proper_output([], [], CodePath) ->
    true = code:set_path(CodePath),
    ok;
handle_proper_output(PropsOutput, CheckSpecOutput, _) ->
    Output = lists:append(PropsOutput, CheckSpecOutput),
    ?ABORT("One or more PropEr properties didn't hold true:~n~p~n",
           [Output]).

ensure_dirs() ->
    ok = filelib:ensure_dir(filename:join(proper_fullpath_dir(), "dummy")),
    ok = filelib:ensure_dir(filename:join(rebar_utils:ebin_dir(), "dummy")).

setup_codepath() ->
    CodePath = code:get_path(),
    true = code:add_patha(proper_fullpath_dir()),
    true = code:add_pathz(rebar_utils:ebin_dir()),
    CodePath.

proper_fullpath_dir() ->
    filename:join(rebar_utils:get_cwd(), ?PROPER_DIR).

run_props(PropOpts) ->
    TestModule = fun(M) -> proper:module(M, PropOpts) end,
    lists:flatmap(TestModule, find_prop_mods()).

run_checkspec(CheckSpecs, PropOpts, SrcErls) ->
    lists:flatmap(fun(CheckSpec) ->
                          Module = extract_module_name(CheckSpec),
                          case source_of_module_exist(Module, SrcErls) of
                              true  -> check_spec(CheckSpec, PropOpts);
                              false -> []
                          end
                  end, CheckSpecs).

%% check if defined module existing in sources of processed project
source_of_module_exist(Module, SrcErls) ->
    lists:any(fun(X) ->
                      SourceName = atom_to_list(Module) ++ ".erl",
                      SourcePath =  filename:join(proper_fullpath_dir(), SourceName),
                      lists:suffix(SourcePath, X)
              end, SrcErls).

extract_module_name({Module, _, _}) -> Module;
extract_module_name(Module) -> Module.

check_spec(CheckSpec={Module, Fun, Arity}, PropOpts) ->
    ?CONSOLE("Testing ~s:~s/~p~n", [atom_to_list(Module), atom_to_list(Fun), Arity]),
    case proper:check_spec(CheckSpec, PropOpts) of
        true  -> [];
        Error -> [{CheckSpec, Error}]
    end;
check_spec(Module, PropOpts) ->
    Results = proper:check_specs(Module, PropOpts),
    io:format("~p~n", [Results]),
    Results.

find_prop_mods() ->
    Beams = rebar_utils:find_files(?PROPER_DIR, ".*\\.beam\$"),
    [M || M <- [rebar_utils:erl_to_mod(Beam) || Beam <- Beams], has_prop(M)].

has_prop(Mod) ->
    lists:any(fun({F,_A}) -> lists:prefix("prop_", atom_to_list(F)) end,
              Mod:module_info(exports)).

proper_check_spec(Config) ->
    rebar_config:get(Config, proper_check_spec, []).
proper_opts(Config) ->
    rebar_config:get(Config, proper_opts, []).
