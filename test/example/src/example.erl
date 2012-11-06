-module(example).
-export([is_empty/1, size/1, new/0, push/2, pop/1, safe_pop/1, prop_push_pop/0]).
-export_type([stack/1]).

-opaque stack(T) :: {non_neg_integer(),[T]}.

%% NOTE: You don't need to include the proper header if no properties are
%%	 declared in the module.
-include_lib("proper/include/proper.hrl").

%% NOTE: Every instance of the ADT in a spec must have variables as parameters.
%%	 When this would mean singleton variables, use variables starting with
%%	 an underscore.
-spec is_empty(stack(_T)) -> boolean().
is_empty({0, []}) ->
    true;
is_empty({_N, [_Top|_Rest]}) ->
    false.

-spec size(stack(_T)) -> non_neg_integer().
size({N, _Elems}) ->
    N.

-spec new() -> stack(_T).
new() ->
    {0, []}.

-spec push(T, stack(T)) -> stack(T).
push(X, {N,Elems}) ->
    {N+1, [X|Elems]}.

-spec pop(stack(T)) -> {T,stack(T)}.
pop({0, []}) ->
    throw(stack_empty);
pop({N, [Top|Rest]}) when N > 0 ->
    {Top, {N-1,Rest}}.

-spec safe_pop(stack(T)) -> {'ok',T,stack(T)} | 'error'.
safe_pop({0, []}) ->
    error;
safe_pop({N, [Top|Rest]}) when N > 0 ->
    {ok, Top, {N-1,Rest}}.

%%------------------------------------------------------------------------------
%% Properties
%%------------------------------------------------------------------------------

prop_push_pop() ->
    ?FORALL({X,S}, {integer(),stack(integer())},
	    begin
		{Y,_} = pop(push(X,S)),
		X =:= Y
	    end).