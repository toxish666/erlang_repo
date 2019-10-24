%%
%% @author toxish666 [https://github.com/toxish666]
%% @doc This module contains functions simulating JSON routines 
%% @reference See <a href="https://github.com/bitgorbovsky/erlang-course-tasks/blob/master/tasks/1-basic.md#18-%D1%80%D0%B0%D0%B1%D0%BE%D1%82%D0%B0-%D1%81-json-%D0%BE%D0%B1%D1%8A%D0%B5%D0%BA%D1%82%D0%B0%D0%BC%D0%B8"> Task itself </a> for more info.
%%

-module(mjson).

-export([new/1,
	read/2,
	write/3]).

-type key() :: string().

-type keyspec() :: string().

-type basicvalue() :: string() | boolean() | integer() | float().

-type valuespec() :: basicvalue() | [basicvalue()] | {key(), valuespec()} | [{key(), valuespec()}].


      
-spec new(ValueSpec) -> JsonObj | {error, bad_arg} when
      ValueSpec :: valuespec(),
      JsonObj :: map().
%% @doc creates new JSON with map() from {Key,Value} spec
new(L) when is_list(L) -> 
    case is_value_spec_keyvaluelist(L) of
	true -> new(L, maps:new());
	false -> {error, bad_arg}
    end.

new([], M) ->
    M;
new([{K, V}| T], M) ->
    new(T, maps:put(K, V, M));
new(_, _) ->
    {error, bad_arg}.


-spec read(KeySpec, JsonObj) -> {ok, ValueSpec} | {error, not_found} when
      KeySpec :: keyspec(),
      JsonObj :: map(),
      ValueSpec :: valuespec().
%% @doc returns value from JSON object given key
read(K, J) when is_map(J) ->
    case {is_key_spec(K), J} of
	    {true, #{K := Val}} -> {ok, Val};
	    _ -> {error, not_found}
    end.


-spec write(KeySpec, ValueSpec, JsonObj) -> JsonObj | {error, not_found} when
      KeySpec :: keyspec(),
      JsonObj :: map(),
      ValueSpec :: valuespec().
%% @doc write new value given key into JSON object
write(K, V, J) when is_map(J) ->
    case {is_key_spec(K) andalso is_value_spec(V), J} of
	{true, #{K := _}} -> J#{K := V};
	_ -> {error, not_found}
    end.
	    


%% -----------------------------------------

is_print(X) when X >= 32, X < 127 -> 
    true;
is_print(_) -> 
    false.

is_string(L) when is_list(L) -> 
    lists:all(fun is_print/1, L);
is_string(_) -> 
    false.


is_key(V) -> 
    is_string(V).

is_key_spec(V) -> 
    is_string(V).

%% BasicValue = string() | boolean() | integer() | float()
is_basic_value(V) ->
    is_string(V)
	orelse is_boolean(V)
	orelse is_integer(V)
	orelse is_float(V).

%% added KeyValue = {Key, ValueSpec}
is_key_value_spec_pair({K, VS}) ->
    is_key(K) andalso is_value_spec(VS);
is_key_value_spec_pair(_) ->
    false.

%% ValueSpec = BasicValue | [BasicValue] | {Key, ValueSpec} | [{Key, ValueSpec}]
 %% [BasicValue] | [{Key, ValueSpec}] part
is_value_spec([H|L]) ->
    case is_basic_value(H) of
	true -> 
	    is_value_spec_basicvaluelist(L);
	false -> case is_key_value_spec_pair(H) of
		     true ->
			 is_value_spec_keyvaluelist(L);
		     false ->
			 false
		 end
    end;
is_value_spec(V) -> 
    is_basic_value(V) orelse is_key_value_spec_pair(V).

is_value_spec_basicvaluelist([]) -> 
    true;
is_value_spec_basicvaluelist([H|L]) ->
    is_basic_value(H) andalso is_value_spec_basicvaluelist(L).

is_value_spec_keyvaluelist([]) ->
    true;
is_value_spec_keyvaluelist([H|L]) ->
    is_key_value_spec_pair(H) andalso is_value_spec_keyvaluelist(L).



