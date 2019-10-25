%%
%%
%% @author toxish666 [https://github.com/toxish666]
%% @doc This module contains functions simulating JSON routines 
%% @reference See <a href="https://github.com/bitgorbovsky/erlang-course-tasks/blob/master/tasks/1-basic.md#18-%D1%80%D0%B0%D0%B1%D0%BE%D1%82%D0%B0-%D1%81-json-%D0%BE%D0%B1%D1%8A%D0%B5%D0%BA%D1%82%D0%B0%D0%BC%D0%B8"> task itself </a> for more info.
%%
%%

-module(mjson).

-export([new/1,
	 read/2,
	 write/3]).

-include_lib("eunit/include/eunit.hrl").

-type key() :: string().

-type keyspec() :: string().

-type basicvalue() :: string() | boolean() | integer() | float().

-type valuespec() :: basicvalue() | [basicvalue()] | {key(), valuespec()} | [{key(), valuespec()}].

-type json() :: map() | [map()].


-spec new(ValueSpec) -> JsonObj | {error, bad_arg} when
      ValueSpec :: valuespec(),
      JsonObj :: json().
%% @doc creates new JSON with map() from {Key,Value} spec
new(L)  -> 
    case is_value_spec(L) of
	true -> 
	    new(L, #{});
	false ->
	    {error, bad_arg}
    end.

new([], M) when map_size(M) == 0 ->
    [];
new([], M) ->
    [M];
%% {"1", [{"2", ...}, ...}]}
new([{K, V}| T], M) ->
    Map = #{K => new(V, M)},
    [Map] ++ new(T, M);
%% {"1", [1, ...]}
new([V | T], M) ->
    [V] ++ new(T, M);
%% {"1", {"2", ...}}
new({K, V}, M) ->
    #{K => new(V, M)};
%% else ({"1", 2})
new(V, M) ->
    V.


-spec read(KeySpec, JsonObj) -> {ok, ValueSpec} | {error, not_found} | {error, bad_arg} when
      KeySpec :: keyspec(),
      JsonObj :: json(),
      ValueSpec :: valuespec().
%% @doc returns value from JSON object given key
read(K, [J|JT]) when is_map(J) ->
    case {is_key_spec(K), J} of
	{true, #{K := V}} -> 
	    {ok, V};
	{true, _} -> 
	    case read_inner(K, J) of
		none ->
		    read(K, JT);
		V ->
		    {ok,V}
	    end;
	_ ->
	    {error, bad_arg}
    end;
read(_, []) ->
    {error, not_found}.

read_inner(K, J) when is_map(J) ->
    I = maps:iterator(J),
    read_inner(K, I);
read_inner(K, I) ->
    case maps:next(I) of
	{K, VM, _} ->
	    VM;
	{_, VM, _} when is_map(VM) ->
	    read_inner(K, VM);
	{_, VM, IN} when is_list(VM) ->
	    case read_inner_list(K, VM) of
		none ->
		    read_inner(K, IN);
		V ->
		    V
	    end;
	{_, _, IN} ->
	    read_inner(K, IN);
	none ->
	    none
    end.

read_inner_list(_, []) ->
    none;
read_inner_list(K, [H|T]) when is_map(H) ->
    case read_inner(K, H) of
	none ->
	    read_inner_list(K,T);
	V ->
	    V
    end;
read_inner_list(K, [_|T]) ->
    read_inner_list(K, T).


-spec write(KeySpec, ValueSpec, JsonObj) -> JsonObj | {error, not_found} | {error, bad_arg} when
      KeySpec :: keyspec(),
      JsonObj :: json(),
      ValueSpec :: valuespec().
%% @doc write(update) new value given key into JSON object
write(K, V, [J|JT]) when is_map(J) ->
    case {is_key_spec(K) andalso is_value_spec(V), J} of
	{true, #{K := _}} -> 
	    [J#{K := V}|JT];
	{true, _} ->
	    case write_inner(K, V, J) of 
		none ->
		    write(K, V, JT);
		V ->
		    [V|JT]
	    end;
	_ -> 
	    {error, bad_arg}
    end;
write(_, _, []) ->
    {error, not_found}.

write_inner(K, V, J) when is_map(J) ->
    ff.



%% ----------------------------------------------------------------
%% specs verification functions
%% ----------------------------------------------------------------

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
is_value_spec([]) -> 
    true;
is_value_spec([H|L]) -> 
    (is_basic_value(H) orelse is_key_value_spec_pair(H)) 
	andalso is_value_spec(L);
is_value_spec({K,V}) ->
    is_key(K) andalso is_value_spec(V);
is_value_spec(V) ->
    is_basic_value(V).


%% value spec with top guard that won't allow 
%% [1,2,3,...] expressions on top of new



%% ----------------------------------------------------------------
%% test suite
%% ----------------------------------------------------------------

create_with_new_test() ->
    M = [#{"1" => [888,#{"2" => 999},#{"2" => [1000, #{"3" => true}]},111]}],
    ?_assertEqual(new([
		       {"1",
			[888,
			 {"2",
			  999
			 },
			 {"2",
			  [1000,
			   {"3",
			    true
			   }
			  ]
			 },
			 111
			]
		       }
		      ]), M).

%% given [{"1", [888, {"2", 999}, {"2", [1000, {"3", true}]}, 111]}] output is
%% [#{"1" => [888,#{"2" => 999},#{"2" => [1000, #{"3" => true}]},111]}]
fff() ->
    new([
	 {"1",
	  [888,
	   {"2",
	    999
	   },
	   {"2",
	    [1000,
	     {"3",
	      true
	     }
	    ]
	   },
	   111
	  ]
	 }
	]).


%% given [{"1",{"2",[{"3",true},4]}}] output is
%% [#{"1" => #{"2" => [#{"3" => true}, 4]}}].
fff2() ->
    new([
	 {"1",
	  {"2",
	   [
	    {"3",
	     true
	    },
	    4
	   ]
	  }
	 }
	]).


%% given [[[{"1", 2}]]] output is 
%% {error, bad_arg}
fff3() ->
    new([
	 [
	  [
	   {"1",
	    2
	   }
	  ]
	 ]
	]).


%% given {"1", {"5",[1,2,3]}} output is
%% #{"1" => #{"5" => [1,2,3]}}
fff4() ->
    new({
	 "1",
	 {
	  "5",
	  [
	   1, 2, 3
	  ]
	 }
	}).

%% given 
