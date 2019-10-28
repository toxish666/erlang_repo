%%
%%
%% @author toxish666 [https://github.com/toxish666]
%% @doc This module contains functions simulating JSON routines 
%% @reference See <a href="https://github.com/bitgorbovsky/erlang-course-tasks/blob/master/tasks/1-basic.md#18-%D1%80%D0%B0%D0%B1%D0%BE%D1%82%D0%B0-%D1%81-json-%D0%BE%D0%B1%D1%8A%D0%B5%D0%BA%D1%82%D0%B0%D0%BC%D0%B8"> task itself </a> for more info.
%%
%%
%% @TODO anonymous objects can be added: {{"key",value1}, {"key2",value2}}
%% and then maps may contain several keys (as of now only 1: #{"key", value}
%% valuespec should include newtype {keyvaluespec()} where keyvaluespec() is {key(), valuespec()}

-module(mjson).

-export([new/1,
	 read/2,
	 write/3]).

-export_type([json/0]).

-type key() :: string().

-type keyspec() :: string().

-type basicvalue() :: string() | boolean() | integer() | float().

-type valuespec() :: basicvalue() | [basicvalue()] | {key(), valuespec()} | [{key(), valuespec()}].

-opaque json() :: map() | [map()].


-spec new(ValueSpec) -> JsonObj | {error, bad_arg} when
      ValueSpec :: valuespec(),
      JsonObj :: json().
%% @doc creates new JSON with map() from {Key,Value} spec
new(L)  -> 
    case is_json_spec(L) of
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
new(V, _) ->
    V.


-spec read(KeySpec, JsonObj) -> {ok, ValueSpec} | {error, not_found} | {error, bad_arg} when
      KeySpec :: keyspec(),
      JsonObj :: json(),
      ValueSpec :: valuespec().
%% @doc returns value from JSON object given key
read(K, J) when is_map(J) ->
    case {is_key_spec(K), J} of
	{true, #{K := V}} -> 
	    {ok, V};
	{true, _} -> 
	    case read_inner(K, J) of
		none ->
		    {error, not_found};
		V ->
		    {ok,V}
	    end;
	_ ->
	    {error, bad_arg}
    end;
read(K, [J|JT]) when is_map(J) ->
    case read(K, J) of
	{ok, _} = Done ->
	    Done;
	{error, not_found} ->
	    read(K, JT);
	{error, bad_arg} = Bad ->
	    Bad;
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


-spec write(KeySpec, ValueSpec, JsonObj) -> {JsonObj, NumberOfChanges} | {error, bad_arg} when
      KeySpec :: keyspec(),
      JsonObj :: json(),
      ValueSpec :: valuespec(),
      NumberOfChanges :: integer().
%% @doc write(update) new value given key into JSON object
%% slightly modified resulting value as it returns JSON with number of updates being made
%% {error, not_found} is equal to the case when there was 0 updates in JSON object
write(K, V, J) ->
    case (is_key_spec(K) andalso is_value_spec(V)) of 
	true ->
	    case (is_list(V) orelse is_key_value_spec_pair(V)) of 
		true ->
		    write_impl(K, value_to_json(V), J, 0);
		false ->
		    write_impl(K, V, J, 0)
	    end;
	_ ->
	    {error, bad_arg}
    end.

%% Key, Value, Json, Counter (integer in order to find not_found case)	    
write_impl(K, V, J, Counter) when is_map(J) ->
    case J of
	#{K := _} -> 
	    {J#{K := V}, Counter + 1};
	_ ->
	    write_inner(K, V, J, Counter)
    end;
write_impl(K, V, [J|JT], Counter) when is_map(J) ->
    {NJ, NC} = write_impl(K, V, J, Counter),
    {WImpl, NCI} = write_impl(K, V, JT, NC),
    {[NJ] ++ WImpl, NCI};
write_impl(_, _, [], Counter) ->
    {[], Counter}.

write_inner(K, V, J, Counter) when is_map(J) ->
    I = maps:iterator(J),
    write_inner(K, V, J, I, Counter).

%% state monad in action (no)
write_inner(K, V, J, I, Counter) ->
    case maps:next(I) of
	{K, _, IM} ->
	    NJ = J#{K := V},
	    write_inner(K, V, NJ, IM, Counter + 1);
	{KM, VM, IM} when is_map(VM) ->
	    {WI, C} = write_inner(K, V, VM, Counter),
	    NJ = J#{KM := WI},
	    write_inner(K, V, NJ, IM, C);
	{KL, VL, IM} when is_list(VL) ->
	    {WIL, C} = write_inner_list(K, V, VL, [], Counter),
	    NJ = J#{KL := WIL},
	    write_inner(K, V, NJ, IM, C);
	{_, _, IM} ->
	    write_inner(K, V, J, IM, Counter);
	none ->
	    {J, Counter}
    end.

write_inner_list(_, _, [], A, Counter) ->
    {lists:reverse(A), Counter};
write_inner_list(K, V, [H|T], A, Counter) when is_map(H) ->
    case write_impl(K, V, H, Counter) of
	{[], C}->
	    write_inner_list(K, V, T, A, C);
        {WI, C} ->
	    write_inner_list(K, V, T, [WI|A], C)
    end;
write_inner_list(K, V, [H|T], A, Counter) ->
    write_inner_list(K, V, T, [H|A], Counter).


%% assuming spec is correct
value_to_json(L) when is_list(L) ->
    value_to_json(L, []);
value_to_json(KV) ->
    new(KV).

value_to_json([], A) ->
    lists:reverse(A);
value_to_json([H|T], A) ->
    case is_key_value_spec_pair(H) of
	true ->
	    value_to_json(T, [new(H)|A]);
	false ->
	    value_to_json(T, [H|A])
    end.


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
%% [1,{"2",3},4,...] expressions on top of json object
%% (1, 4 here is incorrect)
is_json_spec(KV = {_, _}) ->
    is_value_spec(KV);
is_json_spec(L = [_|_]) ->
    is_json_spec(L, []).

is_json_spec([], A) ->
    is_value_spec(lists:reverse(A));
is_json_spec([H|L], A) ->
    case is_basic_value(H) of
	false ->
	    is_json_spec(L, [H|A]);
	true ->
	    false
    end.



%% ----------------------------------------------------------------
%% test suite
%% ----------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

module_test_() -> 
    [{"test suite 1",
      {foreach, fun start1/0, [fun new1/1, fun read1/1, fun write1/1]}},
     {"test suite 2",
      {foreach, fun start2/0, [fun new2/1, fun read2/1, fun write2/1]}},
     {"test suite 3",
      {setup, fun start3/0, fun new3/1}},
     {"test suite 4",
      {foreach, fun start4/0, [fun new4/1, fun write4/1]}},
     {"test suite 5",
      {setup, fun start5/0, fun new5/1}}
    ].



start1() ->
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

%% given [{"1", [888, {"2", 999}, {"2", [1000, {"3", true}]}, 111]}] outup of new is
%% M = [#{"1" => [888,#{"2" => 999},#{"2" => [1000, #{"3" => true}]},111]}]
new1(Json) ->
    JExpected = [#{"1" => [888,#{"2" => 999},#{"2" => [1000, #{"3" => true}]},111]}],
    ?_assertEqual(Json, JExpected).

read1(Json) ->
    ReadFound1 = {ok, true},
    ReadFound2 = {ok,[888,#{"2" => 999},#{"2" => [1000,#{"3" => true}]},111]},
    ReadNotFound = {error, not_found},
    [
     ?_assertEqual(read("3", Json), ReadFound1),
     ?_assertEqual(read("1", Json), ReadFound2),
     ?_assertEqual(read("4", Json), ReadNotFound)
    ].

write1(Json) ->
    WriteResult1 = {[#{"1" => [888,#{"2" => 999},#{"2" => [1000,#{"3" => [1,2,3]}]},111]}], 1},
    WriteResult2 = {[#{"1" => [888,#{"2" => false},#{"2" => false},111]}],2},
    WriteResult3 = {[#{"1" => #{"2" => [3,#{"4" => 5}]}}],1},
    WriteResult4 = {Json,0},
    [
     ?_assertEqual(WriteResult1, write("3", [1,2,3], Json)),
     ?_assertEqual(WriteResult2, write("2", false, Json)),
     ?_assertEqual(WriteResult3, write("1", {"2", [3, {"4", 5}]} , Json)),
     ?_assertEqual(WriteResult4, write("4", false, Json))
    ].



start2() ->
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

%% given [{"1",{"2",[{"3",true},4]}}] output is
%% [#{"1" => #{"2" => [#{"3" => true}, 4]}}].
new2(Json) ->
    JExpected = [#{"1" => #{"2" => [#{"3" => true}, 4]}}],
    ?_assertEqual(Json, JExpected).

read2(Json) ->
    ReadFound1 = {ok, true},
    ReadFound2 = {ok,[#{"3" => true},4]},
    ReadNotFound = {error, not_found},
    [
     ?_assertEqual(read("3", Json), ReadFound1),
     ?_assertEqual(read("2", Json), ReadFound2),
     ?_assertEqual(read("4", Json), ReadNotFound)
    ].

write2(Json) ->
    WriteResult1 = {[#{"1" => 1}],1},
    WriteResult2 = {[#{"1" => #{"2" => 2}}],1},
    [
     ?_assertEqual(WriteResult1, write("1", 1, Json)),
     ?_assertEqual(WriteResult2, write("2", 2, Json))
    ].



start3() ->
    new([
	 [
	  [
	   {"1",
	    2
	   }
	  ]
	 ]
	]).

%% given [[[{"1", 2}]]] output is 
%% {error, bad_arg}
new3(Json) ->
    JExpected = {error, bad_arg},
    ?_assertEqual(Json, JExpected).



start4() ->
    new({
	 "1",
	 {
	  "5",
	  [
	   1, 2, 3
	  ]
	 }
	}).

%% given {"1", {"5",[1,2,3]}} output is
%% #{"1" => #{"5" => [1,2,3]}}
new4(Json) ->
    JExpected = #{"1" => #{"5" => [1,2,3]}},
    ?_assertEqual(Json, JExpected).

write4(Json) ->
    WriteResult1 = {#{"1" => #{"5" => #{"5" => #{"5" => [1,2]}}}},1},
    ?_assertEqual(write("5", {"5",{"5", [1,2]}}, Json), WriteResult1).




start5() ->
    new([
	 {
	  "1",
	  2
	 },
	 3
	]).

%% given mjson:new([{"1", 2}, 3]) output is
%% {error, bad_arg}
new5(Json) ->
    JExpected = {error, bad_arg},
    ?_assertEqual(Json, JExpected).

-endif.
