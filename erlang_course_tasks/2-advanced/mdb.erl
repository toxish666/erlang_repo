%%
%%
%% @author toxish666 [https://github.com/toxish666]
%% @doc This module contains functions simulating database structure.
%% Inner representation of a DB inside this module is a binary tree
%% @end
%% @reference See <a href="https://github.com/bitgorbovsky/erlang-course-tasks/blob/master/tasks/2-advanced.md#20-%D0%B1%D0%B0%D0%B7%D0%B0-%D0%B4%D0%B0%D0%BD%D0%BD%D1%8B%D1%85"> task itself </a> for more info.
%%
%%

-module(mdb).

-export([new/0,
	 new/1,
	 destroy/1,
	 write/3,
	 delete/2,
	 read/2,
	 match/2,
	 append/3,
	 batch_delete/2,
	 batch_read/2]).

-export([mjson_to_mdb/1]).

-on_load(load_mjson_module/0).


-type tree() :: {{key(), any()}, tree(), tree()}.

-type key() :: any().

-type keylist() :: [key(), ...].

-type opt() :: {append, allow|deny} | {batch, Number :: non_neg_integer()}.

-type db() :: {[opt()], tree()}.

%% you better compile mjson in 1-basic folder to .beam format
load_mjson_module() ->
    code:add_path("../1-basic"),
    code:atomic_load([mjson]).


-spec new(Params) -> db() when
      Params :: [opt()].
%% @doc creates empty `Db` given parameters
new(P) ->
    {P, {}}.
new() ->
    new([{append, allow}]).


%% @doc releases resources used for db
destroy(_Db) -> 
    ok.


-spec write(Key, Element, Db) -> Db | {error, append_deny, Db} when
      Key :: key(),
      Element :: any(),
      Db :: db().
%% @doc adds element to db given key (or rewrite it)
write(K, E, Db = {Params, _Tree}) ->
    write(K, E, Db, Params).

write(K, E, _Db = {Params, Tree}, [{append, allow}| _]) -> 
    {Params, write_impl(K, E, Tree)};   
write(_, _, Db, [{append, deny}| _]) -> 
    {error, append_deny, Db};
write(K, E, Db, [_ | OT]) ->
    write(K, E, Db, OT).
%% assuming tree structure 
%% Key, Element, Tree {{Root key, Root element}, {Smaller subtree}, {Larger subtree}}
write_impl(K, E, {}) ->
    {{K, E}, {}, {}};
write_impl(K, E, {{RK, RE}, S, L}) when K < RK ->
    {{RK, RE}, write_impl(K, E, S), L};
write_impl(K, E, {{RK, RE}, S, L}) when K > RK ->
    {{RK, RE}, S, write_impl(K, E, L)};
write_impl(K, E, {{K, _}, S, L}) ->
    {{K, E}, S, L}.


-spec delete(Key, Db) -> Db when
      Key :: key(),
      Db :: db().
%% @doc deletes element given key from db
delete(K, _Db = {Params, Tree}) ->
    {Params, delete_impl(K, Tree)}.

delete_impl(K, [H|T]) ->
    [delete_impl(K, H)] ++ delete_impl(K, T);
delete_impl(K, {{RK, RD}, S, L}) when K < RK ->
    DRK = delete_impl(K, RK),
    DRD = delete_impl(K, RD),
    {{DRK, DRD}, delete_impl(K, S), L};
delete_impl(K, {{RK, RD}, S, L}) when K > RK ->
    DRK = delete_impl(K, RK),
    DRD = delete_impl(K, RD),
    {{DRK, DRD}, S, delete_impl(K, L)};
delete_impl(K, {{K, _}, {}, {}}) ->
    {};
delete_impl(K, {{K, _}, {}, L}) ->
    L;
delete_impl(K, {{K, _}, S, {}}) ->
    S;
delete_impl(K, {{K, RD}, S, L}) ->
    DRD = delete_impl(K, RD),
    delete_impl(K, rotate(K,{{K, DRD}, S, L}));
delete_impl(_, V) ->
    V.

rotate(K, {{RK, RD}, S, L}) when K < RK ->
    {{RK, RD}, rotate(K, S), L};
rotate(K, {{RK, RD}, S, L}) when K > RK ->
    {{RK, RD}, S, rotate(K, L)};
rotate(K, {{K, RD}, {{RSK, RSD}, SS, SL}, L}) ->
    {{RSK, RSD}, SS, {{K, RD}, SL, L}};
rotate(_Key, _Db) -> {error, not_found}.


-spec read(Key, Db) -> {ok, Element} | {error, not_found} when
      Key :: key(),
      Db :: db(),
      Element :: any().
%% @doc find element in db
read(K, _Db = {_, T}) ->
    try read1(K, T) of 
	{error, not_found} = ER ->
	    ER
    catch
	Val ->
	    {ok, Val}
    end.
%% Key, {{RootKey, RootValue}, SmallSubtree, LargeSubtree}
read1(K, {{RK, RV}, S, _}) when K < RK ->
    read1(K, RK),
    read1(K, RV),
    read1(K, S);
read1(K, {{RK, RV}, _, L}) when K > RK ->
    read1(K, RK),
    read1(K, RV),
    read1(K, L);
read1(K, {{K, RD}, _, _}) ->
    throw(RD);
read1(K, [H|L]) ->
    read1(K, H),
    read1(K, L);
read1(_, _) ->
    {error, not_found}.


-spec match(Element, Db) -> KeyList when
      Element :: any(),
      Db :: db(),
      KeyList :: keylist().
%% @doc find keys corresponding to a given element
match(E, {_, L}) when is_list(L) ->
    match_without_prop_list(E, L);
match(E, {_, T}) ->
    match_without_prop(E, T).

match_without_prop_list(_, []) ->
    [];
match_without_prop_list(E, [H|T]) ->
    match_without_prop(E, H) ++ match_without_prop_list(E, T) ++ [].

match_without_prop(E, T) when is_tuple(T)->
    match(E, tuple_to_list(T), []);
match_without_prop(E, T) ->
    match(E, T, []).

match(_, [], A) ->
    A;
match(E, [{} | T], A) ->
    match(E, T, A);
match(E, [{RK, E} | T], A) ->
    MatchComplexRootKey = match_without_prop(E, RK),
    match(E, T, [RK | A] ++ MatchComplexRootKey);
match(E, [{RK, RV} | T], A) ->
    MatchComplexRootKey = match_without_prop(E, RK),
    MatchComplexRootValue = match_without_prop(E, RV),
    match(E, T, MatchComplexRootKey ++ MatchComplexRootValue ++ A);
match(E, [{{RK, E}, S, L} | T], A) ->
    MatchComplexRootKey = match_without_prop(E, RK),
    match(E, [S, L | T], MatchComplexRootKey ++ [RK | A]);
match(E, [{{RK, RV}, S, L} | T], A) ->
    MatchComplexRootKey = match_without_prop(E, RK),
    MatchComplexRootValue = match_without_prop(E, RV),
    match(E, [S, L | T], MatchComplexRootKey ++ MatchComplexRootValue ++ A);
match(_, _, A) ->
    A.


%% @doc append = delete
append(K, E, Db) ->
    write(K, E, Db).


-spec batch_delete(KeyList, Db) -> Db | {error, batch_limit} when
      KeyList :: keylist(),
      Db :: db().
%% @doc deletes data based on keylist
%% if no batch has been given - batch is equal to 0
%% @end
batch_delete(KL, Db = {P, _}) ->
    batch_delete(KL, Db, P).

batch_delete(KL, Db, []) ->
    batch_delete(KL, Db, [{batch, 0}]);
batch_delete(KL, Db, [{batch, N} | _]) ->
    batch_delete(KL, Db, N);
batch_delete(KL, Db, [_|T]) ->
    batch_delete(KL, Db, T);
batch_delete([], Db, N) when is_number(N) ->
    Db;
batch_delete([_|_], _, 0) -> 
    {error, batch_limit};
batch_delete([K|T], Db, N) when is_number(N) -> 
    batch_delete(T, delete(K, Db), N - 1).


-spec batch_read(KeyList, Db) -> [{Key, Element}] | {error, instance} | {error, batch_limit} when
      KeyList :: keylist(),
      Db :: db(),
      Key :: key(),
      Element :: any().
%% @doc find elements based on keylist
%% if no batch has been given - batch is equal to 0
%% @end
batch_read(KL, Db = {P, _}) ->
    batch_read(KL, Db, P).

batch_read(KL, Db, []) ->
    batch_read(KL, Db, [{batch, 0}]);
batch_read(KL, Db, [{batch,N} | _]) when is_number(N) -> 
    batch_read(KL, Db, N, []);
batch_read(KL, Db, [_| T]) -> 
    batch_read(KL, Db, T).

batch_read([], _, _, A) -> 
    A;
batch_read([_|_], _, 0, _) -> 
    {error, batch_limit};
batch_read([K|T], Db, N, A) -> 
    case read(K, Db) of
	{ok, El} ->
	    batch_read(T, Db, N - 1, [El | A]); 
	_ -> 
	    {error, instance}
    end.


-spec mjson_to_mdb(Json) -> Db when
      Json :: mjson:json(),
      Db :: db().
%% @doc mjson to mdb converter
%% it's assumed that an order in json array doesn't preserved in a db's tree structure;
%% items in a list are keys without data
%% consider
%%   "cars": [
%%     "models":[ "Fiesta", "Focus", "Mustang" ],
%%     "models":[ "320", "X3", "X5" ],
%%     "models":[ "500", "Panda" ]
%%  ]
%% here "cars" is the "root" object in a tree:  {"cars", ...}
%% ... is the corresponding array which is storred as a nested tree
%% 
%%
%% "models":[ "Fiesta", "Focus", "Mustang" ] should be encoded in terms of db as
%% DBI=db:new(), DBI1 = db:write("Fiesta", [], DBI), DBI2 = ..., DBI3 = ...,
%% FinalDB = db:write("models", ["Fiesta","Focus","Mustang"], DBI3). DBI should not 
%% contains property part
%%
%% at the same time json:new returns above json in the next format:
%% #{"cars", [#{"models", ["Fiesta", "Focus", "Mustang"]}, ...]}
%%
%%
%% consider [2, #{"1"=>3}, [1,2,3]]. 2 should be left as is, map should 
%% be encoded as inner db structure and array too.
%%
%%
%% @TODO as of now #{"key1" => value1, "key2" => value2} which is 
%% {"key1":value1, "key2":value2} is not supported, only #{"key1" => value1}
mjson_to_mdb(J) ->
    InnerDb = mjson_to_mdb_m(J, new_tree_struc()),
    Db = new(),
    {Params, _} = DbJ = write(top_scope, InnerDb, Db),
    {ok, Tree} = read(top_scope, DbJ),
    {Params, Tree}.

mjson_to_mdb_m(J, Db) when is_map(J) ->
    Iter = maps:iterator(J),
    mjson_to_mdb_iter(Db, Iter);
mjson_to_mdb_m([], Db) ->
    Db;
mjson_to_mdb_m([HJ|T], Db) when is_map(HJ) ->
    InnerDb = mjson_to_mdb_m(HJ, Db),
    NextDb = mjson_to_mdb_m(T, InnerDb), % or Db
    NextDb.

%% only #{"key1" => value1} patterns are allowed!
mjson_to_mdb_iter(Db, I) ->
    {K, V, _} = maps:next(I),
    case V of
	VM when is_map(VM) ->
	    NewDb = mjson_to_mdb_m(VM, new_tree_struc()),
	    write_impl(K, NewDb, Db);
	VL when is_list(VL) ->
	    NewDb = mjson_to_mdb_list(VL, new_tree_struc()),
	    write_impl(K, NewDb, Db);
	V ->
	    write_impl(K, V, Db)
    end.

mjson_to_mdb_list([], Db) ->
    Db;
mjson_to_mdb_list([H|L], Db) when is_map(H) ->
    InnerDb = mjson_to_mdb_m(H, new_tree_struc()),
						%mjson_to_mdb_list(L, Db, [InnerDb|A]);
    mjson_to_mdb_list(L, write_impl(InnerDb, "", Db));
mjson_to_mdb_list([H|L], Db) when is_list(H) ->
    InnerDb = mjson_to_mdb_list(H, new_tree_struc()),
						%mjson_to_mdb_list(L, Db, [InnerDb|A]);
    mjson_to_mdb_list(L, write_impl(InnerDb, "", Db));
mjson_to_mdb_list([H|L], Db) ->
						%mjson_to_mdb_list(L, Db, [H|A]).
    mjson_to_mdb_list(L, write_impl(H, "", Db)). 

new_tree_struc() ->			     
    take_tree_from_db(new()).

take_tree_from_db(Db) ->
    case Db of 
	{_Params, Tree} ->
	    Tree
    end.


%% ----------------------------------------------------------------
%% test suite
%% ----------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

module_test_() -> 
    [{"test suite for json 1",
      {foreach, fun start1/0, [fun deletet1/1, fun readt1/1, fun matcht1/1]}},
     {"test suite for json 2",
       {foreach, fun start2/0, [fun deletet2/1, fun matcht2/1]}}
    ].

start1() ->
    J = #{"1" => #{"5" => [1, #{"12" => 42}, 3]}},
    mjson_to_mdb(J).

deletet1(Db) ->
    DbWithout1 = delete("12", Db),
    DbWithout2 = delete("5", Db),

    [
     ?_assertEqual(read("12", Db), {ok, 42}),
     ?_assertEqual(read("12", DbWithout1), {error, not_found}),
     ?_assertEqual(read("5", Db), {ok,{{1,[]},{},{{{{"12",42},{},{}},[]},{{3,[]},{},{}},{}}}}),
     ?_assertEqual({[{append,allow}],{{"1",{}},{},{}}}, DbWithout2)
    ].

readt1(Db) ->   
    [
     ?_assertEqual(read("12", Db), {ok, 42}),
     ?_assertEqual(read("5", Db), {ok,{{1,[]},{},{{{{"12",42},{},{}},[]},{{3,[]},{},{}},{}}}})
    ].

matcht1(Db) ->
    [
     ?_assertEqual(match(42, Db), ["12"]),
     ?_assertEqual(mdb:match([], Db), [3,{{"12",42},{},{}},1])
    ].


start2() ->
    J = [#{"1" => [888,#{"2" => 999},#{"2" => [1000, #{"3" => true}]},111]}],
    mjson_to_mdb(J).

deletet2(Db) ->
    DbWithout1 = delete("3", Db),
    DbWithout2 = delete("2", Db),

    [
     ?_assertEqual(read("3", Db), {ok, true}),
     ?_assertEqual(read("3", DbWithout1), {error, not_found}),
     ?_assertEqual(read("2", Db), {ok, 999}),
     ?_assertEqual(read("2", DbWithout2), {error, not_found})
    ].

matcht2(Db) ->
    [
     ?_assertEqual(match(true, Db), ["3"]),
     ?_assertEqual(match(999, Db), ["2"])
    ].

