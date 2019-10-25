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

-include_lib("eunit/include/eunit.hrl").

-type tree() :: {{key(), any()}, tree(), tree()}.

-type key() :: any().

-type keylist() :: [key(), ...].

-type opt() :: {append, allow|deny} | {batch, Number :: non_neg_integer()}.

-type db() :: {[opt()], tree()}.


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

delete_impl(K, {{RK, RD}, S, L}) when K < RK ->
    {{RK, RD}, delete_impl(K, S), L};
delete_impl(K, {{RK, RD}, S, L}) when K > RK ->
    {{RK, RD}, S, delete_impl(K, L)};
delete_impl(K, {{K, _}, {}, {}}) ->
    {};
delete_impl(K, {{K, _}, {}, L}) ->
    L;
delete_impl(K, {{K, _}, S, {}}) ->
    S;
delete_impl(K, {{K, RD}, S, L}) ->
    delete_impl(K, rotate(K,{{K, RD}, S, L})).

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
read(K, {_, T}) ->
    read(K, T);
read(K, {{RK, _}, S, _}) when K < RK ->
    read(K, S);
read(K, {{RK, _}, _, L}) when K > RK ->
    read(K, L);
read(K, {{K, RD}, _, _}) ->
    {ok, RD};
read(_, _) ->
    {error, not_found}.


-spec match(Element, Db) -> KeyList when
      Element :: any(),
      Db :: db(),
      KeyList :: keylist().
%% @doc find keys corresponding to a given element
match(E, {_, T}) ->
    match(E, tuple_to_list(T), []).

match(_, [], A) ->
    A;
match(E, [{} | T], A) ->
    match(E, T, A);
match(E, [{RK, E} | T], A) ->
    match(E, T, [RK | A]);
match(E, [{_, _RD} | T], A) ->
    match(E, T, A);
match(E, [{{RK, E}, S, L} | T], A) ->
    match(E, [S, L | T], [RK | A]);
match(E, [{{_, _RD}, S, L} | T], A) ->
    match(E, [S, L | T], A).


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


-ifdef(test).



-endif.
