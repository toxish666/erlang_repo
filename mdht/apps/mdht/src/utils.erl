%%% @doc Module with utility functions.
%%% @end

-module(utils).

-export([
	 get_opt_or_default/2,
	 map_opt_or_default/3,
	 binary_search_by/2,
	 rfind_index/2,
	 update_list_with_element/3,
	 delete_nth/2,
	 delete_nth_tup/2
	]).


%% @doc Extract value from Option or default if none 
-spec get_opt_or_default(mdht:option(term()), term()) -> term().
get_opt_or_default(none, Default) ->
    Default;
get_opt_or_default(Some, _) ->
    Some.


%% @doc Extract value from Option or default if none 
-spec map_opt_or_default(
	Option :: mdht:option(term()), 
	MapF :: fun((term()) -> term()),
	Default :: term()
       ) -> term().
map_opt_or_default(none, _, Default) ->
    Default;
map_opt_or_default(Some, MapF, _) ->
    MapF(Some).


%% @doc Binary search with a comparator function.
%% Note that comparator function should return mdht:ordering()
%% If the lambda returns equal than {some, Res} is returned;
%% If no element were found than {error, Res} is returned where Res
%% is the position where a matching element could be inserted.
%% @end
-spec binary_search_by(list(), fun((atom()) -> mdht:ordering())) -> mdht:either(non_neg_integer(), non_neg_integer()).
binary_search_by(List, Comparator) ->
    SortedList = lists:sort(List),
    binary_search_by(Comparator, 1, length(List), SortedList).

binary_search_by(_Comparator, Left, Right, OrigList ) when Left > Right ->
    if Right == length(OrigList) ->
	    {error, Right};
       true ->
	    {error, Left}
    end;
binary_search_by(Comparator, Left, Right, OrigList ) when Left =< Right ->
    Middle = (Left + Right) div 2, 
    Item = lists:nth(Middle, OrigList),
    case Comparator(Item) of
	equal -> 
	    {some, Middle};
	greater ->
	    binary_search_by(Comparator, Left, Middle-1,  OrigList);
	less ->
	    binary_search_by(Comparator, Middle+1, Right , OrigList)
    end.
    

%% @doc Searches for an element in a list from the right, returning its index.
%% It takes lambda that returns true or false.
%% @end
-spec rfind_index(list(), fun((atom()) -> boolean())) -> mdht:option(non_neg_integer()).
rfind_index(List, Predicate) ->
    ReversedList = lists:reverse(List),
    rreversed(ReversedList, Predicate, length(List)).
    
rreversed([], _, _) ->
    none;
rreversed([H|L], Predicate, Position) ->
    case Predicate(H) of
	true ->
	    Position;
	false ->
	    rreversed(L, Predicate, Position - 1)
    end.


%% @doc Update list by takin n'th element and putting there new element
-spec update_list_with_element(list(), non_neg_integer(), atom()) -> list().
update_list_with_element(List, Nth, NewElement) ->
    {FirstPart, ElsePart} = lists:split(Nth - 1, List),
    [_ | E] = ElsePart,
    NewElsePart = [NewElement | E],
    FirstPart ++ NewElsePart.
    

%% @doc Delete n'th element from list and return list
-spec delete_nth(list(), non_neg_integer()) -> list().
delete_nth(List, Nth) ->
    {FirstPart, ElsePart} = lists:split(Nth - 1, List),
    [_ | E] = ElsePart,
    FirstPart ++ E.


%% @doc Delete n't element from list and return that deleted element and list
-spec delete_nth_tup(list(), non_neg_integer()) -> {any(), list()}.
delete_nth_tup(List, Nth) ->
    {FirstPart, ElsePart} = lists:split(Nth - 1, List),
    [FF | E] = ElsePart,
    {FF, FirstPart ++ E}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

binary_search_by_test() ->
    SomeList = [1,2,3,4,5,6,8,9,12,14,21],
    FunN = fun(N) -> 
			fun(E) ->			
				if E == N ->
					equal;
				   true ->
					if E < N ->
						less;
					   true ->
						greater
					end
				end
			end
		end,
    LongList = lists:seq(3,100000),
    ?assertEqual({some,4}, utils:binary_search_by(LongList, FunN(6))),
    ?assertEqual({some,2}, utils:binary_search_by(SomeList, FunN(2))),
    ?assertEqual({some,6}, utils:binary_search_by(SomeList, FunN(6))),
    ?assertEqual({error,7}, utils:binary_search_by(SomeList, FunN(7))),
    ?assertEqual({error,10}, utils:binary_search_by(SomeList, FunN(13))),
    ?assertEqual({some,1}, utils:binary_search_by(SomeList, FunN(1))),
    ?assertEqual({error,1}, utils:binary_search_by(SomeList, FunN(0))),
    ?assertEqual({error, length(SomeList)}, utils:binary_search_by(SomeList, FunN(55))).


rfind_index_test() ->
    SomeList = [1,2,3],
    ?assertEqual(3, utils:rfind_index(SomeList, fun(E) -> E == 3 end)),
    ?assertEqual(none, utils:rfind_index(SomeList, fun(E) -> E == 5 end)).
    

-endif.
