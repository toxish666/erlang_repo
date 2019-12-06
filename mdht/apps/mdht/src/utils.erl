%%% @doc Module with utility functions.
%%% @end

-module(utils).


-export([
	 get_opt_or_default/2,
	 map_opt_or_default/3,
	 binary_search/2,
	 binary_search_by/2
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


%% @doc Binary search on lists
-spec binary_search(list(), atom()) -> mdht:option(non_neg_integer()).
binary_search(List, N) ->
    SortedList = lists:sort(List),
    binary_search(N, 1, length(List), SortedList).

binary_search(N, Left, Right, _OrigList ) when Left > Right ->
    none;
binary_search(N, Left, Right, OrigList ) when Left =< Right ->
  Middle = (Left + Right) div 2, 
  Item = lists:nth(Middle, OrigList),
  case Item of
    N -> 
	  Middle;
    _ -> case Item > N of
           true  -> binary_search(N, Left, Middle-1,  OrigList);
           false -> binary_search(N, Middle+1, Right , OrigList)
         end
  end.


%% @doc Binary search with a comparator function.
%% Note that comparator function should return mdht:ordering()
%% @end
-spec binary_search_by(list(), fun((atom()) -> mdht:ordering())) -> mdht:option(non_neg_integer()).
binary_search_by(List, Comparator) ->
    SortedList = lists:sort(List),
    binary_search_by(Comparator, 1, length(List), SortedList).

binary_search_by(Comparator, Left, Right, _OrigList ) when Left > Right ->
    none;
binary_search_by(Comparator, Left, Right, OrigList ) when Left =< Right ->
  Middle = (Left + Right) div 2, 
  Item = lists:nth(Middle, OrigList),
  case Comparator(Item) of
      equal -> 
	  Middle;
      greater ->
	  binary_search(Comparator, Left, Middle-1,  OrigList);
      less ->
	  binary_search(Comparator, Middle+1, Right , OrigList)
  end.
