%%% @doc Module with utility functions.
%%% @end


-module(utils).


-export([
	 get_opt_or_default/2,
	 map_opt_or_default/3
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

