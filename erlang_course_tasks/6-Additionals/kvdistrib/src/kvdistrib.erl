-module(kvdistrib).

-export([
	 store_local/2,
	 store_replicated/2,
	 get_local/1,
	 get_replicated/1,
	 get_with_fun_local/1,
	 get_with_fun_replicated/1
	]).

store_local(Key, Value) ->
    kvdistrib_serv:store_local({Key, Value}).

store_replicated(Key, Value) ->
    kvdistrib_serv:store_replicated({Key, Value}).

get_local(Key) ->
    kvdistrib_serv:get_local(Key).

get_replicated(Key) ->
    kvdistrib_serv:get_replicated(Key).

get_with_fun_local(MatchSpec) ->
    kvdistrib_serv:get_with_fun_local(MatchSpec).

get_with_fun_replicated(MatchSpec) ->
    kvdistrib_serv:get_with_fun_replicated(MatchSpec).
