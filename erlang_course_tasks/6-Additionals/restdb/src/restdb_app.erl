%%%-------------------------------------------------------------------
%% @doc restdb public API
%% @end
%%%-------------------------------------------------------------------

-module(restdb_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
				      {'_', [
					     {"/[:paste_id]", restdb_handler, []}
					    ]}
				     ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
							 env => #{dispatch => Dispatch}
							}),
    restdb_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http).
