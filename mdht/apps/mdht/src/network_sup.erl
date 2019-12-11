-module(network_sup).
-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

start_link(Port) ->
    supervisor:start_link(?MODULE, [Port]).

init([Port]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 3000},
    NetworkServerSpec = #{id => network_server,
			  start => {network_server, start_link, [Port]},
			  restart => permanent,
			  shutdown => 5000,
			  type => worker,
			  modules => [network_server]},
    {ok, {SupFlags, [NetworkServerSpec]}}.
