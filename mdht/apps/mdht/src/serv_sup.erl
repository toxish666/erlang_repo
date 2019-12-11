-module(serv_sup).
-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

start_link(Port) ->
    supervisor:start_link(?MODULE, [Port]).

init([Port]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 5,
                 period => 3000},
    MDhtServerSpec = #{id => mdht_server,
		       start => {mdht_server, start_link, [self()]},
		       restart => permanent,
		       shutdown => 5000,
		       type => worker,
		       modules => [mdht_server]},
    NetworkSup = #{id => network_sup,
		   start => {network_sup, start_link, [Port]},
		   restart => permanent, 
		   shutdown => 7000,
		   type => supervisor,
		   modules => [network_sup]},
    {ok, {SupFlags, [MDhtServerSpec, NetworkSup]}}.
