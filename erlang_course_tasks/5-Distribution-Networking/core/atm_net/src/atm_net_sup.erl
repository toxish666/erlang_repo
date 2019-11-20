-module(atm_net_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    {ok, Port} = application:get_env(port),
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active,once}, {packet,line},
					      {mode, binary}]),
    MaxRestart = 2,
    MaxTime = 3000,
    SupFlags = #{strategy => one_for_one, intensity => MaxRestart, period => MaxTime},
    ChildSpecForAtmServ = [#{
		  id => atm_serv, 
		  start => {atm_serv, start_link, [ListenSocket]},
		  restart => permanent,
		  shutdown => 5000,
		  type => worker,
		  modules => [atm_serv]
		 }],
    {ok, {SupFlags, ChildSpecForAtmServ}}.
