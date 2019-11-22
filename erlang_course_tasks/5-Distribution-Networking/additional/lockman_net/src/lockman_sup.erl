-module(lockman_sup).

-behaviour(supervisor).


-export([
	 start_link/0, 
	 init/1
	]).


start_link() ->
    supervisor:start_link(?MODULE, []).


init([]) ->
    {ok, Port} = application:get_env(port),
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active,once}, {packet,line},
					      {mode, binary}]),	
    MaxRestart = 2,
    MaxTime = 3000,
    SupFlags = #{strategy => one_for_one, intensity => MaxRestart, period => MaxTime},
    ChildSpecForLockmanServer = [
				 % listening server
				 #{
				   id => lockman_listen_serv, 
				   start => {lockman_listen_serv, start_link, [ListenSocket]},
				   restart => permanent,
				   shutdown => 5000,
				   type => worker,
				   modules => [lockman_listen_serv]
				  },
				 % lockman actual server
				 #{
				   id => lockman_serv, 
				   start => {lockman_serv, start_link, []},
				   restart => permanent,
				   shutdown => 5000,
				   type => worker,
				   modules => [lockman_serv]
				  }
				],
    {ok, {SupFlags, ChildSpecForLockmanServer}}.
    

