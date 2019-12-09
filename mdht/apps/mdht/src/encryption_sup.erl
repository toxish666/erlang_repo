-module(encryption_sup).
-behaviour(supervisor).

-export([init/1]).
-export([start_link/1]).

start_link(MDhtServer) ->
    supervisor:start_link(?MODULE, MDhtServer).

init(MDhtServer) ->
    MaxRestart = 5,
    MaxTime = 3000,
    SupFlags = #{strategy => one_for_one, intensity => MaxRestart, period => MaxTime},
    ChildSpecForEncryptionServer = [#{
				      id => encryption_server, 
				      start => {encryption_server, start_link, [MDhtServer]},
				      restart => permanent,
				      shutdown => 5000,
				      type => worker,
				      modules => [encryption_server]
			    }],
    {ok, {SupFlags, ChildSpecForEncryptionServer}}.
