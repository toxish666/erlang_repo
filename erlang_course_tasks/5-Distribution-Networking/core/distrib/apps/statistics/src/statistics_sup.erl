-module(statistics_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 0, period => 1000},
    SpecForServer = [#{
		       id => statistics_manager,
		       start => {statistics_manager, start_link, []},
		       restart => permanent, 
		       shutdown => 2000,
		       type => worker,
		       modules => [statistics_manager]
		      }],
    {ok, {SupFlags, SpecForServer}}.
