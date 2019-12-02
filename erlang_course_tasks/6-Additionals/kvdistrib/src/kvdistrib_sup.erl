%%%-------------------------------------------------------------------
%% @doc kvdistrib top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(kvdistrib_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 2,
                 period => 100},
    ChildSpecs = [#{id => kvdistrib_serv,       
		    start => {kvdistrib_serv, start_link, []},      
		    restart => permanent,   
		    shutdown => 5000, 
		    type => worker,       
		    modules => [kvdistrib_serv]}],
    {ok, {SupFlags, ChildSpecs}}.

