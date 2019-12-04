%%%-------------------------------------------------------------------
%% @doc restdb top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(restdb_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{
		    id => mdb_server,
		    start => {mdb_server, new, [mdb_server]},
		    shitdown => 5000,
		    type => worker,
		    modules => [mdb_server]
		   }],
    {ok, {SupFlags, ChildSpecs}}.
