%%%-------------------------------------------------------------------
%% @doc mdht top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mdht_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Port = case application:get_env(mdht, test_port) of 
	       undefined ->
		   {ok, PortR} = application:get_env(mdht, port),
		   PortR;
	       %% for testing purposes
	       {ok, MockedPort} ->
		   MockedPort
	   end,
    SupFlags = #{strategy => rest_for_one,
                 intensity => 5,
                 period => 3000},
    KTreeSpec = #{id => ktree,
		  start => {ktree_server, start_link, []},
		  restart => permanent,
		  shutdown => 5000,
		  type => worker,
		  modules => [ktree_server]},
    ServSupSpec = #{id => serv_sup,
		    start => {serv_sup, start_link, [Port]},
		    restart => permanent, 
		    shutdown => 10000,
		    type => supervisor,
		    modules => [serv_sup]},
    {ok, {SupFlags, [KTreeSpec, ServSupSpec]}}.
