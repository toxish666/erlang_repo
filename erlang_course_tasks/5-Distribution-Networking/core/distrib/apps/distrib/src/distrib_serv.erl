-module(distrib_serv).

-behaviour(gen_server).

-export([start_link/0]).
-export([
	 init/1, 
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2
	]).

-define(SERVER, distrib_serv). 
-define(WAIT_FOR_RESOURCES, 5000).

-record(state, {
		workers
	       }).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).


init([]) ->
    rd:add_local_resource(?SERVER, self()),
    rd:add_target_resource_type(?SERVER),
    rd:trade_resources(),
    io:format(" Waiting for resource discovery...~n"),
    timer:sleep(?WAIT_FOR_RESOURCES),
    io:format(" Finished waiting for resource discovery.~n"),
    {ok, #state{}}.


handle_call(Msg, From, State) ->
    {noreply, State}.


handle_cast(Msg, State) ->
    {noreply, State}.


handle_info(Msg, State) ->
    {noreply, State}.
  

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 

terminate(_Reason, _State) ->
    ok.
    

-compile(export_all).
fiva() ->
    {ok, Servers} = rd:fetch_resources(?SERVER),
    io:format("GOT : ~p~n", [Servers]).


choose_replication_partner() ->
    Self = self(),
    {ok, Servers} = rd:fetch_resources(?SERVER),
    Servers2 = lists:filter(fun(X) -> X =/= Self end, Servers),
    Index = rand:uniform(length(Servers2)),
    Partner = lists:nth(Index, Servers2),
    Partner.
