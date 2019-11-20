-module(atm_serv).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
code_change/3, terminate/2]).


-record(state, {
		capacity,
		refs = [],%%gb_trees:empty(),
		socket
	       }). 

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).


init(Socket) ->
    {ok, AtmCapacity} = application:get_env(capacity),
    gen_server:cast(self(), accept),
    {ok, #state{capacity = AtmCapacity, socket=Socket}}.


handle_call(_E, _From, State) ->
    {noreply, State}.


handle_cast(accept, #state{socket = ListenSocket, capacity = Cap, refs = Refs} = S) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    %case gb_trees:size(Refs) of
    case length(Refs) of
	L when L == Cap ->
	    send(AcceptSocket, "Server capacity is full", []),
	    gen_tcp:close(AcceptSocket),
	    {noreply, S};
	_ ->
	    Fiva = [{1, 1234, 5}, {2, 1231, 10}],
	    {ok, Pid} = atm_state:start_link(Fiva),
	    Ref = monitor(process, Pid),
	    gen_tcp:controlling_process(AcceptSocket, Pid),
	    N = [{AcceptSocket, Pid, Ref} | Refs],
	    {noreply, S#state{refs = N}}
    end.
				

handle_info({tcp, Socket, Str}, #state{refs = Refs} = S) ->
    %{_Pid, _Ref} = gb_trees:get(Socket, Refs),
    case Str of
	A ->
	    io:format("GOT: ~p~n", [A])
    end,
    {noreply, S};
handle_info({'DOWN', Ref, process, Pid, _Reason}, S = #state{refs = Refs}) ->
    demonitor(Pid),
    io:format("REASON IS ~p ~p ~n", [_Reason, Pid]),
    %RefsList = gb_trees:to_list(Refs),
    %{AcceptSocket, _}  = lists:keyfind({Pid,Ref}, 2, RefsList),
    {noreply, S};
handle_info(_Event, State) ->
    {noreply, State}.    


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 

terminate(_Reason, _State) ->
    ok.



send(Socket, Str, Args) ->
    ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
    ok = inet:setopts(Socket, [{active, once}]),
    ok.



