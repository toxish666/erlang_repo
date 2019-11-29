-module(distrib_worker).

-behaviour(gen_server).

-export([
	 start_link/0
	]).

-export([
	 init/1, 
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2
	]).

-record(state, {
	       replicahost = empty,
	       stack = []
	       }).


-define(CHANCE_TO_FAIL, 20). % 1 to N chance to fail


%%----------------------------------------------------------------------------
%% LIFECYCLE
%%----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).


init([]) ->
    {ok, #state{}}.


handle_call(_Msg, _From, State) ->
    {noreply, State}.


handle_cast({replicate_to, empty}, State) ->
    {noreply, State};
handle_cast({replicate_to, Partner}, #state{stack = Stack} = State) ->
    gen_server:cast(Partner, {full_replica, self(), Stack}),
    {noreply, State#state{replicahost = Partner}};
handle_cast({add_work, Lmbd}, #state{stack = Stack, replicahost = ReplicaHost} = State) ->
    % check if Lmbd comes from dead worker
    case Lmbd of
	{replicated, Lambda} ->
	    statistics:restarted_task(self()),
	    Task = Lambda,
	    send_to_replica(Task, ReplicaHost),
	    gen_server:cast(self(), {start_work});
	Work ->
	    Task = Work,
	    send_to_replica(Task, ReplicaHost)
    end,
    {noreply, State#state{stack = [Task|Stack]}};
handle_cast({start_work}, #state{stack = Stack, replicahost = ReplicaHost} = State) ->
    case Stack of
	[] ->
	    {noreply, State};
	[Lmbd | Tail] ->
	    statistics:started_task(self()),
	    case rand:uniform(?CHANCE_TO_FAIL) of
		1 ->
		    statistics:failed_task(self()),
		    io:format("death pill~n"),
		    {stop, normal, State};
		_ ->
		    Lmbd(),
		    statistics:completed_task(self()),
		    delete_from_replica(Lmbd, ReplicaHost),
		    gen_server:cast(self(), {start_work}),
		    {noreply, State#state{stack = Tail}}
	    end
    end.


handle_info(Msg, State) ->
    io:format("Got unexpected message ~p~n", [Msg]),
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 

terminate(_Reason, _State) ->
    ok.
   

%%----------------------------------------------------------------------------
%% HANDLERS
%%----------------------------------------------------------------------------
send_to_replica(_, empty) ->
    ok;
send_to_replica(Lmbd, ReplicaHost) ->
    gen_server:cast(ReplicaHost, {append_replica, self(), Lmbd}),
    ok.


delete_from_replica(_, empty) ->
    ok;
delete_from_replica(Lmbd, ReplicaHost) ->
    gen_server:cast(ReplicaHost, {delete_replica, self(), Lmbd}),
    ok.
