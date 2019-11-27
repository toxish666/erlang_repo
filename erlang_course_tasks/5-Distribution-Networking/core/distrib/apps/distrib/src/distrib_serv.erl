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


-define(CHANCE_TO_FAIL, 20). % 1 to N chance to fail
-define(WORKER_COUNT, 4).
-define(SERVER, distrib_serv). 
-define(WAIT_FOR_RESOURCES, 5000).

-record(state, {
		queue,
		workers,
		replicas % [{pid, lambda}]
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
    %% create a few children
    create_children(?WORKER_COUNT),
    %% replicate message
    gen_server:cast(?SERVER, {replicate}),
    {ok, #state{}}.


handle_call(_Msg, _From, State) ->
    {noreply, State}.


%% add new some work for workers
handle_cast({add_work, TasksList}, #state{workers = Workers}) ->
    TasksListLength = length(TasksList),
    WorkersLength = length(Workers),
    add_tasks_for_workers_multiple(TasksList, Workers, trunc(TasksListLength / WorkersLength));
%% distribute tasks between nodes and add some tasks locally
handle_cast({add_work_replicated, TasksList}, #state{workers = Workers}) ->
    {ok, Servers} = rd:fetch_resources(?SERVER),
    % distribute evenly
    ServersLength = length(Servers),
    ServersAndWorkersLength = ServersLength * ?WORKER_COUNT,
    TasksListLength = length(TasksList),
    add_tasks_distributed(Servers, TasksList, trunc(TasksListLength / ServersAndWorkersLength);
%% standart replicate msg
handle_cast({replicate}, #state{workers = Workers} = State) ->
    Partner = choose_replication_partner(),
    lists:foreach(fun (Pid) ->
			  Pid ! {replicate_to, Partner}
		  end, Workers),
    {noreply, State};
%% add some workers
handle_cast({add_worker, Pid}, #state{workers = Workers} = State) ->
    monitor(process, Pid),
    {noreply, State#state{workers = [Pid | Workers]}};
%% worker from another node has sent us append replica message
handle_cast({append_replica, WorkerPid, Lmbd}, #state{replicas = Replicas} = State) ->
    case lists:keytake(WorkerPid, 1, Replicas) of 
	false ->
	    {noreply, State#state{replicas = [{WorkerPid, [Lmbd]} | Replicas]}};
	{value, {WorkerPid, LmbdStack}, ElseReplicas} ->
	    {noreply, State#state{replicas = [{WorkerPid, [Lmbd|LmbdStack]} | ElseReplicas]}}
    end;
%% worker from another node has sent us delete replica message
handle_cast({delete_replica, WorkerPid, Lmbd}, #state{replicas = Replicas} = State) ->
    {value, {WorkerPid, [Lmbd | ElseLmbdStack]}, ElseReplicas} = lists:keytake(WorkerPid, 1 ,Replicas),
    {noreply, State#state{replicas = [{WorkerPid, ElseLmbdStack} | ElseReplicas]}};
handle_cast(_Msg, State) ->
    {noreply, State}.


%% we can receive monitor messages from workers that are on different nodes
handle_info(
  {'DOWN', _Ref, process, Pid, _Reason}, 
  #state{workers = Workers, replicas = Replicas} = State
 ) ->
    {From, LmbdStack} = lists:keyfind(Pid, 1, Replicas),
    gen_server:cast(?SERVER, {add_work_replicated, LmbdStack}),
    
	    
	    
    {noreply, State}.
  

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 

terminate(_Reason, _State) ->
    ok.
    

%%----------------------------------------------------------------------------
%% HANDLERS
%%----------------------------------------------------------------------------
choose_replication_partner() ->
    Self = self(),
    {ok, Servers} = rd:fetch_resources(?SERVER),
    Servers2 = lists:filter(fun(X) -> X =/= Self end, Servers),
    Index = rand:uniform(length(Servers2)),
    Partner = lists:nth(Index, Servers2),
    Partner.


add_tasks_for_workers([T|TL], [W|WL]) ->
    W ! {add_work, T},
    add_tasks_for_workers(TL, WL);
add_tasks_for_workers([], []) ->
    ok.


%% when number of Workers prevail number of tasks
add_tasks_for_workers_multiple(TasksList, Workers, 0) ->
    TasksListLength = length(TasksList),
    {FirstWorkers, _Else} = lists:split(TasksListLength, Workers),
    add_tasks_for_workers(TasksList, FirstWorkers);
%% else
add_tasks_for_workers_multiple(TasksList, Workers, N) when is_integer(N) ->
    {NTasks, ElseTasks} = lists:split(N, TasksList),
    add_tasks_for_workers(NTasks, Workers),
    add_tasks_for_workers_multiple(ElseTasks, Workers, N - 1).
    

add_tasks_distributed(Servers, TasksList, 0) ->
    length(TasksList)


%%----------------------------------------------------------------------------
%% WORKERS
%%----------------------------------------------------------------------------
create_children(N) ->
    [create_children() || lists:seq(1, N)].
			  
create_children() ->
    Pid = create(),
    gen_server:cast(?SERVER, {add_worker, Pid}),
    Pid.


create() ->
    spawn(?MODULE, server_loop, [empty, []]). 


server_loop(ReplicaHost, Stack) ->
    receive 
	{replicate_to, Partner} ->
	    ReplicaHost ! {full_replica, self(), Stack},
	    server_loop(Partner, Stack);
	{add_work, Lmbd} ->
	    send_to_replica(Lmbd, ReplicaHost),
	    server_loop(ReplicaHost, [Lmbd | Stack]);
	{start_work} ->
	    case Stack of
		[] ->
		    server_loop(ReplicaHost, Stack);
		[Lmbd | Tail] ->
		    % maybe fail
		    case rand:uniform(?CHANCE_TO_FAIL) of
			1 ->
			    exit(self(), death_pill);
			_ ->
			    ok
		    end,
		    Lmbd(),
		    delete_from_replica(Lmbd, ReplicaHost),
		    self() ! start_work,
		    server_loop(ReplicaHost, Tail)
	    end
    end.


send_to_replica(_, empty) ->
    ok;
send_to_replica(Lmbd, ReplicaHost) ->
    ReplicaHost ! {append_replica, self(), Lmbd},
    ok.


delete_from_replica(_, empty) ->
    ok;
delete_from_replica(Lmbd, ReplicaHost) ->
    ReplicaHost ! {delete_replica, self(), Lmbd},
    ok.

