-module(distrib_serv).

-behaviour(gen_server).

-export([
	 start_link/0,
	 load_tasks_list_internal/0,
	 initiate_loaded_work/0,
	 load_and_initiate/0
	]).

-export([
	 init/1, 
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2
	]).



-define(WORKER_COUNT, 4).
-define(SERVER, distrib_serv). 
-define(WAIT_FOR_RESOURCES, 1000).


-record(state, {
		replicate_partner,
		queue = [],
		workers = [],
		replicas = [] % [{pid, [lambdas]}]
	       }).


%%----------------------------------------------------------------------------
%% PUBLIC API
%%----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).


load_tasks_list_internal() ->
    {ok, ComplexityList} = application:get_env(distrib, tasks_list),
    TasksList = lists:map(fun(N) -> fun () -> io:format("Got task ~p~n", [N]), timer:sleep(N) end end, ComplexityList),
    gen_server:cast(?SERVER, {add_work_replicated, TasksList}).


initiate_loaded_work() ->
    gen_server:cast(?SERVER, {initiate_work}).


load_and_initiate() ->
    load_tasks_list_internal(),
    initiate_loaded_work().


%%----------------------------------------------------------------------------
%% LIFECYCLE
%%----------------------------------------------------------------------------
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


%% tell own workers and all other node in cluster to start work
handle_cast({initiate_work}, State) ->
    {ok, Servers} = rd:fetch_resources(?SERVER),
    lists:foreach(fun (Server) -> gen_server:cast(Server, {start_work}) end, Servers),
    {noreply, State};
%% tell workers to start doing their tasks
handle_cast({start_work}, #state{workers = Workers} = State) ->
    lists:foreach(fun (Pid) -> gen_server:cast(Pid,{start_work}) end, Workers),
    {noreply, State};
%% add some tasks for workers
handle_cast({add_work, TasksList}, #state{workers = Workers} = State) ->
    TasksListLength = length(TasksList),
    WorkersLength = length(Workers),
    add_tasks_for_workers_multiple(TasksList, Workers, trunc(TasksListLength / WorkersLength)),
    {noreply, State};
%% distribute tasks between nodes and add some tasks locally
handle_cast({add_work_replicated, TasksList}, State) ->
    {ok, Servers} = rd:fetch_resources(?SERVER),
    % distribute evenly between servers(nodes)
    TasksListLength = length(TasksList),
    add_tasks_distributed(Servers, TasksList, trunc(TasksListLength / ?WORKER_COUNT), Servers),
    {noreply, State};
%% standart replicate msg
handle_cast({replicate}, #state{workers = Workers} = State) ->
    Partner = choose_replication_partner(),
    lists:foreach(fun (Pid) -> gen_server:cast(Pid, {replicate_to, Partner}) end, Workers),
    {noreply, State#state{replicate_partner = Partner}};
%% replicate only one child
handle_cast({replicate, Pid}, #state{replicate_partner = Partner} = State) ->
    gen_server:cast(Pid, {replicate_to, Partner}),
    {noreply, State};
%% add some workers
handle_cast({add_worker, Pid}, #state{workers = Workers} = State) ->
    monitor(process, Pid),
    {noreply, State#state{workers = [Pid | Workers]}};
%% worker from another node has sent us message telling to us to add full replica
handle_cast({full_replica, WorkerPid, LmbdList}, #state{replicas = Replicas} = State) ->
    monitor(process, WorkerPid),
    {noreply, State#state{replicas = [{WorkerPid, LmbdList} | Replicas]}};
%% worker from another node has sent us message telling to append some value
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
    {noreply, State#state{replicas = [{WorkerPid, ElseLmbdStack} | ElseReplicas]}}.


%% we can receive monitor messages from workers that are on different nodes
%% or our own workers so we can respawn them
handle_info(
  {'DOWN', _Ref, process, Pid, _Reason}, 
  #state{workers = Workers, replicas = Replicas} = State
 ) ->
    % check if own worker is dead
    case lists:search(fun(P) -> P == Pid end, Workers) of 
	{value, Pid} ->
	    ChildPid = create_child(),
	    gen_server:cast(self(), {replicate, ChildPid}),
	    {noreply, State#state{workers = lists:delete(Pid, Workers)}};
        % monitor from different node
	false ->
	    {value, {Pid, LmbdStack}, ElseReplicas} = lists:keytake(Pid, 1, Replicas),
	    LmbdStackPacked = lists:map(
				fun(Lmbd) -> {replicated, Lmbd}
				end, LmbdStack),
	    gen_server:cast(?SERVER, {add_work_replicated, LmbdStackPacked}),
	    {noreply, State#state{replicas = ElseReplicas}}
    end;
handle_info(Msg, State) ->
    io:format("Got unexpected message: ~p~n", [Msg]),
    {noreply, State}.



code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, _State) ->
    ok.


%%----------------------------------------------------------------------------
%% HANDLERS
%%----------------------------------------------------------------------------
choose_replication_partner() ->
    {ok, Servers} = rd:fetch_resources(?SERVER),
    Servers2 = lists:filter(fun(X) -> X =/= self() end, Servers),
    ServersLength = length(Servers2),
    case ServersLength of 
	0 ->
	    empty;
	_ ->
	    Index = rand:uniform(length(Servers2)),
	    Partner = lists:nth(Index, Servers2),
	    Partner
    end.


add_tasks_for_workers([T|TL], [W|WL]) ->
    gen_server:cast(W, {add_work, T}),
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
    {NTasks, ElseTasks} = lists:split(length(Workers), TasksList),
    add_tasks_for_workers(NTasks, Workers),
    add_tasks_for_workers_multiple(ElseTasks, Workers, N - 1).


%% number of tasks smaller than number of workers and all servers got their tasks
add_tasks_distributed([], TasksList, 0, _) ->
    gen_server:cast(?SERVER, {add_work, TasksList});
%% all servers got their tasks and there's more of them so go round-robbin again 
add_tasks_distributed([], TasksList, N, [Server|ElseServers] = Servers) ->
    {TasksToServer, ElseTasks} = lists:split(?WORKER_COUNT, TasksList),
    gen_server:cast(Server, {add_work, TasksToServer}),
    add_tasks_distributed(ElseServers, ElseTasks, N - 1, Servers);
%% some tasks are left but it's count is smaller than worker count
add_tasks_distributed([_|_] = ServersList, TasksList, 0, _) ->
    % choose server randomly
    Nth = rand:uniform(length(ServersList)),
    Server = lists:nth(Nth, ServersList),
    gen_server:cast(Server, {add_work, TasksList});
%% more than worker count tasks are there still
add_tasks_distributed([Server|ElseServers], TasksList, N, Servers) when is_integer(N) ->
    {TasksToServer, ElseTasks} = lists:split(?WORKER_COUNT, TasksList),
    gen_server:cast(Server, {add_work, TasksToServer}),
    add_tasks_distributed(ElseServers, ElseTasks, N - 1, Servers).


%%----------------------------------------------------------------------------
%% WORKERS
%%----------------------------------------------------------------------------
create_children(N) ->
    [create_child() || _ <- lists:seq(1, N)].


create_child() ->
    Pid = create(),
    gen_server:cast(?SERVER, {add_worker, Pid}),
    Pid.


create() ->
    {ok, WorkerPid} = distrib_worker:start_link(),
    WorkerPid.



