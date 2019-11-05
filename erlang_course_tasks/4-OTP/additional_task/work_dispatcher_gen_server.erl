-module(work_dispatcher_gen_server).

-behaviour(gen_server).

%%----------------------------------------------------------------------------
%% BEHAVIOUR EXPORTS
%%----------------------------------------------------------------------------
-export([
	 init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
         terminate/2, 
	 code_change/3
	]).

%%----------------------------------------------------------------------------
%% PUBLIC API EXPORTS
%%----------------------------------------------------------------------------
-export([
	 start_link/0, 
	 add_work/3,
	 acquire/2,
	 init_worker/0,
	 release/2
	]).
%%----------------------------------------------------------------------------
%% TEST API EXPORTS
%%----------------------------------------------------------------------------
-export([
	 test_add_work/0,
	 test_acquire/0
	 ]).

%%----------------------------------------------------------------------------
%% STRUCTURES
%%----------------------------------------------------------------------------
-record(worker, {
		 pid,
		 busyness
		}).

-record(state, {
		workers = [],
		prio_set = gb_sets:new(), %% {{Priority, Ref}, From, Work}
		awaiting = gb_sets:new() %% {Priority,Pid}
	       }).

%%----------------------------------------------------------------------------
%% PUBLIC API
%%----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

add_work(GenServerPid, Work, Priority)  ->
    gen_server:call(GenServerPid, {new_work, Priority, Work}, infinity).

acquire(GenServerPid, Priority) ->
    gen_server:call(GenServerPid, {acquire, Priority}, infinity).

release(GenServerPid, WorkerPid) ->
    gen_server:cast(GenServerPid, {release, WorkerPid}).

%%----------------------------------------------------------------------------
%% LIFECYCLE
%%----------------------------------------------------------------------------
init([]) -> 
    V1 = create_worker(spawn_link(?MODULE, init_worker, []), free),
    V2 = create_worker(spawn_link(?MODULE, init_worker, []), free),
    V3 = create_worker(spawn_link(?MODULE, init_worker, []), free),
    {ok, #state{workers = [V1, V2, V3]}}. 

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 

%%----------------------------------------------------------------------------
%% HANDLERS
%%----------------------------------------------------------------------------
handle_call({new_work, Priority, Work}, From, #state{workers = Workers, prio_set = WorkingQueue} = State) 
  when Priority > 0,
       Priority < 11 ->
    NewWorkingQueue = gb_sets:add({{Priority, make_ref()}, From, Work}, WorkingQueue),
    {HighestPriorityTask, NewWorkingQueue2} = gb_sets:take_largest(NewWorkingQueue),
    case lists:keytake(free, #worker.busyness, Workers) of
	false ->
	    {noreply, State#state{prio_set = NewWorkingQueue}};
	{value, #worker{pid = WorkerFounded} = FreeWorker, ElseWorkers} ->
	    WorkerFounded ! {self(), add_new_task, HighestPriorityTask},
	    BusyWorker = FreeWorker#worker{busyness = busy},
	    {noreply, State#state{workers = [BusyWorker|ElseWorkers], prio_set = NewWorkingQueue2}}
    end;
handle_call({acquire, Priority}, From, #state{workers = Workers, awaiting = Awaiting} = State)
  when Priority > 0,
       Priority < 11 ->
    case lists:keytake(free, #worker.busyness, Workers) of
	false ->
	    {noreply, State#state{awaiting = gb_sets:add({Priority, From}, Awaiting)}};
	{value, #worker{pid = WorkerFounded} = FreeWorker, ElseWorkers}	->
	    WorkerFounded ! {transferred_to_client},
	    BusyWorker = FreeWorker#worker{busyness = busy},	    
	    {reply, {ok, WorkerFounded}, State#state{workers = [BusyWorker|ElseWorkers]}}
    end.


handle_cast({work_done, WorkerPid, Result, Client}, #state{workers = Workers, prio_set = WorkingQueue} = State) ->
    gen_server:reply(Client, {work_done, Result}),
    {[FreedWorker], Rest} = lists:partition(fun(#worker{pid = Pid, busyness = Busyness}) ->
				 Pid == WorkerPid andalso Busyness == busy
					    end, Workers),
    NewFreeWorker = FreedWorker#worker{busyness = free},
    %% and then send another work (if any) to any free worker
    NewStateWorkers = [NewFreeWorker|Rest],
    case gb_sets:is_empty(WorkingQueue) of
	true ->
	    {noreply, State#state{workers = NewStateWorkers}};
	false ->
	    {HighestPriorityTask, NewWorkingQueue} = gb_sets:take_largest(WorkingQueue),
	    {value, #worker{pid = WorkerFounded} = FreeWorker, ElseWorkers} = lists:keytake(free, #worker.busyness, NewStateWorkers),
	    WorkerFounded ! {self(), add_new_task, HighestPriorityTask},
	    BusyWorker = FreeWorker#worker{busyness = busy},
	    {noreply, State#state{workers = [BusyWorker|ElseWorkers], prio_set = NewWorkingQueue}}
    end;
handle_cast({release, WorkerPid}, #state{workers = Workers, awaiting = Awaiting} = State) ->
    {[FreedWorker], Rest} = lists:partition(fun(#worker{pid = Pid, busyness = Busyness}) ->
				 Pid == WorkerPid andalso Busyness == busy
					    end, Workers),
    FreedWorker#worker.pid ! {make_me_free},
    NewFreeWorker = FreedWorker#worker{busyness = free},
    NewStateWorkers = [NewFreeWorker|Rest],
    case gb_sets:is_empty(Awaiting) of
	true ->
	    {noreply, State#state{workers = NewStateWorkers}};
	false ->
	    {{_Prio, HighestPriorityAwaiting}, NewAwaitings} = gb_sets:take_largest(Awaiting), 
	    {value, #worker{pid = WorkerFounded} = FreeWorker, ElseWorkers} = lists:keytake(free, #worker.busyness, NewStateWorkers),
	    WorkerFounded ! {transferred_to_client},
	    gen_server:reply(HighestPriorityAwaiting, {ok, WorkerFounded}),
	    BusyWorker = FreeWorker#worker{busyness = busy},
	    {noreply, State#state{workers = [BusyWorker|ElseWorkers], awaiting = NewAwaitings}}
    end;
handle_cast(_Smth, State) ->
    {noreply, State}.

%% simply recreate worker
handle_info({'EXIT', FromPid, Reason}, #state{workers = Workers} = State) when Reason =/= normal ->
    case lists:keytake(FromPid, #worker.pid, Workers) of
	false ->
	    {noreply, State};
	{value, _, ElseWorkers} ->
	    NewWorker = create_worker(spawn_link(?MODULE, init_worker, []), free),
	    {noreply, State#state{workers = [NewWorker|ElseWorkers]}}
    end;
handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.



%%----------------------------------------------------------------------------
%% WORKER FUNCTIONS
%%----------------------------------------------------------------------------
init_worker() ->
    loop_worker(free).

loop_worker(Atom) when Atom == free ->
    receive
	{transferred_to_client} ->
	    loop_worker(transferred_to_client);
	{Server, add_new_task, Task} ->
	    loop_worker(busy, {Server, Task})
    end;
loop_worker(Atom) when Atom == transferred_to_client ->
    receive
	{make_me_free} ->
	    loop_worker(free);
	{From, Ref, Task} ->
	    From ! {done, Ref, Task()},
	    loop_worker(transferred_to_client)
    end.

loop_worker(busy, {Server, {{_Priority, _Ref}, Client, Work}}) ->
    TaskResult = Work(),
    gen_server:cast(Server, {work_done, self(), TaskResult, Client}),
    loop_worker(free).



%%----------------------------------------------------------------------------
%% HELPER FUNCTIONS
%%----------------------------------------------------------------------------
create_worker(Pid, Busyness) ->
    #worker{pid = Pid, busyness = Busyness}.


%%----------------------------------------------------------------------------
%% TESTS
%%----------------------------------------------------------------------------
test_add_work() ->
    {ok, WorkManager} = work_dispatcher_gen_server:start_link(),
    [spawn(
       fun() ->
	       Work = fun() -> timer:sleep(5000), nothing end,
	       Priority = rand:uniform(10),
	       io:format("Add work with PID: ~p and Priority ~p ~n", [self(), Priority]),
	       Msg = work_dispatcher_gen_server:add_work(WorkManager, Work, Priority),
	       io:format("Priority ~p GETS result  : ~p ~n", [Priority,Msg])
       end
      ) || _ <- lists:seq(1,10)]. 



test_acquire() ->
   {ok, WorkManager} = work_dispatcher_gen_server:start_link(),
    [spawn(
       fun() ->
	       Priority = rand:uniform(10),
	       {ok, Worker} = work_dispatcher_gen_server:acquire(WorkManager, Priority),
	       io:format("Get worker ~p~n",[Worker]),
	       Worker ! {self(), make_ref(), fun() -> timer:sleep(1000), nothing end},
	       receive 
		   An ->
		       io:format("Received ~p~n", [An])
	       end,
	       work_dispatcher_gen_server:release(WorkManager, Worker)
       end
      ) || _ <- lists:seq(1,10)]. 
