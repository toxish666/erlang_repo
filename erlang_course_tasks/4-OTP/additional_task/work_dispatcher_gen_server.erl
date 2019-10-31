-module(work_dispatcher_gen_server).

-behaviour(gen_server).

%% client interface
-export([start_link/0, init_worker/0, add_work/3]).
%% gen_server API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%% Client API
start_link() ->
    gen_server:start_link(?MODULE, [], []).


add_work(GenServerPid, Work, Priority)  ->
    gen_server:call(GenServerPid, {new_work, Priority, Work, 500}).



%%% Server functions
init([]) -> 
    V1 = spawn_link(?MODULE, init_worker, []),
    V2 = spawn_link(?MODULE, init_worker, []),
    V3 = spawn_link(?MODULE, init_worker, []),
    {ok, {[{V1, free}, {V2, free}, {V3, free}], gb_sets:new()}}. %% no treatment of info here!


handle_call({new_work, Priority, Work}, From, State) ->
    handle_call({new_work, Priority, Work, 500}, From, State);

handle_call({new_work, Priority, Work, _Timeout}, From, State = {Workers, WorkingQueue}) 
  when Priority > 0,
       Priority < 11 ->
    NewWorkingQueue = gb_sets:add({Priority, Work, From, make_ref()}, WorkingQueue),
    HighestPriorityTask = sets:take_largest(NewWorkingQueue),
    FreeWorker = lists:search(fun(El) ->
				      case El of 
					  {_, free} ->
					      true;
					  _ ->
					      false
				      end
			      end, Workers),
    case FreeWorker of 
	false -> 
	    {noreply, State};
	{value, WF = {WorkerFounded, _}} ->
	    WorkerFounded ! {self(), add_new_task, HighestPriorityTask},
	    {noreply, {(Workers -- WF) ++ [{WorkerFounded, busy}], gb_sets:del_element(HighestPriorityTask,WorkingQueue)}}
    end;   
    %gen_server:call(self(), check_for_work), %% здесь тоже
    %{noreply, {Workers, gb_sets:add({Priority, Work, From, make_ref()}, WorkingQueue)}};

handle_call({work_done, WorkerPID, Result, Client}, _From, {Workers, WorkingQueue}) ->
    gen_server:reply(Client, {work_done, Result}),
    ExactWorker = lists:search(fun(El) ->
				       case El of
					   {WorkerPID, busy} ->
					       true;
					   _ ->
					       false
				       end
			      end, Workers),
    {noreply, {(Workers -- ExactWorker) ++ [{WorkerPID, free}] ,WorkingQueue}}.
	    
    









handle_cast(_Smth, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

terminate(normal, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}. 



%% free / busy worker
init_worker() ->
    loop_worker(free).

loop_worker(free) ->
    receive
	{Server, add_new_task, Task} ->
	    loop_worker(busy, {Server, Task})
    end.

loop_worker(busy, {Server, {_Priority, Work, Client, _Ref}}) ->
    TaskResult = Work(),
    Server ! {work_done, self(), TaskResult, Client},
    loop_worker(free).
