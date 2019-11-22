-module(lockman_serv).

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

-record(lock, {
	       key,
	       from,
	       queue
	      }).


-record(event, {
		key,
		waiting
	       }).

-record(state, {
		locks = [],
		events = []
	       }).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    {ok, #state{}}.



handle_call({acquire, Key}, From, #state{locks = Locks} = State) ->
    case lists:keyfind(Key, #lock.key, Locks) of
	false ->
	    Lock = #lock{key = Key,
			 from = From,
			 queue = queue:new()},
	    {reply, ok, State#state{locks = [Lock|Locks]}};
	#lock{key = Key, queue = Waiting} = Lock ->
	    UpdatedQueue = queue:in(From, Waiting),
	    NewLocks = lists:keyreplace(Key, #lock.key, Locks,
					Lock#lock{queue = UpdatedQueue}),
	    {noreply, State#state{locks = NewLocks}}
    end;
handle_call({wait, Key}, From, #state{events = Events} = State) ->
    case lists:keytake(Key, #event.key, Events) of
	false ->
	    Event = #event{key = Key,
			   waiting = [From]},
	    {noreply, State#state{events = [Event|Events]}};
	{value, #event{key = Key, waiting = Waiting} = Event, NewEvents} ->
	    NewEvent = Event#event{waiting = [From|Waiting]},
	    {noreply, State#state{events = [NewEvent|NewEvents]}}
    end;
handle_call(_Msg, _From, S) ->
    {noreply, S}.


handle_cast({accepted_sock, AcceptedSocket}, State) ->
    {ok, Pid} = lockman:start_link(self()),
    monitor(process, Pid),
    gen_tcp:controlling_process(AcceptedSocket, Pid),
    {noreply, State};
handle_cast({fire, Key}, #state{events = Events} = State) ->
    case lists:keytake(Key, #event.key, Events) of
	false ->
	    {noreply, State};
	{value, #event{key = Key, waiting = Pids}, NewEvents} ->
	    [gen_server:reply(Pid, ok) || Pid <- Pids],
	    {noreply, State#state{events = NewEvents}}
    end;
handle_cast({{release, FromPid}, Key}, #state{locks = Locks} = State) ->
    case lists:keytake(Key, #lock.key, Locks) of
	false ->
	    {noreply, State};
	{value, #lock{key = Key, from = {FromPid, _}} = Lock, NewLocks} ->
	    case get_alive_pid_from_queue(Lock#lock.queue) of
		{{value, Client}, NewQueue} ->
		    gen_server:reply(Client, ok),
		    NewLock = Lock#lock{
				from = Client,
				queue = NewQueue
			       },
		    {noreply, State#state{locks = [NewLock|NewLocks]}};
		{empty, _Q} ->
		    {noreply, State#state{locks = NewLocks}}
	    end;
	{value, _, _} ->
	    {noreply, State}
    end.


handle_info({'DOWN', Ref, process, FromPid, _Reason}, #state{locks = Locks} = State) ->
    demonitor(Ref),
    case lists:partition(fun(#lock{from = {Pid, _}}) ->
			    Pid == FromPid
		 end, Locks) of 
	 {[], _NewLocks} ->
	    {noreply, State};
	{[Lock], NewLocks} ->
	    case get_alive_pid_from_queue(Lock#lock.queue) of
		{{value, Client}, NewQueue} ->
		    gen_server:reply(Client, ok),
		    NewLock = Lock#lock{
				from = Client,
				queue = NewQueue
			       },
		    {noreply, State#state{locks = [NewLock|NewLocks]}};
		{empty, _Q} ->
		    {noreply, State#state{locks = NewLocks}}
	    end
    end;
    handle_info(_Event, State) ->
    {noreply, State}.  


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 

terminate(_Reason, _State) ->
    ok.


%%----------------------------------------------------------------------------
%% HANDLERS
%%----------------------------------------------------------------------------
get_alive_pid_from_queue(Q) ->
    case queue:out(Q) of
	{{value, _Client = {Pid, _Ref}}, NewQueue} = Correct ->
	    case is_process_alive(Pid) of
		true ->
		    Correct;
		fales ->
		    get_alive_pid_from_queue(NewQueue)
	    end;
	{empty, _Q} = End ->
	    End
    end.
