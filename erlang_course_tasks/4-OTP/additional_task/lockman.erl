-module(lockman).
-behaviour(gen_server).

%%----------------------------------------------------------------------------
%% BEHAVIOUR EXPORTS
%%----------------------------------------------------------------------------
-export([
	 init/1,
	 terminate/2,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2
	]).

%%----------------------------------------------------------------------------
%% PUBLIC API EXPORTS
%%----------------------------------------------------------------------------
-export([
	 wait/2,
	 fire/2,
	 acquire/2,
	 release/2,
	 start_link/0,
	 unlin/1
	]).

%%----------------------------------------------------------------------------
%% TEST API EXPORTS
%%----------------------------------------------------------------------------
-export([
	 test_acquire_release/0,
	 test_wait_fire/0,
	 test_acquire_release_with_failure/0
	]).

%%----------------------------------------------------------------------------
%% STRUCTURES
%%----------------------------------------------------------------------------
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

%%----------------------------------------------------------------------------
%% PUBLIC API
%%----------------------------------------------------------------------------
wait(P, Atom) ->
    gen_server:call(P, {wait, Atom}, infinity).

fire(P, Atom) ->
    gen_server:cast(P, {fire, Atom}).

acquire(P, Atom) ->
    gen_server:call(P, {acquire, Atom}, infinity).

release(P, Atom) ->
    gen_server:cast(P, {release, Atom, self()}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

unlin(P) ->
    gen_server:call(P, unlinkmanually, infinity).

%%----------------------------------------------------------------------------
%% LIFECYCLE
%%----------------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

terminate(_Reason, _State) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%----------------------------------------------------------------------------
%% HANDLERS
%%----------------------------------------------------------------------------
handle_call(unlinkmanually, From, State) ->
    unlink(element(1,From)),
    {reply, unlinked, State}; 
handle_call({acquire, Key}, From, #state{locks = Locks} = State) ->
    link(element(1,From)),
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
handle_call(_, _From, State) ->
    {reply, {error, wrong_request}, State}.


handle_cast({fire, Key}, #state{events = Events} = State) ->
    case lists:keytake(Key, #event.key, Events) of
	false ->
	    {noreply, State};
	{value, #event{key = Key, waiting = Pids}, NewEvents} ->
	    [gen_server:reply(Pid, ok) || Pid <- Pids],
	    {noreply, State#state{events = NewEvents}}
    end;
handle_cast({release, Key, Pid}, #state{locks = Locks} = State) ->
    unlink(Pid),
    case lists:keytake(Key, #lock.key, Locks) of
	false ->
	    {noreply, State};
	{value, #lock{key = Key, from = {Pid, _}} = Lock, NewLocks} ->
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
	{value, {Key, _, _}} ->
	    {noreply, State}
    end;
handle_cast(_, State) ->
    {noreply, State}.


%% special treatment when process that grabbed acquire-lock dies with abnormal reason
handle_info({'EXIT', FromPid, Reason}, #state{locks = Locks} = State) when Reason =/= normal ->
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

handle_info(_, State) ->
    {noreply, State}.


%%----------------------------------------------------------------------------
%% HELPER FUNCTIONS
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



%%----------------------------------------------------------------------------
%% TESTS
%%----------------------------------------------------------------------------

test_acquire_release() ->
    {ok, LockManager} = lockman:start_link(),
   [spawn(
      fun() ->
	       timer:sleep(rand:uniform(1000)),
	       ok = lockman:acquire(LockManager, lock),
	       io:format("Do! ~p~n", [self()]),
	       timer:sleep(rand:uniform(10) * 1000),
	       lockman:release(LockManager, lock)
      end
     ) || _ <- lists:seq(1, 10)].


test_acquire_release_with_failure() ->
    {ok, LockManager} = lockman:start_link(),
   [spawn(
      fun() ->
	       timer:sleep(rand:uniform(1000)),
	       ok = lockman:acquire(LockManager, lock),
	       io:format("Do! ~p~n", [self()]),
	       timer:sleep(rand:uniform(10) * 1000),
	       exit(abnormal)
      end
     ) || _ <- lists:seq(1, 10)].


test_wait_fire() ->
    {ok, LockManager} = lockman:start_link(),
    [spawn(
       fun() ->
	       ok = lockman:wait(LockManager, a),
	       io:format("Do! ~p~n", [I])
       end
      ) || I <- lists:seq(1, 10)],
    timer:sleep(5000),
    lockman:fire(LockManager, a).
